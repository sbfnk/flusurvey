test_that("contact_illness takes status from the symptom report of the contact day", {
  contacts <- data.table::data.table(
    global_id = c("a", "a", "b", "c"),
    date = as.Date("2013-01-02") + c(0, 1, 0, 0)
  )
  contacts[, contact.date := date - 1]
  symptoms <- data.table::data.table(
    global_id = c("a", "a", "b"),
    date = as.Date("2013-01-01") + c(0, 5, 0),
    symptoms.start.date = as.Date("2013-01-01"),
    symptoms.end.date = as.Date(NA), symptoms.end.option = "still_ill",
    no.symptoms = factor(c("f", "t", "t"), levels = c("f", "t")),
    ili.self = factor(c("t", "f", "f"), levels = c("f", "t")),
    resp.symptoms = factor(c("t", "f", "f"), levels = c("f", "t"))
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(nrow(res), 4)
  data.table::setkey(res, global_id, contact.date)
  expect_equal(res$current_symptoms, c("symptoms", NA, "no symptoms", NA))
  expect_equal(res$current_ili, c("ILI", NA, "no ILI", NA))
  expect_equal(res$current_ari, c("ARI", NA, "no ARI", NA))
})

test_that("contact_illness does not match missing participant IDs or dates", {
  contacts <- data.table::data.table(
    global_id = c("a", NA, NA, "a"),
    contact.date = as.Date(c(NA, "2013-01-01", NA, "2013-01-01"))
  )
  symptoms <- data.table::data.table(
    global_id = c("a", NA, NA, "a"),
    date = as.Date(c(NA, "2013-01-01", NA, "2013-01-01")),
    no.symptoms = "f", ili.self = "t", resp.symptoms = "t",
    symptoms.start.date = as.Date("2013-01-01"),
    symptoms.end.date = as.Date(NA), symptoms.end.option = "still_ill"
  )
  res <- contact_illness(contacts, symptoms)
  matched <- !is.na(res$global_id) & !is.na(res$contact.date)
  expect_equal(nrow(res), nrow(contacts))
  expect_true(all(is.na(res$current_symptoms[!matched])))
  expect_true(all(is.na(res$current_ili[!matched])))
  expect_true(all(is.na(res$current_ari[!matched])))
  expect_equal(res$current_symptoms[matched], "symptoms")
  expect_equal(res$current_ili[matched], "ILI")
  expect_equal(res$current_ari[matched], "ARI")
})

test_that("contact illness uses illness dates and distinguishes unknown dates", {
  day <- as.Date("2013-01-07")
  contacts <- data.table::data.table(global_id = letters[1:7], contact.date = day)
  symptoms <- data.table::data.table(
    global_id = letters[1:7], date = day, no.symptoms = "f",
    ili.self = "t", resp.symptoms = "t",
    symptoms.start.date = day + c(-5, 0, -5, -5, NA, -5, 1),
    symptoms.end.date = day + c(-4, 0, NA, NA, 0, NA, 2),
    symptoms.end.option = c("date", "date", "still_ill", "dont_know",
                            "date", "date", "date")
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$current_symptoms, c("no symptoms", "symptoms", "symptoms",
                                       NA, NA, NA, "no symptoms"))
  expect_equal(res$current_ili, c("no ILI", "ILI", "ILI", NA, NA, NA, "no ILI"))
  expect_equal(res$current_ari, c("no ARI", "ARI", "ARI", NA, NA, NA, "no ARI"))
})

test_that("contact_illness uses symptom reports that cleaning removes", {
  contacts <- extract_data(
    simulated_data, years = 2013:2014, surveys = c("background", "contact")
  )
  ## simulated symptom reports fall on the contact report dates, so use
  ## those dates for the lookup
  contacts[, contact.date := date]
  symptoms <- extract_data(
    simulated_data, years = 2013:2014, surveys = "symptom",
    clean = c("unsuccessful.join", "remove.bad.symptom.dates")
  )
  cleaned <- extract_data(
    simulated_data, years = 2013:2014, surveys = "symptom"
  )
  expect_lt(nrow(cleaned), nrow(symptoms))
  res <- contact_illness(contacts, symptoms)
  expect_equal(nrow(res), nrow(contacts))
  res_cleaned <- contact_illness(contacts, cleaned)
  expect_gt(
    sum(!is.na(res$current_symptoms)),
    sum(!is.na(res_cleaned$current_symptoms))
  )
})

test_that("contact analysis cleans invalid dates without dropping reports", {
  expressions <- parse(test_path("..", "..", "analysis", "contacts.R"))
  assignment <- expressions[[which(vapply(expressions, function(x) {
    is.call(x) && identical(x[[1]], as.name("<-")) &&
      identical(x[[2]], as.name("dt_symptoms"))
  }, logical(1)))]]
  clean <- eval(assignment[[3]][["clean"]])
  day <- as.Date("2013-01-07")
  reports <- data.table::data.table(
    global_id = "a", date = day, no.symptoms = "f", ili.self = "t",
    resp.symptoms = "t", symptoms.start.date = day - 1,
    symptoms.end.date = day + 2, symptoms.end.option = "date"
  )
  symptoms <- extract_data(list(`2013` = list(symptom = reports)),
                           surveys = "symptom", clean = clean)
  expect_equal(nrow(symptoms), 1)
  res <- contact_illness(data.table::data.table(global_id = "a",
                                                contact.date = day), symptoms)
  expect_true(is.na(res$current_symptoms))
  expect_true(is.na(res$current_ili))
  expect_true(is.na(res$current_ari))
})
