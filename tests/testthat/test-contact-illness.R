report <- function(id, date, ill = TRUE, start = NA, end = NA,
                   option = "date", ili = "f", ili.fever = "f",
                   ili.self = "f", ari = "f") {
  data.table::data.table(
    global_id = id, date = as.Date(date),
    no.symptoms = if (ill) "f" else "t",
    symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end),
    symptoms.end.option = option, ili = ili, ili.fever = ili.fever,
    ili.self = ili.self, resp.symptoms = ari
  )
}

contact <- function(id, day) {
  data.table::data.table(global_id = id, contact.date = as.Date(day))
}

test_that("contact illness keeps the four case definitions separate", {
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "t", ili.fever = "f", ili.self = "f", ari = "f"),
    report("b", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "t", ili.fever = "t", ili.self = "f", ari = "t"),
    report("c", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "f", ili.fever = "f", ili.self = "t", ari = "t"),
    report("d", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "f", ili.fever = "f", ili.self = "f", ari = "t")
  ))
  res <- contact_illness(contact(letters[1:4], "2013-01-07"), symptoms)
  expect_equal(res$current_symptoms, rep("symptoms", 4))
  expect_equal(res$current_ili, c("ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$current_ili_fever, c("no ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$current_ili_self, c("no ILI", "no ILI", "ILI", "no ILI"))
  expect_equal(res$current_ari, c("no ARI", "ARI", "ARI", "ARI"))
})

test_that("a contact day between symptom reports lies in the bout", {
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-03", start = "2013-01-02", option = "still_ill",
           ili = "t", ili.fever = "t", ari = "t"),
    report("a", "2013-01-08", start = "2013-01-02", end = "2013-01-07",
           ili = "t", ili.fever = "f", ari = "t")
  ))
  res <- contact_illness(contact("a", c("2013-01-05", "2013-01-03")), symptoms)
  expect_equal(res$current_symptoms, c("symptoms", "symptoms"))
  ## no report is dated on 5 January: the bout description applies, the daily
  ## flags are not assigned
  expect_equal(res$bout_ili, c("ILI", "ILI"))
  expect_equal(res$bout_ili_fever, c("ILI", "ILI"))
  expect_true(is.na(res$current_ili[res$contact.date == as.Date("2013-01-05")]))
  expect_equal(res$current_ili[res$contact.date == as.Date("2013-01-03")],
               "ILI")
  expect_true(
    is.na(res$current_ili_fever[res$contact.date == as.Date("2013-01-05")])
  )
})

test_that("bout flags combine all reports of the bout", {
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-03", start = "2013-01-02", option = "still_ill",
           ili = "t", ili.fever = "f"),
    report("a", "2013-01-05", start = "2013-01-02", end = "2013-01-04",
           ili = "f", ili.fever = "f")
  ))
  res <- contact_illness(contact("a", "2013-01-04"), symptoms)
  expect_equal(res$bout_ili, "ILI")
  expect_equal(res$bout_ili_fever, "no ILI")
})

test_that("a participant who was not observed has an unknown status", {
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-03", start = "2013-01-02", end = "2013-01-03"),
    report("b", "2013-01-10", ill = FALSE)
  ))
  res <- contact_illness(
    contact(c("a", "a", "b", "b", "c"),
            c("2013-01-03", "2013-01-20", "2013-01-10", "2013-01-20",
              "2013-01-03")),
    symptoms
  )
  data.table::setkey(res, global_id, contact.date)
  expect_equal(res$current_symptoms,
               c("symptoms", NA, "no symptoms", NA, NA))
  expect_true(all(is.na(res$current_ili[c(2, 4, 5)])))
  expect_true(all(is.na(res$bout_ili_fever[c(2, 4, 5)])))
  expect_equal(res$current_ili[3], "no ILI")
})

test_that("reports without a usable interval give no bout", {
  day <- as.Date("2013-01-07")
  symptoms <- data.table::rbindlist(list(
    report("a", day, start = day - 5, end = day - 4),
    report("b", day, start = day - 5, option = "dont_know"),
    report("c", day, start = NA, end = day),
    report("d", day, start = day + 1, end = day + 2),
    report("e", day, start = day - 5, option = "still_ill")
  ))
  res <- contact_illness(contact(letters[1:5], day), symptoms)
  expect_equal(res$current_symptoms,
               c("no symptoms", NA, NA, NA, "symptoms"))
})

test_that("bouts from separate reports merge when they overlap or touch", {
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-03", start = "2013-01-01", end = "2013-01-03"),
    report("a", "2013-01-08", start = "2013-01-04", end = "2013-01-06"),
    report("a", "2013-01-20", start = "2013-01-15", end = "2013-01-16")
  ))
  res <- contact_illness(
    contact("a", c("2013-01-02", "2013-01-05", "2013-01-10", "2013-01-16")),
    symptoms
  )
  expect_equal(res$current_symptoms, c("symptoms", "symptoms", NA, "symptoms"))
})

test_that("same-day reports combine independently of row order", {
  day <- as.Date("2013-01-07")
  symptoms <- data.table::rbindlist(list(
    report("a", day, start = day, option = "still_ill", ili = "t", ari = "t"),
    report("a", day, start = day, option = "still_ill", ili = "f", ari = "f"),
    report("b", day, start = day, option = "still_ill", ili = NA),
    report("b", day, start = day, option = "still_ill", ili = "f"),
    report("c", day, start = day, option = "still_ill")
  ))
  forward <- contact_illness(contact(letters[1:3], day), symptoms)
  backward <- contact_illness(contact(letters[1:3], day),
                              symptoms[nrow(symptoms):1])
  expect_equal(forward, backward)
  expect_equal(forward$current_ili, c("ILI", NA, "no ILI"))
  expect_equal(forward$current_ari, c("ARI", "no ARI", "no ARI"))
})

test_that("contact_illness does not match missing participant IDs or dates", {
  contacts <- data.table::data.table(
    global_id = c("a", NA, NA, "a"),
    contact.date = as.Date(c(NA, "2013-01-01", NA, "2013-01-01"))
  )
  symptoms <- report(c("a", NA, NA, "a"),
                     c(NA, "2013-01-01", NA, "2013-01-01"),
                     start = "2013-01-01", option = "still_ill",
                     ili = "t", ari = "t")
  res <- contact_illness(contacts, symptoms)
  matched <- !is.na(res$global_id) & !is.na(res$contact.date)
  expect_equal(nrow(res), nrow(contacts))
  expect_true(all(is.na(res$current_symptoms[!matched])))
  expect_equal(res$current_symptoms[matched], "symptoms")
  expect_equal(res$current_ili[matched], "ILI")
})

test_that("extraction preserves same-day symptom reports for contact illness", {
  day <- as.Date("2013-01-07")
  reports <- data.table::data.table(
    global_id = "a", date = day, no.symptoms = c("f", "t"),
    ili.self = c("t", "f"),
    symptoms.start.date = day, symptoms.end.date = as.Date(NA),
    symptoms.end.option = "still_ill", fever = c("t", "f"),
    cough = c("t", "f")
  )
  symptoms <- extract_data(
    list(`2013` = list(symptom = reports)), surveys = "symptom",
    clean = c("unsuccessful.join", "remove.bad.symptom.dates")
  )
  expect_equal(nrow(symptoms), 2)
  res <- contact_illness(contact("a", day), symptoms)
  expect_equal(res$current_symptoms, "symptoms")
  expect_equal(res$current_ili_self, "ILI")
  expect_equal(res$current_ari, "ARI")
})

test_that("contact_illness uses symptom reports that cleaning removes", {
  contacts <- extract_data(
    simulated_data, years = 2013:2014, surveys = c("background", "contact")
  )
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
  clean <- c("unsuccessful.join", "remove.bad.symptom.dates")
  analysis_path <- test_path("..", "..", "analysis", "contacts.R")
  if (file.exists(analysis_path)) {
    expressions <- parse(analysis_path)
    assignment <- expressions[[which(vapply(expressions, function(x) {
      is.call(x) && identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], as.name("dt_symptoms"))
    }, logical(1)))]]
    expect_equal(eval(assignment[[3]][["clean"]]), clean)
  }
  day <- as.Date("2013-01-07")
  reports <- report("a", day, start = day - 1, end = day + 2, ili = "t")
  symptoms <- extract_data(list(`2013` = list(symptom = reports)),
                           surveys = "symptom", clean = clean)
  expect_equal(nrow(symptoms), 1)
  res <- contact_illness(contact("a", day), symptoms)
  ## cleaning clears the invalid end date, leaving no usable interval, so the
  ## contact day has an unknown status
  expect_true(is.na(res$current_symptoms))
})
