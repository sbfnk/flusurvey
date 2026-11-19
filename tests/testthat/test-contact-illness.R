test_that("contact_illness takes status from the same-day symptom report", {
  contacts <- data.table::data.table(
    global_id = c("a", "a", "b", "c"),
    date = as.Date("2013-01-01") + c(0, 1, 0, 0)
  )
  symptoms <- data.table::data.table(
    global_id = c("a", "a", "b"),
    date = as.Date("2013-01-01") + c(0, 5, 0),
    no.symptoms = factor(c("f", "t", "t"), levels = c("f", "t")),
    ili.self = factor(c("t", "f", "f"), levels = c("f", "t")),
    resp.symptoms = factor(c("t", "f", "f"), levels = c("f", "t"))
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(nrow(res), 4)
  data.table::setkey(res, global_id, date)
  expect_equal(res$current_symptoms, c("symptoms", NA, "no symptoms", NA))
  expect_equal(res$current_ili, c("ILI", NA, "no ILI", NA))
  expect_equal(res$current_ari, c("ARI", NA, "no ARI", NA))
})

test_that("contact_illness uses symptom reports that cleaning removes", {
  contacts <- extract_data(
    simulated_data, years = 2013:2014, surveys = c("background", "contact")
  )
  symptoms <- extract_data(
    simulated_data, years = 2013:2014, surveys = "symptom",
    clean = "unsuccessful.join"
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
