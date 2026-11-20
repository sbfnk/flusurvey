test_that("flag_ill_contacts flags contact days within an episode", {
  d <- as.Date("2013-01-10")
  contacts <- data.table::data.table(
    global_id = c("a", "a", "a", "a", "b", "c"),
    contact.date = d + c(-1, 0, 3, 4, 1, NA)
  )
  bouts <- data.table::data.table(
    global_id = c("a", "a", "b", "c"),
    symptoms.start.date = c(d, d + 10, as.Date(NA), d),
    symptoms.end.date = c(d + 3, d + 12, d + 5, d + 2)
  )
  res <- flag_ill_contacts(contacts, bouts)
  ## bounds are inclusive; episodes of other participants or with a missing
  ## date, and missing contact dates, are not matched
  expect_identical(res$ill, c(FALSE, TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_identical(res$global_id, contacts$global_id)
  expect_false("flag.row" %in% colnames(res))
  expect_false("ill" %in% colnames(contacts))
})

test_that("flag_ill_contacts does not duplicate contacts in several episodes", {
  d <- as.Date("2013-01-10")
  contacts <- data.table::data.table(global_id = "a", contact.date = d)
  bouts <- data.table::data.table(
    global_id = c("a", "a"),
    symptoms.start.date = c(d - 1, d - 2),
    symptoms.end.date = c(d + 1, d + 2)
  )
  expect_identical(flag_ill_contacts(contacts, bouts)$ill, TRUE)
})

test_that("flag_ill_contacts needs the columns it matches on", {
  contacts <- data.table::data.table(global_id = "a")
  bouts <- data.table::data.table(global_id = "a")
  expect_error(flag_ill_contacts(contacts, bouts), "contact.date")
  contacts[, contact.date := Sys.Date()]
  expect_error(flag_ill_contacts(contacts, bouts), "symptoms.start.date")
})

test_that("flag_ill_contacts works on the simulated data", {
  symptoms <- extract_data(
    simulated_data, years = 2013, surveys = c("background", "symptom")
  )
  contacts <- extract_data(
    simulated_data, years = 2013, surveys = c("background", "contact")
  )
  bouts <- suppressMessages(utils::capture.output(
    b <- bouts_of_illness(symptoms)
  ))
  res <- flag_ill_contacts(contacts, b)
  expect_identical(nrow(res), nrow(contacts))
  ## independent check: a loop over the contact reports
  expected <- vapply(seq_len(nrow(contacts)), function(i) {
    e <- b[global_id == contacts$global_id[i]]
    any(e$symptoms.start.date <= contacts$contact.date[i] &
      e$symptoms.end.date >= contacts$contact.date[i], na.rm = TRUE)
  }, logical(1))
  expect_identical(res$ill, expected)
})

test_that("merge_data warns when joining contact and symptom surveys", {
  expect_warning(
    merge_data(simulated_data[["2013"]]), "flag_ill_contacts"
  )
  expect_no_warning(
    merge_data(simulated_data[["2013"]][c("background", "symptom")])
  )
  expect_no_warning(
    merge_data(simulated_data[["2013"]][c("background", "contact")])
  )
})
