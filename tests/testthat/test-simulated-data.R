test_that("simulated data has the questionnaires of both seasons", {
  expect_named(simulated_data, c("2013", "2014"))
  for (season in simulated_data) {
    expect_named(season, c("background", "contact", "symptom"))
    for (survey in season) expect_gt(nrow(survey), 0)
  }
})

test_that("extract_data returns a non-empty table with default cleaning", {
  dt <- extract_data(simulated_data, years = 2013:2014)
  expect_s3_class(dt, "data.table")
  expect_gt(nrow(dt), 0)
  expect_setequal(unique(dt$season), c("2012/13", "2013/14"))
  expect_true(all(c("ili", "agegroup", "region", "participant_id") %in%
                  colnames(dt)))
  expect_true(any(dt$ili == "t", na.rm = TRUE))
})

test_that("get_incidence returns incidence for both seasons", {
  dt <- extract_data(simulated_data, years = 2013:2014)
  incidence <- get_incidence(dt, incidence.columns = "ili",
                             aggregation = "week")
  expect_gt(nrow(incidence), 0)
  expect_setequal(unique(incidence$season), c("2012/13", "2013/14"))
  expect_gt(sum(incidence$ili), 0)
  by_age <- get_incidence(dt, by = "agegroup")
  expect_gt(nrow(by_age), 0)
})

test_that("bouts_of_illness returns bouts of illness", {
  dt <- extract_data(simulated_data, years = 2013:2014)
  invisible(capture.output(res <- bouts_of_illness(dt)))
  expect_gt(nrow(res), 0)
  expect_false(anyNA(res$symptoms.start.date))
})

test_that("health scores outside 0 to 100 are removed by default", {
  raw <- unlist(lapply(simulated_data, function(season) {
    season$symptom$health.score
  }))
  expect_true(any(raw < 0 | raw > 100, na.rm = TRUE))
  dt <- extract_data(simulated_data, years = 2013:2014)
  expect_true(all(dt$health.score >= 0 & dt$health.score <= 100, na.rm = TRUE))
})

test_that("remove.bad.health.scores is accepted by name", {
  dt <- extract_data(simulated_data, years = 2013:2014,
                     clean = "remove.bad.health.scores")
  expect_true(all(dt$health.score >= 0 & dt$health.score <= 100, na.rm = TRUE))
  dt <- extract_data(simulated_data, years = 2013:2014,
                     clean = "remove.first")
  expect_true(any(dt$health.score < 0 | dt$health.score > 100, na.rm = TRUE))
})

test_that("merge_data dates contact reports by the day they describe", {
  dt <- merge_data(simulated_data[["2013"]])
  expect_true(all(c("contact.date", "contact.day.of.week") %in% colnames(dt)))
  has_contact <- !is.na(dt$contact.date)
  expect_true(any(has_contact))
  ## the survey asks about the previous day: the contact day precedes the
  ## date of the (possibly rolled-forward) report
  expect_true(all(dt$contact.date[has_contact] < dt$date[has_contact]))
  ## Monday is 1 and Sunday 7, as for day.of.week
  expect_equal(
    dt$contact.day.of.week[has_contact],
    as.integer(format(dt$contact.date[has_contact], "%u"))
  )
  ## a contact report filed on Monday describes a Sunday
  monday <- merge_data(
    simulated_data[["2013"]], clean = NULL
  )[day.of.week == 1 & !is.na(contact.date) & contact.date == date - 1]
  expect_gt(nrow(monday), 0)
  expect_true(all(monday$contact.day.of.week == 7))
})
