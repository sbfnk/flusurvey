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

test_that("age groups follow the contact age bands", {
  ages <- c(3, 18, 19, 44, 45, 64, 65, 90)
  background <- data.table::data.table(simulated_data[["2013"]]$background)
  background <- background[!duplicated(global_id)][seq_along(ages)]
  background[, date := as.Date("2013-12-01")]
  background[, birthmonth := format(date - 365.25 * ages - 10, "%Y-%m")]
  dt <- merge_data(list(background = background), clean = "n.reports")
  dt <- dt[order(age)]
  expect_equal(dt$age, ages)
  expect_equal(
    as.character(dt$agegroup),
    c("0-18", "0-18", "19-44", "19-44", "45-64", "45-64", "65+", "65+")
  )
})
