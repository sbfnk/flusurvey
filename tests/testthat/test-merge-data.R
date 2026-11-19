test_that("merge_data turns t/f columns from every survey into factors", {
  data <- lapply(simulated_data[["2013"]], data.table::copy)
  ## columns found only in the background survey, which is not the last
  ## survey processed
  background_only <- setdiff(
    colnames(data[["background"]]),
    colnames(data[[length(data)]])
  )
  tf_columns <- background_only[vapply(background_only, function(x) {
    all(c("t", "f") %in% data[["background"]][[x]])
  }, logical(1))]
  expect_gt(length(tf_columns), 0)
  for (column in tf_columns) {
    data[["background"]][, (column) := as.character(get(column))]
  }

  dt <- merge_data(data, clean = NULL)
  for (column in tf_columns) {
    expect_true(is.factor(dt[[column]]), info = column)
  }
})

test_that("merge_data skips every cleaning step for clean = NULL", {
  raw <- merge_data(simulated_data[["2013"]], clean = NULL)
  first_only <- merge_data(simulated_data[["2013"]], clean = "remove.first")
  expect_gt(nrow(raw), nrow(first_only))
  expect_identical(
    nrow(merge_data(simulated_data[["2013"]], clean = character(0))),
    nrow(raw)
  )
})

test_that("the driving survey retains same-day reports through joins", {
  data <- lapply(simulated_data[["2013"]], data.table::copy)
  for (name in c("background", "contact", "symptom")) {
    data[[name]] <- data.table::rbindlist(list(data[[name]][1], data[[name]][1]))
  }
  for (surveys in list("background", c("background", "contact"),
                       c("background", "contact", "symptom"))) {
    if (length(surveys) == 3) {
      expect_warning(result <- merge_data(data[surveys], clean = NULL))
    } else {
      result <- merge_data(data[surveys], clean = NULL)
    }
    expect_equal(nrow(result), 2)
  }
})

test_that("season selection handles tied report dates", {
  report <- data.table::copy(simulated_data[["2013"]][["contact"]][1])
  reports <- data.table::rbindlist(rep(list(report), 6))
  reports[, global_id := c("a", "a", "b", "c", "d", "e")]
  reports[, date := as.Date(c(
    "2014-01-01", "2014-01-01", "2014-01-02", "2014-01-02",
    "2013-11-01", "2014-04-02"
  ))]
  result <- merge_data(list(contact = reports), clean = "limit.season")
  expect_equal(result$date, as.Date(c(
    "2013-11-01", "2014-01-01", "2014-01-01", "2014-01-02", "2014-01-02"
  )))

  across_seasons <- data.table::data.table(date = as.Date(c(
    "2015-01-01", "2015-01-01", "2014-01-01", "2014-01-01"
  )))
  expect_equal(
    flusurvey:::limit_to_season(across_seasons)$date,
    as.Date(c("2014-01-01", "2014-01-01"))
  )

  for (date in as.Date(c("2014-01-15", "2014-04-01", "2014-11-01",
                         "2014-12-15"))) {
    date <- as.Date(date, origin = "1970-01-01")
    dates <- as.Date(c("2013-11-01", "2014-04-01", "2014-11-01",
                       "2015-04-01"))
    tied <- data.table::data.table(date = c(date + 1, date, dates))
    tied <- data.table::rbindlist(list(tied, tied[1:2]))
    start <- if (date < as.Date("2014-11-01")) dates[1] else dates[3]
    end <- if (date < as.Date("2014-11-01")) dates[2] else dates[4]
    expect_equal(
      flusurvey:::limit_to_season(tied),
      tied[date >= start & date <= end]
    )
  }
})

test_that("minimum symptom reports counts reporting days", {
  report <- data.table::copy(simulated_data[["2013"]][["symptom"]][1])
  reports <- data.table::rbindlist(rep(list(report), 6))
  reports[, global_id := rep(c("a", "b"), each = 3)]
  reports[global_id == "b", date := date + 0:2]

  result <- merge_data(list(symptom = reports), clean = "n.reports")
  expect_equal(as.character(result$global_id), rep("b", 3))
  expect_equal(result$nReports, rep(3L, 3))

  unfiltered <- merge_data(list(symptom = reports), clean = NULL)
  expect_equal(unfiltered[global_id == "a", nReports], rep(1L, 3))
})

test_that("remove.first removes all reports on each first reporting day", {
  report <- data.table::copy(simulated_data[["2013"]][["symptom"]][1])
  reports <- data.table::rbindlist(rep(list(report), 5))
  reports[, global_id := c("a", "a", "a", "b", "b")]
  reports[, date := date + c(7, 0, 0, 0, 0)]

  result <- merge_data(list(symptom = reports), clean = "remove.first")
  expect_equal(as.character(result$global_id), "a")
  expect_equal(result$date, reports$date[1])
})
