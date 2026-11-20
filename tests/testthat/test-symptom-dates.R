## prepare a season in which one later report has a given start date
season_with_start <- function(offset, from = c("first", "report"),
                              gap = 0) {
  from <- match.arg(from)
  season <- simulated_data[["2013"]]
  symptom <- data.table::copy(data.table::data.table(season$symptom))
  symptom <- symptom[!duplicated(symptom[, list(global_id, date)],
                                 fromLast = TRUE)]
  symptom[, first := min(date), by = global_id]
  target <- symptom[date > first & date - first >= gap][1]
  rows <- symptom$global_id == target$global_id & symptom$date == target$date
  anchor <- if (from == "first") target$first else target$date
  symptom[rows, symptoms.start.date := anchor + offset]
  symptom[, first := NULL]
  season$symptom <- symptom
  list(season = season, id = target$global_id, date = target$date)
}

start_after_cleaning <- function(x, ...) {
  cleaned <- expect_contact_warning(merge_data(
    x$season, clean = "remove.bad.symptom.dates", ...
  ))
  cleaned[global_id == x$id & date == x$date]$symptoms.start.date
}

test_that("start dates before the first report are removed", {
  x <- season_with_start(-1, "first")
  expect_true(is.na(start_after_cleaning(x, max.symptom.days = Inf)))
})

test_that("start dates long before the report are removed", {
  x <- season_with_start(-60, "report", gap = 90)
  expect_true(is.na(start_after_cleaning(x, max.symptom.days = 28)))
  expect_false(is.na(start_after_cleaning(x, max.symptom.days = Inf)))
})

test_that("plausible start dates are kept", {
  x <- season_with_start(-3, "report")
  expect_false(is.na(start_after_cleaning(x, max.symptom.days = 28)))
})
