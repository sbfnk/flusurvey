reports <- function(id, dates, symptomatic, same, start, end) {
  data.table::data.table(
    participant_id = id, global_id = paste0("p", id), season = "2012/13",
    symptom.id = seq_along(dates), date = as.Date(dates),
    no.symptoms = factor(ifelse(symptomatic, "f", "t"), levels = c("f", "t")),
    same = factor(same, levels = c("yes", "no", "dont_know")),
    symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end)
  )
}

a <- reports(
  1, c("2013-01-03", "2013-01-10"), c(FALSE, TRUE), c(NA, "no"),
  c(NA, "2013-01-05"), c(NA, "2013-01-08")
)
b <- reports(
  2, c("2013-01-17", "2013-01-24", "2013-01-31"), c(TRUE, TRUE, FALSE),
  c("yes", "yes", NA), c("2013-01-15", "2013-01-10", NA), c(NA, NA, NA)
)

test_that("the previous end date does not carry over between participants", {
  capture.output(
    alone <- bouts_of_illness(b),
    together <- bouts_of_illness(rbind(a, b))
  )
  expect_identical(nrow(alone), 1L)
  expect_identical(together[participant_id == 2, .N], 1L)
})

flag_reports <- function() {
  r <- reports(
    1, c("2013-01-03", "2013-01-06", "2013-01-12"), c(TRUE, TRUE, TRUE),
    c(NA, "yes", "yes"), c("2013-01-02", NA, NA),
    rep("2013-01-12", 3)
  )
  r[, ili := factor(c("f", "t", "f"), levels = c("f", "t"))]
  r
}
ili_by_day <- function(mode) {
  capture.output(res <- bouts_of_illness(flag_reports(), daily.flags = mode))
  as.character(res$ili)
}

test_that("daily.flags gives one row per day of a bout", {
  capture.output(res <- bouts_of_illness(flag_reports(), daily.flags = "bout"))
  expect_identical(nrow(res), 11L)
  expect_identical(range(res$day), as.Date(c("2013-01-02", "2013-01-12")))
})

test_that("daily.flags = 'none' keeps one row per bout", {
  capture.output(res <- bouts_of_illness(flag_reports()))
  expect_identical(nrow(res), 1L)
  expect_false("day" %in% colnames(res))
})

test_that("daily.flags = 'carry.forward' uses the last earlier report", {
  expect_identical(
    ili_by_day("carry.forward"),
    c(NA, "f", "f", "f", "t", "t", "t", "t", "t", "t", "f")
  )
})

test_that("daily.flags = 'bout' uses the merged flag on every day", {
  expect_identical(ili_by_day("bout"), rep("t", 11))
})

test_that("daily.flags = 'report' is missing on days without a report", {
  expect_identical(
    ili_by_day("report"),
    c(NA, "f", NA, NA, "t", NA, NA, NA, NA, NA, "f")
  )
})

test_that("daily.flags covers bouts whose reports have no start or end date", {
  ## the dates of such a bout come from the dates of its reports
  none <- reports(
    3, c("2013-01-17", "2013-01-24"), c(TRUE, TRUE), c("yes", "yes"),
    c(NA, NA), c(NA, NA)
  )
  for (mode in c("carry.forward", "bout", "report")) {
    capture.output(res <- bouts_of_illness(none, daily.flags = mode))
    expect_equal(
      res$day, seq(as.Date("2013-01-17"), as.Date("2013-01-24"), by = "day")
    )
  }
  capture.output(res <- bouts_of_illness(b, daily.flags = "bout"))
  expect_false(anyNA(res$day))
})

test_that("input is sorted by participant, season and date", {
  capture.output(
    ordered <- bouts_of_illness(rbind(a, b)),
    shuffled <- bouts_of_illness(rbind(b[3:1], a[2:1]))
  )
  expect_equal(shuffled, ordered)
})
