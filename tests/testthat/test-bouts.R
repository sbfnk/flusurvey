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

test_that("input is sorted by participant, season and date", {
  capture.output(
    ordered <- bouts_of_illness(rbind(a, b)),
    shuffled <- bouts_of_illness(rbind(b[3:1], a[2:1]))
  )
  expect_equal(shuffled, ordered)
})

test_that("an unknown onset is left missing, not set to a report date", {
  ## the end date precedes the first report, so a report-date onset would
  ## give an end before the start
  x <- reports(
    3, c("2013-01-10", "2013-01-17"), c(TRUE, TRUE), c(NA, "yes"),
    c(NA, NA), c("2013-01-08", NA)
  )
  capture.output(res <- bouts_of_illness(x))
  expect_identical(nrow(res), 1L)
  expect_true(is.na(res$symptoms.start.date))
  expect_identical(res$symptoms.end.date, as.Date("2013-01-08"))
})

test_that("incidence does not count bouts with an unknown onset", {
  x <- reports(
    3, c("2013-01-10", "2013-01-17"), c(TRUE, TRUE), c(NA, "yes"),
    c(NA, NA), c(NA, NA)
  )
  x[, c("ili", "min.date", "max.date") := list(
    factor("t", levels = c("f", "t")), min(date), max(date)
  )]
  capture.output(inc <- get_incidence(x, aggregation = "day"))
  expect_false(anyNA(inc$day))
  expect_identical(sum(inc$ili), 0)
})
