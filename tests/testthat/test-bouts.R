make_reports <- function(id, dates, symptomatic, same, start, end) {
  data.table::data.table(
    participant_id = id, global_id = paste0("p", id), season = "2012/13",
    symptom.id = seq_along(dates), date = as.Date(dates),
    no.symptoms = factor(ifelse(symptomatic, "f", "t"), levels = c("f", "t")),
    same = factor(same, levels = c("yes", "no", "dont_know")),
    symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end)
  )
}

bouts <- function(x) {
  invisible(capture.output(res <- bouts_of_illness(x)))
  res
}

test_that("a report starting after the previous report begins a new bout", {
  x <- make_reports(
    1, c("2013-01-03", "2013-01-10", "2013-01-17"), c(FALSE, TRUE, TRUE),
    c(NA, "no", "yes"), c(NA, "2013-01-08", "2013-01-16"), c(NA, NA, NA)
  )
  res <- bouts(x)
  expect_equal(nrow(res), 2)
  expect_equal(res$symptoms.start.date, as.Date(c("2013-01-08", "2013-01-16")))
  expect_true(all(res$symptoms.start.date <= res$symptoms.end.date))
})

test_that("a bout takes the earliest start date of its reports", {
  x <- make_reports(
    1, c("2013-01-10", "2013-01-17"), c(TRUE, TRUE),
    c("no", "yes"), c("2013-01-08", "2013-01-09"), c(NA, NA)
  )
  res <- bouts(x)
  expect_equal(nrow(res), 1)
  expect_equal(res$symptoms.start.date, as.Date("2013-01-08"))
  expect_equal(res$symptoms.end.date, as.Date("2013-01-17"))
})

a <- make_reports(
  1, c("2013-01-03", "2013-01-10"), c(FALSE, TRUE), c(NA, "no"),
  c(NA, "2013-01-05"), c(NA, "2013-01-08")
)
b <- make_reports(
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
