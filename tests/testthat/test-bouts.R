bout_reports <- function(id, dates, symptomatic, same, start, end) {
  data.table::data.table(
    participant_id = id, global_id = paste0("p", id), season = "2012/13",
    symptom.id = seq_along(dates), date = as.Date(dates),
    no.symptoms = factor(ifelse(symptomatic, "f", "t"), levels = c("f", "t")),
    same = factor(same, levels = c("yes", "no", "dont_know")),
    symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end)
  )
}

test_that("bouts_of_illness uses an end date given only by the last report", {
  x <- bout_reports(
    1, c("2013-01-03", "2013-01-10", "2013-01-17"), c(FALSE, TRUE, TRUE),
    c(NA, "no", "yes"), c(NA, "2013-01-08", NA), c(NA, NA, "2013-01-15")
  )
  invisible(capture.output(res <- bouts_of_illness(x)))
  expect_equal(nrow(res), 1)
  expect_equal(res$symptoms.end.date, as.Date("2013-01-15"))
})

test_that("bouts_of_illness uses the last end date given within a bout", {
  x <- bout_reports(
    1, c("2013-01-03", "2013-01-10", "2013-01-17"), c(FALSE, TRUE, TRUE),
    c(NA, "no", "yes"), c(NA, "2013-01-08", NA), c(NA, "2013-01-12", NA)
  )
  invisible(capture.output(res <- bouts_of_illness(x)))
  expect_equal(res$symptoms.end.date, as.Date("2013-01-12"))
})
