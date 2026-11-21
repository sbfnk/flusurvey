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

test_that("reports outside a bout keep their own values", {
  x <- data.table(
    participant_id = 1, global_id = "p1", season = "2012/13",
    symptom.id = 1:5,
    date = as.Date(c(
      "2013-01-03", "2013-01-10", "2013-01-17", "2013-02-20", "2013-02-27"
    )),
    no.symptoms = factor(c("f", "f", "t", "t", "t")),
    same = factor(
      c("yes", NA, NA, NA, NA), levels = c("yes", "no", "dont_know")
    ),
    symptoms.start.date = as.Date(c("2013-01-02", "2013-01-02", NA, NA, NA)),
    symptoms.end.date = as.Date(c(NA, "2013-01-12", NA, NA, NA)),
    ili = factor(c("t", "f", "f", "t", "f"))
  )
  capture.output(res <- bouts_of_illness(x, symptomatic.only = FALSE))
  expect_identical(res$bout, c(1L, NA, NA, NA))
  expect_identical(as.character(res$no.symptoms), c("f", "t", "t", "t"))
  expect_identical(as.character(res$ili), c("t", "f", "t", "f"))
})
