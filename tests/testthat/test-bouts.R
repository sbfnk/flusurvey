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

test_that("bout dates preserve reports and reject inverted intervals", {
  x <- rbind(
    reports(1, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            c("2013-01-05", "2013-01-06"),
            c("2013-01-08", "2013-01-12")),
    reports(2, "2013-01-10", TRUE, "no", NA, NA),
    reports(3, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            c("2013-01-09", NA),
            c(NA, "2013-01-08"))
  )
  original <- data.table::copy(x)
  capture.output(out <- bouts_of_illness(x))
  expect_equal(x, original)
  expect_equal(out[participant_id == 1, symptoms.start.date], as.Date("2013-01-06"))
  expect_equal(out[participant_id == 1, symptoms.end.date], as.Date("2013-01-12"))
  expect_true(is.na(out[participant_id == 2, symptoms.start.date]))
  expect_true(is.na(out[participant_id == 3, symptoms.start.date]))
  expect_true(is.na(out[participant_id == 3, symptoms.end.date]))
})

test_that("unknown onsets do not become incidence or contact events", {
  x <- rbind(
    reports(1, "2013-01-10", TRUE, "no", NA, NA),
    reports(2, "2013-01-10", TRUE, "no", "2013-01-10", "2013-01-10")
  )
  x[, ili := "t"]
  x[, `:=`(min.date = date, max.date = date)]
  capture.output(bouts <- bouts_of_illness(x))
  contacts <- data.table::data.table(
    global_id = c("p1", "p2", "p2"),
    contact.date = as.Date(c("2013-01-10", "2013-01-10", "2013-01-11"))
  )
  expect_identical(flag_ill_contacts(contacts, bouts)$ill, c(FALSE, TRUE, FALSE))
  for (period in c("day", "week", "month", "year")) {
    capture.output(res <- get_incidence(x, aggregation = period,
                                        denominator = "reports"))
    expect_equal(sum(res$ili), 1, info = period)
    capture.output(empty <- get_incidence(x[participant_id == 1],
                                           aggregation = period,
                                           denominator = "reports"))
    expect_equal(sum(empty$ili), 0, info = period)
  }
})
