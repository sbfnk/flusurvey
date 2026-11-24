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

dates_example <- function() {
  rbind(
    reports(1, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            c("2013-01-05", "2013-01-06"),
            c("2013-01-08", "2013-01-12")),
    reports(2, "2013-01-10", TRUE, "no", NA, NA),
    reports(3, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            c("2013-01-09", NA),
            c(NA, "2013-01-08"))
  )
}
bout_dates <- function(x, col, ...) {
  capture.output(out <- bouts_of_illness(x, ...))
  out[order(participant_id), get(col)]
}

test_that("the defaults keep the last reported start date", {
  x <- dates_example()
  original <- data.table::copy(x)
  expect_equal(bout_dates(x, "symptoms.start.date"),
               as.Date(c("2013-01-06", NA, "2013-01-09")))
  expect_equal(bout_dates(x, "symptoms.end.date"),
               as.Date(c("2013-01-12", "2013-01-10", "2013-01-08")))
  expect_equal(x, original)
})

test_that("start.date chooses the earliest reported start date", {
  expect_equal(
    bout_dates(dates_example(), "symptoms.start.date", start.date = "earliest"),
    as.Date(c("2013-01-05", NA, "2013-01-09"))
  )
})

test_that("missing.start chooses what unreported start dates become", {
  res <- bout_dates(dates_example(), "symptoms.start.date",
                    missing.start = "first.report")
  expect_equal(res, as.Date(c("2013-01-06", "2013-01-10", "2013-01-09")))
  res <- bout_dates(dates_example(), "symptoms.start.date",
                    missing.start = "missing")
  expect_equal(res, as.Date(c("2013-01-06", NA, "2013-01-09")))
})

test_that("inverted.dates keeps, clears or resets inverted bounds", {
  x <- dates_example()
  expect_equal(
    bout_dates(x, "symptoms.start.date", inverted.dates = "clear"),
    as.Date(c("2013-01-06", NA, NA))
  )
  expect_equal(
    bout_dates(x, "symptoms.end.date", inverted.dates = "clear"),
    as.Date(c("2013-01-12", "2013-01-10", NA))
  )
  expect_equal(
    bout_dates(x, "symptoms.end.date", inverted.dates = "reset.end"),
    as.Date(c("2013-01-12", "2013-01-10", "2013-01-17"))
  )
  expect_equal(
    bout_dates(x, "symptoms.start.date", inverted.dates = "reset.end"),
    as.Date(c("2013-01-06", NA, "2013-01-09"))
  )
})

end_example <- function() {
  rbind(
    ## three distinct reported end dates: the last one is used for every row
    reports(1, c("2013-01-10", "2013-01-17", "2013-01-24"), TRUE,
            c("no", "yes", "yes"), "2013-01-08",
            c("2013-01-09", "2013-01-15", "2013-01-20")),
    ## only the first report gives an end date
    reports(2, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            "2013-01-08", c("2013-01-12", NA)),
    ## only the last report gives an end date
    reports(3, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"),
            "2013-01-08", c(NA, "2013-01-16")),
    ## no end date: the date of the last symptomatic report is used
    reports(4, c("2013-01-10", "2013-01-17", "2013-01-24"),
            c(TRUE, TRUE, FALSE), c("no", "yes", NA), "2013-01-08", NA)
  )
}

test_that("a bout ends on its last reported end date, whatever the arguments", {
  x <- end_example()
  expected <- as.Date(c("2013-01-20", "2013-01-12", "2013-01-16",
                        "2013-01-17"))
  expect_equal(bout_dates(x, "symptoms.end.date"), expected)
  expect_equal(
    bout_dates(x, "symptoms.end.date", start.date = "earliest",
               missing.start = "missing", inverted.dates = "reset.end"),
    expected
  )
})

test_that("every report of a bout keeps the same end date", {
  capture.output(
    res <- bouts_of_illness(end_example(), symptomatic.only = FALSE)
  )
  expect_equal(
    res[participant_id == 1, unique(symptoms.end.date)],
    as.Date("2013-01-20")
  )
})

test_that("participants with no reported onset each get one bout", {
  x <- rbind(
    reports(1, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"), NA, NA),
    reports(2, c("2013-01-10", "2013-01-17"), TRUE, c("no", "yes"), NA, NA)
  )
  expect_equal(
    bout_dates(x, "symptoms.start.date", missing.start = "missing"),
    as.Date(c(NA, NA))
  )
  expect_equal(
    bout_dates(x, "symptoms.start.date", missing.start = "first.report"),
    as.Date(c("2013-01-10", "2013-01-10"))
  )
  expect_equal(bout_dates(x, "symptoms.end.date"),
               as.Date(c("2013-01-17", "2013-01-17")))
})

test_that("invalid rule arguments are rejected", {
  expect_error(bouts_of_illness(dates_example(), start.date = "middle"))
  expect_error(bouts_of_illness(dates_example(), missing.start = "drop"))
  expect_error(bouts_of_illness(dates_example(), inverted.dates = "swap"))
})

incidence_example <- function() {
  x <- rbind(
    reports(1, "2013-01-10", TRUE, "no", NA, NA),
    reports(2, "2013-01-10", TRUE, "no", "2013-01-10", "2013-01-10"),
    reports(3, "2013-01-10", TRUE, "no", "2013-01-05", "2013-01-10"),
    reports(4, "2013-01-05", FALSE, NA, NA, NA)
  )
  x[, ili := ifelse(participant_id == 4, "f", "t")]
  x[, `:=`(min.date = date, max.date = date)]
  x
}
total_incidence <- function(x, period, ...) {
  capture.output(res <- get_incidence(x, aggregation = period,
                                      denominator = "reports", ...))
  sum(res$ili)
}
periods <- c("day", "week", "month", "year")

test_that("get_incidence leaves out bouts with unknown onset by default", {
  x <- incidence_example()
  for (period in periods) {
    expect_equal(total_incidence(x, period), 2, info = period)
    expect_equal(
      total_incidence(x, period, missing.start = "first.report"), 3,
      info = period
    )
  }
})

test_that("exclude leaves out bouts with unknown onset", {
  x <- incidence_example()
  for (period in periods) {
    expect_equal(
      total_incidence(x, period, exclude = "unknown.onset",
                      missing.start = "missing"),
      2, info = period
    )
    expect_equal(
      total_incidence(x[participant_id == 1], period,
                      exclude = "unknown.onset", missing.start = "missing"),
      0, info = period
    )
  }
})

test_that("exclude leaves out bouts ongoing at entry", {
  x <- incidence_example()
  for (period in periods) {
    expect_equal(
      total_incidence(x, period, exclude = "ongoing.at.entry",
                      missing.start = "first.report"),
      2, info = period
    )
    expect_equal(
      total_incidence(x, period,
                      exclude = c("unknown.onset", "ongoing.at.entry"),
                      missing.start = "missing"),
      1, info = period
    )
  }
  expect_error(get_incidence(x, exclude = "other"))
})

test_that("unknown onsets are not matched to contacts by report date", {
  x <- incidence_example()[participant_id %in% 1:2]
  capture.output(bouts <- bouts_of_illness(x, missing.start = "missing"))
  contacts <- data.table::data.table(
    global_id = c("p1", "p2", "p2"),
    contact.date = as.Date(c("2013-01-10", "2013-01-10", "2013-01-11"))
  )
  expect_identical(flag_ill_contacts(contacts, bouts)$ill, c(FALSE, TRUE, FALSE))
})

test_that("unknown onset preserves a recovery before the first illness report", {
  x <- reports(1, "2013-01-10", TRUE, "no", NA, "2013-01-08")
  capture.output(b <- bouts_of_illness(x, missing.start = "missing"))
  expect_equal(nrow(b), 1L)
  expect_true(is.na(b$symptoms.start.date))
  expect_equal(b$symptoms.end.date, as.Date("2013-01-08"))
})

test_that("surveys without the same-illness question use the other start rules", {
  x <- dates_example()
  x[, same := NULL]
  original <- data.table::copy(x)
  unanswered <- data.table::copy(x)
  unanswered[, same := NA_character_]
  capture.output(
    absent <- bouts_of_illness(x, missing.start = "missing"),
    expected <- bouts_of_illness(unanswered, missing.start = "missing")
  )
  expect_equal(absent, expected)
  expect_equal(x, original)
  expect_equal(nrow(absent), 3L)
})
