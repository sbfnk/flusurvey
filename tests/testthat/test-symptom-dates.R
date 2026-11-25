## a season with two synthetic participants, each reporting weekly
fixture_season <- function() {
  season <- simulated_data[["2013"]]
  symptom <- data.table::copy(season$symptom)
  well <- symptom[no.symptoms == "t"][1]
  ill <- data.table::copy(symptom[no.symptoms == "f"][1])
  ill[, c("fever", "tired", "cough") := "t"]
  report <- function(who, template, when, from = NA, to = NA) {
    row <- data.table::copy(template)
    row[, `:=`(global_id = who, date = as.Date(when),
               symptoms.start.date = as.Date(from),
               symptoms.end.date = as.Date(to))]
    row
  }
  rows <- data.table::rbindlist(list(
    report("early", well, "2013-01-01"),
    report("early", ill, "2013-01-10", "2012-12-25", "2013-01-08"),
    report("early", well, "2013-01-17"),
    report("early", well, "2013-01-24"),
    report("long", well, "2012-12-20"),
    report("long", well, "2013-01-08"),
    report("long", ill, "2013-01-31", "2013-01-10", "2013-01-30"),
    report("long", well, "2013-02-07")
  ))
  background <- data.table::rbindlist(lapply(c("early", "long"), function(who) {
    row <- data.table::copy(season$background[1])
    row[, `:=`(global_id = who, date = as.Date("2012-12-01"))]
    row
  }))
  list(background = background, symptom = rows)
}

fixture_data <- function(...) {
  extract_data(list("2013" = fixture_season()), clean = c(
    "remove.first", "remove.bad.symptom.dates", "unsuccessful.join"
  ), ...)
}

test_that("reported onset dates are kept and flagged", {
  dt <- fixture_data()
  early <- dt[global_id == "early" & no.symptoms == "f"]
  expect_equal(early$symptoms.start.date, as.Date("2012-12-25"))
  expect_equal(early$symptoms.end.date, as.Date("2013-01-08"))
  expect_true(early$illness.ongoing.at.entry)
  long <- dt[global_id == "long" & no.symptoms == "f"]
  expect_equal(long$symptoms.start.date, as.Date("2013-01-10"))
  expect_false(long$illness.ongoing.at.entry)
  expect_false(any(dt[no.symptoms == "t"]$illness.ongoing.at.entry))
})

test_that("bouts keep chronology and flags", {
  dt <- fixture_data()
  capture.output(bouts <- bouts_of_illness(dt))
  expect_identical(nrow(bouts), 2L)
  expect_true(all(bouts$symptoms.end.date >= bouts$symptoms.start.date))
  early <- bouts[global_id == "early"]
  expect_equal(early$symptoms.start.date, as.Date("2012-12-25"))
  expect_equal(early$symptoms.end.date, as.Date("2013-01-08"))
  expect_true(early$illness.ongoing.at.entry)
  long <- bouts[global_id == "long"]
  expect_equal(long$symptoms.start.date, as.Date("2013-01-10"))
  expect_equal(long$symptoms.end.date, as.Date("2013-01-30"))
  expect_false(long$illness.ongoing.at.entry)
})

test_that("incidence excludes illness ongoing at entry only on request", {
  dt <- fixture_data()
  capture.output(all <- get_incidence(dt, aggregation = "day"))
  expect_equal(sum(all$ili), 2)
  capture.output(
    inc <- get_incidence(dt, aggregation = "day", exclude = "ongoing.at.entry")
  )
  cases <- inc[ili > 0]
  expect_equal(cases$day, as.Date("2013-01-10"))
  expect_equal(cases$ili, 1)
  expect_equal(sum(inc$ili), 1)
})

test_that("incidence assigns retained early onsets to the first retained report", {
  season <- fixture_season()
  season$background <- season$background[global_id == "early"]
  season$symptom <- season$symptom[global_id == "early"]
  dt <- extract_data(list("2013" = season), clean = c(
    "remove.first", "remove.bad.symptom.dates", "unsuccessful.join"
  ))
  for (aggregation in c("day", "week", "month", "year")) {
    for (denominator in c("active.members", "reports")) {
      capture.output(retained <- get_incidence(
        dt, aggregation = aggregation, denominator = denominator
      ))
      expect_equal(sum(retained$ili), 1, info = paste(aggregation, denominator))
      if (aggregation == "day") {
        expect_equal(retained[ili > 0]$day, as.Date("2013-01-10"))
      }
    }
    capture.output(inc <- get_incidence(
      dt, aggregation = aggregation, exclude = "ongoing.at.entry"
    ))
    expect_gt(nrow(inc), 0)
    expect_equal(sum(inc$ili), 0, info = aggregation)
  }
})

test_that("first report evidence follows a continuing illness", {
  season <- fixture_season()
  season$background <- season$background[global_id == "early"]
  season$symptom <- season$symptom[global_id == "early"]
  first <- data.table::copy(season$symptom[date == as.Date("2013-01-10")])
  first[, `:=`(date = as.Date("2013-01-01"),
               symptoms.end.date = as.Date(NA))]
  season$symptom <- season$symptom[date != as.Date("2013-01-01")]
  season$symptom[date == as.Date("2013-01-10"), `:=`(
    same = "yes", symptoms.start.date = as.Date(NA)
  )]
  season$symptom <- data.table::rbindlist(list(first, season$symptom))
  dt <- extract_data(list("2013" = season), clean = c(
    "remove.first", "remove.bad.symptom.dates", "unsuccessful.join"
  ))
  capture.output(bouts <- bouts_of_illness(dt))
  expect_true(bouts$illness.ongoing.at.entry[1])
  capture.output(inc <- get_incidence(
    dt, aggregation = "day", exclude = "ongoing.at.entry"
  ))
  expect_equal(sum(inc$ili), 0)

  season$symptom[date == as.Date("2013-01-10"), same := "no"]
  separate <- extract_data(list("2013" = season), clean = c(
    "remove.first", "remove.bad.symptom.dates", "unsuccessful.join"
  ))
  capture.output(bouts <- bouts_of_illness(separate))
  expect_false(bouts$illness.ongoing.at.entry[1])
})
