test_that("simulated data has the questionnaires of both seasons", {
  expect_named(simulated_data, c("2013", "2014"))
  for (season in simulated_data) {
    expect_named(season, c("background", "contact", "symptom"))
    for (survey in season) expect_gt(nrow(survey), 0)
  }
})

test_that("the contact warning expectation leaves other warnings visible", {
  expect_failure(expect_contact_warning(NULL))
  expect_warning(
    expect_contact_warning({
      warning("The contact and symptom surveys are joined by report date")
      warning("Another warning")
    }),
    "Another warning"
  )
})

test_that("extract_data returns a non-empty table with default cleaning", {
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  expect_s3_class(dt, "data.table")
  expect_gt(nrow(dt), 0)
  expect_setequal(unique(dt$season), c("2012/13", "2013/14"))
  expect_true(all(c("ili", "agegroup", "region", "participant_id") %in%
                  colnames(dt)))
  expect_true(any(dt$ili == "t", na.rm = TRUE))
})

test_that("extract_data accepts the default years = 'all'", {
  all_years <- expect_contact_warning(extract_data(simulated_data))
  expect_setequal(unique(all_years$season), c("2012/13", "2013/14"))
  expect_equal(
    extract_data(simulated_data, years = "all", surveys = "background"),
    extract_data(simulated_data, years = 2013:2014, surveys = "background")
  )
})

test_that("get_incidence returns incidence for both seasons", {
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  incidence <- get_incidence(dt, incidence.columns = "ili",
                             aggregation = "week")
  expect_gt(nrow(incidence), 0)
  expect_setequal(unique(incidence$season), c("2012/13", "2013/14"))
  expect_gt(sum(incidence$ili), 0)
  by_age <- get_incidence(dt, by = "agegroup")
  expect_gt(nrow(by_age), 0)
})

test_that("get_incidence keeps periods without new cases", {
  reports <- function(id, dates, symptomatic, same, start, end) {
    data.table::data.table(
      participant_id = id, global_id = paste0("p", id), season = "2012/13",
      symptom.id = seq_along(dates), date = as.Date(dates),
      no.symptoms = factor(ifelse(symptomatic, "f", "t"),
                           levels = c("f", "t")),
      same = factor(same, levels = c("yes", "no", "dont_know")),
      symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end)
    )
  }
  weeks <- c("2013-01-07", "2013-01-14", "2013-01-21", "2013-01-28")
  d <- rbind(
    reports(1, weeks, c(FALSE, TRUE, FALSE, FALSE), c(NA, "no", NA, NA),
            c(NA, "2013-01-13", NA, NA), NA),
    reports(2, weeks, FALSE, NA, NA, NA)
  )
  d[, ili := data.table::fifelse(no.symptoms == "f", "t", "f")]
  d[, c("min.date", "max.date") := list(min(date), max(date)),
    by = global_id]
  res <- get_incidence(d)
  expect_equal(nrow(res), 4)
  expect_equal(res$N, rep(2, 4))
  expect_equal(sum(res$ili), 1)
  res_reports <- get_incidence(d, denominator = "reports")
  expect_equal(nrow(res_reports), 4)
  expect_equal(sum(res_reports$ili), 1)
})

test_that("bouts_of_illness returns bouts of illness", {
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  invisible(capture.output(res <- bouts_of_illness(dt)))
  expect_gt(nrow(res), 0)
  expect_false(anyNA(res$symptoms.start.date))
})

test_that("age groups follow the contact age bands", {
  ages <- c(3, 18, 19, 44, 45, 64, 65, 90)
  background <- data.table::data.table(simulated_data[["2013"]]$background)
  background <- background[!duplicated(global_id)][seq_along(ages)]
  background[, date := as.Date("2013-12-01")]
  background[, birthmonth := format(date - 365.25 * ages - 10, "%Y-%m")]
  dt <- merge_data(list(background = background), clean = "n.reports")
  dt <- dt[order(age)]
  expect_equal(dt$age, ages)
  expect_equal(
    as.character(dt$agegroup),
    c("0-18", "0-18", "19-44", "19-44", "45-64", "45-64", "65+", "65+")
  )
  unsorted <- merge_data(
    list(background = background), clean = "n.reports",
    age.breaks = c(64, 18, 44)
  )
  unsorted <- unsorted[order(age)]
  expect_identical(unsorted$agegroup, dt$agegroup)
  expect_no_warning(single <- merge_data(
    list(background = background), clean = "n.reports", age.breaks = numeric()
  ))
  expect_identical(levels(single$agegroup), "0+")
  expect_true(all(as.character(single$agegroup) == "0+"))
})


test_that("missing household counts are set to 0", {
  dt <- data.table::data.table(
    nb.household.0.4 = c(1L, NA, NA),
    nb.household.5.18 = c(NA, NA, 2L)
  )
  fill_household_counts(dt)
  expect_identical(dt$nb.household.0.4, c(1L, 0L, 0L))
  expect_identical(dt$nb.household.5.18, c(0L, 0L, 2L))
})

test_that("the order of surveys does not change the merged table", {
  forward <- extract_data(
    simulated_data, years = 2013, surveys = c("background", "symptom")
  )
  backward <- extract_data(
    simulated_data, years = 2013, surveys = c("symptom", "background")
  )
  expect_gt(nrow(backward), 0)
  expect_equal(backward, forward)
})

test_that("merge_data gives the same table whatever the order", {
  reports <- simulated_data[["2013"]]
  ## contact ranges are sampled; set the seed before each call
  set.seed(1)
  shuffled <- expect_contact_warning(
    merge_data(reports[c("symptom", "contact", "background")], clean = NULL)
  )
  set.seed(1)
  ordered <- expect_contact_warning(
    merge_data(reports[c("background", "contact", "symptom")], clean = NULL)
  )
  expect_equal(shuffled, ordered)
})

test_that("merge_data stops on surveys it cannot order", {
  reports <- simulated_data[["2013"]]
  other <- reports["symptom"]
  names(other) <- "other"
  expect_error(merge_data(c(reports["background"], other)), "Unknown survey")
  expect_error(
    merge_data(reports[c("background", "background")]),
    "only once"
  )
})

test_that("extract_data drops surveys it cannot merge when extracting all", {
  data <- simulated_data["2013"]
  data[["2013"]]$vaccine <- data[["2013"]]$background
  ## contact ranges are sampled; set the seed before each call
  set.seed(1)
  expect_warning(
    dt <- expect_contact_warning(extract_data(data, years = 2013)), "'vaccine'"
  )
  set.seed(1)
  expect_equal(dt, expect_contact_warning(
    extract_data(simulated_data["2013"], years = 2013)
  ))
})

test_that("health scores outside 0 to 100 are removed by default", {
  raw <- unlist(lapply(simulated_data, function(season) {
    season$symptom$health.score
  }))
  expect_true(any(raw < 0 | raw > 100, na.rm = TRUE))
  dt <- expect_contact_warning(extract_data(simulated_data, years = 2013:2014))
  expect_true(all(dt$health.score >= 0 & dt$health.score <= 100, na.rm = TRUE))
})

test_that("default extraction handles symptom surveys without health scores", {
  data <- lapply(simulated_data, function(season) {
    season$symptom <- data.table::copy(season$symptom)
    season$symptom[, health.score := NULL]
    season
  })
  other_cleaning <- c(
    "remove.first", "remove.bad.symptom.dates", "limit.season",
    "remove.postcodes", "n.reports", "unsuccessful.join", "only.symptoms"
  )
  for (years in list(2013, 2014, 2013:2014)) {
    expected <- extract_data(data, years = years, surveys = "symptom",
                             clean = other_cleaning)
    actual <- extract_data(data, years = years, surveys = "symptom")
    expect_gt(nrow(expected), 0)
    expect_false("health.score" %in% names(actual))
    expect_equal(actual, expected)
  }
})

test_that("limit.season does not drop symptom reports by background date", {
  d <- data.table::copy(simulated_data[["2013"]][c("background", "symptom")])
  d_original <- data.table::copy(d)
  d$background <- data.table::data.table(d$background)
  d$background[global_id == "sim0002", date := as.Date("2012-10-15")]
  clean <- c("limit.season", "unsuccessful.join")
  expected <- merge_data(d_original, clean = clean)[global_id == "sim0002"]
  expect_gt(nrow(expected), 0)
  actual <- merge_data(d, clean = clean)[global_id == "sim0002"]
  expect_equal(actual$date, expected$date)
})

test_that("limit.season limits the contact survey when no symptom survey is merged", {
  d <- data.table::copy(simulated_data[["2013"]][c("background", "contact")])
  d$background <- data.table::data.table(d$background)
  d$contact <- data.table::data.table(d$contact)
  d$background[global_id == "sim0002", date := as.Date("2012-10-01")]
  d$contact[global_id == "sim0002", date := as.Date("2012-10-15")]
  clean <- c("limit.season", "unsuccessful.join")
  merged <- merge_data(d, clean = clean)
  early <- merged$global_id == "sim0002" & merged$date < as.Date("2012-11-01")
  expect_false(any(early))
  alone <- merge_data(d["contact"], clean = clean)
  expect_false(any(alone$date < as.Date("2012-11-01")))
})

test_that("remove.bad.health.scores is accepted by name", {
  dt <- expect_contact_warning(
    extract_data(simulated_data, years = 2013:2014,
                 clean = "remove.bad.health.scores")
  )
  expect_true(all(dt$health.score >= 0 & dt$health.score <= 100, na.rm = TRUE))
  dt <- expect_contact_warning(
    extract_data(simulated_data, years = 2013:2014, clean = "remove.first")
  )
  expect_true(any(dt$health.score < 0 | dt$health.score > 100, na.rm = TRUE))
})

test_that("merge_data dates contact reports by the day they describe", {
  dt <- expect_contact_warning(merge_data(simulated_data[["2013"]]))
  expect_true(all(c("contact.date", "contact.day.of.week") %in% colnames(dt)))
  has_contact <- !is.na(dt$contact.date)
  expect_true(any(has_contact))
  ## the survey asks about the previous day: the contact day precedes the
  ## date of the (possibly rolled-forward) report
  expect_true(all(dt$contact.date[has_contact] < dt$date[has_contact]))
  ## Monday is 1 and Sunday 7, as for day.of.week
  expect_equal(
    dt$contact.day.of.week[has_contact],
    as.integer(format(dt$contact.date[has_contact], "%u"))
  )
  ## a contact report filed on Monday describes a Sunday
  monday <- expect_contact_warning(
    merge_data(simulated_data[["2013"]], clean = NULL)
  )[day.of.week == 1 & !is.na(contact.date) & contact.date == date - 1]
  expect_gt(nrow(monday), 0)
  expect_true(all(monday$contact.day.of.week == 7))
})
