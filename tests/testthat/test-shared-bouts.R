## Synthetic end-to-end checks of the shared bout reconstruction: raw symptom
## columns -> aggregate_symptoms() -> bout_evidence() / bouts_of_illness() ->
## contact_illness().

flu <- function(date, symptoms = character(), same = NA, start = NA,
                end = NA, option = "date", onset = "yes", id = "a") {
  ill <- length(symptoms) > 0
  cols <- c("fever", "tired", "cough", "sore.throat")
  res <- data.table::data.table(
    global_id = id, participant_id = 1L, season = "2012/13",
    date = as.Date(date), no.symptoms = if (ill) "f" else "t",
    same = same, symptoms.start.date = as.Date(start),
    symptoms.end.date = as.Date(end), symptoms.end.option = option,
    symptoms.suddenly = if (ill) onset else NA_character_
  )
  for (col in cols) res[, (col) := if (col %in% symptoms) "t" else "f"]
  res
}

prepare <- function(...) {
  res <- aggregate_symptoms(data.table::rbindlist(list(...)))
  res[, no.symptoms := factor(no.symptoms, levels = c("f", "t"))]
  res[, same := factor(same, levels = c("yes", "no", "dont_know"))]
  res[, symptom.id := seq_len(.N)]
  res
}

wellness <- function() {
  list(flu("2013-01-01"), flu("2013-01-31"))
}

test_that("a still-ill report without onset establishes only its own day", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-15", c("fever", "cough"), option = "still_ill")
  )))
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 1)
  expect_false(evidence$bouts$onset.known)
  expect_false(evidence$bouts$recovery.known)
  expect_equal(evidence$reports$basis, "still.ill.unknown.onset")
  expect_equal(evidence$days$day, as.Date("2013-01-15"))
  expect_equal(evidence$days$source, "report.date")
  ## the contact filed on 16 January describes 15 January; neither recovery
  ## on the 15th nor illness on the 16th is established
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13",
    contact.date = as.Date(c("2013-01-14", "2013-01-15", "2013-01-16"))
  )
  res <- contact_illness(contacts, symptoms)
  data.table::setkey(res, contact.date)
  expect_equal(res$current_symptoms, c(NA, "symptoms", NA))
  expect_equal(res$bout_ili[2], "ILI")
})

test_that("adjacent illnesses keep their own case definitions", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", c("fever", "cough"), start = "2013-01-03",
        end = "2013-01-05"),
    flu("2013-01-09", "cough", same = "no", start = "2013-01-06",
        end = "2013-01-09", onset = "no")
  )))
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 2)
  expect_equal(evidence$bouts$ili, c("t", "f"))
  expect_equal(evidence$bouts$ili.symptoms, c("t", "f"))
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13",
    contact.date = as.Date(c("2013-01-05", "2013-01-06"))
  )
  res <- contact_illness(contacts, symptoms)
  data.table::setkey(res, contact.date)
  expect_equal(res$current_symptoms, c("symptoms", "symptoms"))
  expect_equal(res$bout_ili, c("ILI", "no ILI"))
  expect_equal(res$bout_ari, c("ARI", "ARI"))
  expect_equal(res$bout_ili_fever, c("ILI", "no ILI"))
})

jointly <- function() {
  do.call(prepare, c(wellness(), list(
    flu("2013-01-05", "fever", start = "2013-01-03", end = "2013-01-05"),
    flu("2013-01-09", "cough", same = "yes", start = "2013-01-03",
        end = "2013-01-08")
  )))
}

test_that("symptoms in separate reports of one bout jointly meet ILI", {
  symptoms <- jointly()
  expect_equal(symptoms[no.symptoms == "f"]$ili, c("f", "f"))
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 1)
  expect_equal(evidence$bouts$ili, "t")
  expect_equal(evidence$bouts$ili.fever, "t")
  expect_equal(evidence$bouts$resp.symptoms, "t")
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13", contact.date = as.Date("2013-01-04")
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$bout_ili, "ILI")
  expect_equal(res$bout_ili_fever, "ILI")
  capture.output(bouts <- bouts_of_illness(symptoms))
  expect_equal(as.character(bouts$ili), "t")
})

test_that("onset answers stay apart when the symptoms of a bout are combined", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", c("fever", "cough"), start = "2013-01-03",
        end = "2013-01-05", onset = "dont_know")
  )))
  expect_equal(bout_evidence(symptoms)$bouts$onset, "dont_know")
  expect_true(is.na(bout_evidence(symptoms)$bouts$ili))
  unanswered <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", c("fever", "cough"), start = "2013-01-03",
        end = "2013-01-05", onset = NA)
  )))
  expect_equal(bout_evidence(unanswered)$bouts$onset, "not.answered")
  ## a later report that records a sudden onset settles it
  both <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", c("fever", "cough"), start = "2013-01-03",
        end = "2013-01-05", onset = "dont_know"),
    flu("2013-01-09", "cough", same = "yes", start = "2013-01-03",
        end = "2013-01-08", onset = "yes")
  )))
  expect_equal(bout_evidence(both)$bouts$ili, "t")
})

test_that("flags are constant over a bout whatever the report dates", {
  symptoms <- jointly()
  capture.output(
    daily <- bouts_of_illness(symptoms, daily.flags = "bout")
  )
  expect_equal(length(unique(as.character(daily$ili))), 1)
  expect_equal(as.character(daily$ili[1]), "t")
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13",
    contact.date = seq(as.Date("2013-01-03"), as.Date("2013-01-08"), by = "day")
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$bout_ili, rep("ILI", 6))
  ## the daily columns describe days with a report and are open elsewhere
  expect_true(any(is.na(res$current_ili)))
})

test_that("a bout with a recovery date before the report and no onset is kept", {
  symptoms <- do.call(prepare, c(
    list(flu("2013-01-03")),
    list(flu("2013-01-10", "cough", end = "2013-01-08")),
    list(flu("2013-01-31"))
  ))
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 1)
  expect_false(evidence$bouts$onset.known)
  expect_true(evidence$bouts$recovery.known)
  expect_equal(evidence$reports$basis, "unresolved")
  expect_equal(nrow(evidence$days), 0)
  ## the default output keeps the bout, with the date of its report as start
  capture.output(bouts <- bouts_of_illness(symptoms))
  expect_equal(nrow(bouts), 1)
  expect_equal(bouts$symptoms.start.date, as.Date("2013-01-10"))
  ## the days up to the report are unknown, as the onset is
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13",
    contact.date = as.Date(c("2013-01-05", "2013-01-09", "2013-01-20"))
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$current_symptoms, c(NA, NA, "no symptoms"))
})

test_that("incidence inclusion and contact illness evidence are separate", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-15", "cough", option = "still_ill"),
    flu("2013-01-22", "cough", same = "no", start = "2013-01-20",
        end = "2013-01-22")
  )))
  evidence <- bout_evidence(symptoms)
  expect_equal(evidence$bouts$onset.known, c(FALSE, TRUE))
  ## an incidence rule that needs an observed onset leaves out the first bout
  incident <- evidence$bouts[onset.known == TRUE]
  expect_equal(nrow(incident), 1)
  ## the contact classification still uses the days of both
  contacts <- data.table::data.table(
    global_id = "a", season = "2012/13",
    contact.date = as.Date(c("2013-01-15", "2013-01-21"))
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$current_symptoms, c("symptoms", "symptoms"))
})

test_that("extraction, shared bouts and contacts agree", {
  raw <- data.table::rbindlist(list(
    data.table::data.table(
      global_id = "a", date = as.Date(c("2013-01-01", "2013-01-31")),
      no.symptoms = "t", same = NA, symptoms.suddenly = NA,
      symptoms.start.date = as.Date(NA), symptoms.end.date = as.Date(NA),
      symptoms.end.option = "date", fever = "f", tired = "f", cough = "f"
    ),
    data.table::data.table(
      global_id = "a", date = as.Date(c("2013-01-06", "2013-01-10")),
      no.symptoms = "f", same = c(NA, "yes"), symptoms.suddenly = "yes",
      symptoms.start.date = as.Date("2013-01-04"),
      symptoms.end.date = as.Date(c(NA, "2013-01-09")),
      symptoms.end.option = c("still_ill", "date"),
      fever = c("t", "f"), tired = "f", cough = c("f", "t")
    )
  ))
  symptoms <- extract_data(
    list(`2013` = list(symptom = raw)), surveys = "symptom",
    clean = c("unsuccessful.join", "remove.bad.symptom.dates")
  )
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 1)
  expect_equal(evidence$bouts$ili, "t")
  expect_equal(range(evidence$days$day),
               as.Date(c("2013-01-04", "2013-01-09")))
  contacts <- data.table::data.table(
    global_id = "a", contact.date = as.Date(c("2013-01-04", "2013-01-12"))
  )
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$bout_ili, c("ILI", "no ILI"))
  expect_equal(res$current_symptoms, c("symptoms", "no symptoms"))
})

test_that("leading symptomatic reports use the same reconstruction for all consumers", {
  symptoms <- prepare(
    flu("2013-01-02", "cough", same = "yes"),
    flu("2013-01-10"),
    flu("2013-01-15", c("fever", "cough"), start = "2013-01-14",
        end = "2013-01-16"),
    flu("2013-01-31")
  )
  res <- reconstruct_bouts(symptoms)
  res <- res[no.symptoms == "f"]
  expect_equal(res$bout[order(res$date)], c(1L, 2L))
})

test_that("one bout stays ill between established illness days", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", "fever", option = "still_ill"),
    flu("2013-01-10", "cough", same = "yes", option = "still_ill")
  )))
  evidence <- bout_evidence(symptoms)
  expect_equal(nrow(evidence$bouts), 1L)
  expect_true(all(is.na(evidence$bouts$symptoms.start.date)))
  expect_equal(evidence$days$day, seq(as.Date("2013-01-05"),
                                     as.Date("2013-01-10"), by = "day"))
  expect_equal(evidence$days$source[2:5], rep("within.bout", 4))
  contacts <- data.table::data.table(global_id = "a",
    contact.date = as.Date(c("2013-01-06", "2013-01-09")))
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$current_symptoms, rep("symptoms", 2))
  expect_equal(res$bout_ili, rep("ILI", 2))
})

test_that("negative bout flags do not vary with unresolved report dates", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", "cough", start = "2013-01-03", option = "still_ill"),
    flu("2013-01-10", "cough", same = "yes"),
    flu("2013-01-15", "cough", same = "yes", start = "2013-01-03",
        end = "2013-01-14")
  )))
  contacts <- data.table::data.table(global_id = "a",
    contact.date = as.Date(c("2013-01-04", "2013-01-09", "2013-01-14")))
  res <- contact_illness(contacts, symptoms)
  expect_equal(res$current_symptoms, rep("symptoms", 3))
  expect_equal(res$bout_ili, rep("no ILI", 3))
})

test_that("date policies preserve report evidence and do not invent daily rows", {
  symptoms <- prepare(flu("2013-01-10", "cough", end = "2013-01-08"))
  for (policy in c("keep", "clear", "reset.end")) {
    e <- bout_evidence(symptoms, missing.start = "first.report",
                       inverted.dates = policy)
    expect_equal(e$reports$report.end, as.Date("2013-01-08"))
    expect_false(e$bouts$onset.known)
    expect_equal(e$bouts$recovery.known, policy == "keep")
  }
  capture.output(daily <- bouts_of_illness(symptoms, missing.start = "missing",
                                          daily.flags = "bout"))
  expect_equal(nrow(daily), 0L)
  capture.output(daily <- bouts_of_illness(symptoms, daily.flags = "bout"))
  expect_equal(nrow(daily), 0L)
})

test_that("unknown component answers are not silently negative for the bout", {
  symptoms <- jointly()
  symptoms[, fever := NA_character_]
  e <- bout_evidence(symptoms)
  expect_true(is.na(e$bouts$ili))
})

test_that("unknown-onset contact evidence survives actual incidence exclusion", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-15", c("fever", "cough"), option = "still_ill")
  )))
  symptoms[, `:=`(min.date = min(date), max.date = max(date))]
  for (period in c("day", "week", "month", "year")) {
    capture.output(inc <- get_incidence(symptoms, aggregation = period,
      missing.start = "missing", exclude = "unknown.onset"))
    expect_equal(sum(inc$ili), 0)
  }
  contact <- data.table::data.table(global_id = "a",
                                   contact.date = as.Date("2013-01-15"))
  expect_equal(contact_illness(contact, symptoms)$bout_ili, "ILI")
})

test_that("leading reports stay in one bout when its onset is revised", {
  for (answer in c("yes", "dont_know", NA_character_)) {
    symptoms <- prepare(
      flu("2013-01-17", "fever", same = answer, start = "2013-01-15",
          option = "still_ill"),
      flu("2013-01-24", "cough", same = "yes", start = "2013-01-10",
          option = "still_ill"),
      flu("2013-01-31")
    )
    e <- bout_evidence(symptoms)
    expect_equal(nrow(e$bouts), 1L)
    expect_equal(nrow(e$reports), 2L)
    expect_equal(e$bouts$ili, "t")
  }
})

test_that("onset from a later report can locate an earlier still-ill report", {
  symptoms <- do.call(prepare, c(wellness(), list(
    flu("2013-01-05", "fever", option = "still_ill"),
    flu("2013-01-10", "cough", same = "yes", start = "2013-01-03",
        end = "2013-01-09")
  )))
  original <- data.table::copy(symptoms)
  e <- bout_evidence(symptoms)
  expect_true(is.na(e$reports$report.start[1]))
  expect_equal(e$reports$first.day[1], as.Date("2013-01-03"))
  expect_equal(range(e$days$day), as.Date(c("2013-01-03", "2013-01-09")))
  expect_equal(symptoms, original)
  capture.output(b <- bouts_of_illness(symptoms, missing.start = "missing"))
  expect_equal(b$symptoms.start.date, e$bouts$symptoms.start.date)
  expect_equal(b$symptoms.end.date, e$bouts$symptoms.end.date)
  expect_equal(as.character(b$ili), e$bouts$ili)
})

test_that("conflicting onset evidence remains visible", {
  symptoms <- jointly()
  symptoms[date == as.Date("2013-01-09"), onset := "not.sudden"]
  symptoms[date == as.Date("2013-01-09"), suddenly := "f"]
  e <- bout_evidence(symptoms)
  expect_true(e$bouts$onset.conflict)
  expect_equal(e$bouts$onset, "sudden")
  expect_equal(e$reports$onset, c("sudden", "not.sudden"))
})
