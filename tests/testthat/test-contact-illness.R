report <- function(id, date, ill = TRUE, start = NA, end = NA,
                   option = "date", ili = "f", ili.fever = "f",
                   ili.self = "f", ari = "f", season = NULL) {
  res <- data.table::data.table(
    global_id = id, date = as.Date(date),
    no.symptoms = if (ill) "f" else "t",
    symptoms.start.date = as.Date(start), symptoms.end.date = as.Date(end),
    symptoms.end.option = option, ili = ili, ili.fever = ili.fever,
    ili.self = ili.self, resp.symptoms = ari
  )
  if (!is.null(season)) res[, season := season]
  res
}

contact <- function(id, day, season = NULL) {
  res <- data.table::data.table(global_id = id, contact.date = as.Date(day))
  if (!is.null(season)) res[, season := season]
  res
}

## reports without symptoms on the given days
well <- function(id, days, season = NULL) {
  data.table::rbindlist(lapply(days, function(day) {
    report(id, day, ill = FALSE, season = season)
  }))
}

## a participant followed up from 1 to 31 January 2013 by reports without
## symptoms on both days and in between
followed <- function(id, season = NULL) {
  well(id, c("2013-01-01", "2013-01-31"), season)
}

test_that("contact illness keeps the four case definitions separate", {
  symptoms <- data.table::rbindlist(list(
    followed(letters[1:4]),
    report("a", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "t", ili.fever = "f", ili.self = "f", ari = "f"),
    report("b", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "t", ili.fever = "t", ili.self = "f", ari = "t"),
    report("c", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "f", ili.fever = "f", ili.self = "t", ari = "t"),
    report("d", "2013-01-07", start = "2013-01-07", option = "still_ill",
           ili = "f", ili.fever = "f", ili.self = "f", ari = "t")
  ))
  res <- contact_illness(contact(letters[1:4], "2013-01-07"), symptoms)
  expect_equal(res$current_symptoms, rep("symptoms", 4))
  expect_equal(res$current_ili, c("ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$current_ili_fever, c("no ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$current_ili_self, c("no ILI", "no ILI", "ILI", "no ILI"))
  expect_equal(res$current_ari, c("no ARI", "ARI", "ARI", "ARI"))
  expect_equal(res$bout_ili, c("ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$bout_ili_fever, c("no ILI", "ILI", "no ILI", "no ILI"))
  expect_equal(res$bout_ili_self, c("no ILI", "no ILI", "ILI", "no ILI"))
  expect_equal(res$bout_ari, c("no ARI", "ARI", "ARI", "ARI"))
})

test_that("healthy days in follow-up need no report on that day", {
  symptoms <- followed("a")
  symptoms <- rbind(symptoms, well("a", "2013-01-15"))
  days <- c("2013-01-01", "2013-01-08", "2013-01-20", "2013-01-31")
  res <- contact_illness(contact("a", days), symptoms)
  expect_equal(res$follow_up, rep("in follow-up", 4))
  expect_equal(res$current_symptoms, rep("no symptoms", 4))
  for (col in c("current_ili", "current_ili_fever", "current_ili_self",
                "bout_ili", "bout_ili_fever", "bout_ili_self")) {
    expect_equal(res[[col]], rep("no ILI", 4))
  }
  expect_equal(res$current_ari, rep("no ARI", 4))
  expect_equal(res$bout_ari, rep("no ARI", 4))
})

test_that("a contact day between symptom reports lies in the bout", {
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-03", start = "2013-01-02", option = "still_ill",
           ili = "t", ili.fever = "t", ari = "t"),
    report("a", "2013-01-08", start = "2013-01-02", end = "2013-01-07",
           ili = "t", ili.fever = "f", ari = "t")
  ))
  res <- contact_illness(contact("a", c("2013-01-05", "2013-01-03")), symptoms)
  expect_equal(res$current_symptoms, c("symptoms", "symptoms"))
  ## no report is dated on 5 January: the bout description applies, the daily
  ## flags are unknown although the participant is known to be ill
  expect_equal(res$bout_ili, c("ILI", "ILI"))
  expect_equal(res$bout_ili_fever, c("ILI", "ILI"))
  expect_true(is.na(res$current_ili[res$contact.date == as.Date("2013-01-05")]))
  expect_equal(res$current_ili[res$contact.date == as.Date("2013-01-03")],
               "ILI")
  expect_true(
    is.na(res$current_ili_fever[res$contact.date == as.Date("2013-01-05")])
  )
  ## healthy again the day after the bout ends
  after <- contact_illness(contact("a", "2013-01-09"), symptoms)
  expect_equal(after$current_symptoms, "no symptoms")
  expect_equal(after$bout_ili, "no ILI")
})

test_that("bout flags combine all reports of the bout", {
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-03", start = "2013-01-02", option = "still_ill",
           ili = "t", ili.fever = "f"),
    report("a", "2013-01-05", start = "2013-01-02", end = "2013-01-04",
           ili = "f", ili.fever = "f")
  ))
  res <- contact_illness(contact("a", "2013-01-04"), symptoms)
  expect_equal(res$bout_ili, "ILI")
  expect_equal(res$bout_ili_fever, "no ILI")
})

test_that("contacts outside follow-up are unknown, never healthy", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-10", "2013-01-20", "2013-01-30")),
    report("a", "2013-01-05", start = "2013-01-04", end = "2013-01-05"),
    report("a", "2013-02-03", start = "2013-02-02", end = "2013-02-03",
           ili = "t")
  ))
  days <- c("2013-01-04", "2013-01-09", "2013-01-10", "2013-01-30",
            "2013-01-31", "2013-02-02")
  res <- contact_illness(contact("a", days), symptoms)
  expect_equal(res$follow_up,
               c("before follow-up", "before follow-up", "in follow-up",
                 "in follow-up", "after follow-up", "after follow-up"))
  expect_equal(res$current_symptoms,
               c(NA, NA, "no symptoms", "no symptoms", NA, NA))
  expect_true(all(is.na(res$bout_ili[c(1, 2, 5, 6)])))
  expect_true(all(is.na(res$current_ari[c(1, 2, 5, 6)])))
  expect_equal(res$bout_ili[c(3, 4)], c("no ILI", "no ILI"))
})

test_that("ineligible participants are not classified", {
  symptoms <- data.table::rbindlist(list(
    ## only two days with reports
    well("few", c("2013-01-01", "2013-01-31")),
    ## enough reports but none without symptoms
    report("ill", "2013-01-01", start = "2013-01-01", option = "still_ill"),
    report("ill", "2013-01-08", start = "2013-01-01", option = "still_ill"),
    report("ill", "2013-01-15", start = "2013-01-01", option = "still_ill"),
    followed("ok"), well("ok", "2013-01-15")
  ))
  contacts <- contact(c("few", "ill", "ok", "unknown"), "2013-01-10")
  res <- contact_illness(contacts, symptoms)
  data.table::setkey(res, global_id)
  expect_equal(res[c("few", "ill", "ok", "unknown")]$follow_up,
               c("not eligible", "not eligible", "in follow-up",
                 "not eligible"))
  expect_equal(res[c("few", "ill", "ok", "unknown")]$current_symptoms,
               c(NA, NA, "no symptoms", NA))
  ## the threshold can be changed
  lenient <- contact_illness(contact("few", "2013-01-10"), symptoms,
                             min.reports = 2)
  expect_equal(lenient$current_symptoms, "no symptoms")
  expect_true(is.na(res["ill"]$bout_ili))
})

test_that("reports of ineligible days still reconstruct bouts", {
  ## the illness report is filed before follow-up starts but its interval
  ## reaches into follow-up
  symptoms <- data.table::rbindlist(list(
    report("a", "2013-01-12", start = "2013-01-01", end = "2013-01-12",
           ili = "t"),
    well("a", c("2013-01-05", "2013-01-31"))
  ))
  res <- contact_illness(contact("a", c("2013-01-02", "2013-01-06",
                                        "2013-01-20")), symptoms)
  expect_equal(res$follow_up,
               c("before follow-up", "in follow-up", "in follow-up"))
  expect_equal(res$current_symptoms, c(NA, "symptoms", "no symptoms"))
  expect_equal(res$bout_ili, c(NA, "ILI", "no ILI"))
})

test_that("eligibility and follow-up are set per season", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-05", "2013-01-15", "2013-01-25"),
         season = "2012/13"),
    ## second season: too few reports
    well("a", c("2014-01-05", "2014-01-25"), season = "2013/14")
  ))
  contacts <- contact("a", c("2013-01-10", "2013-01-01", "2014-01-10",
                             "2013-12-01"),
                      season = c("2012/13", "2012/13", "2013/14", "2013/14"))
  res <- contact_illness(contacts, symptoms)
  data.table::setkey(res, season, contact.date)
  expect_equal(res$follow_up,
               c("before follow-up", "in follow-up", "not eligible",
                 "not eligible"))
  ## a season's bouts do not carry into the next one
  carry <- data.table::rbindlist(list(
    well("a", c("2013-01-05", "2013-01-15", "2013-01-25"),
         season = "2012/13"),
    well("a", c("2013-03-05", "2013-03-15", "2013-03-25"),
         season = "2013/14"),
    report("a", "2013-01-20", start = "2013-01-18", end = "2013-01-19",
           season = "2012/13")
  ))
  res <- contact_illness(
    contact("a", c("2013-01-18", "2013-03-18"), season = c("2013/14", "2013/14")),
    carry
  )
  expect_equal(res$follow_up, c("before follow-up", "in follow-up"))
  expect_equal(res$current_symptoms, c(NA, "no symptoms"))
})

test_that("a symptom report without a usable interval leaves days unknown", {
  day <- as.Date("2013-01-17")
  bad <- list(
    report("a", day, start = day - 4, end = day - 5),
    report("b", day, start = day - 5, option = "dont_know"),
    report("c", day, start = NA, end = day),
    report("d", day, start = day + 1, end = day + 2)
  )
  symptoms <- data.table::rbindlist(c(
    lapply(seq_along(bad), function(i) {
      rbind(well(letters[i], c("2013-01-01", "2013-01-31")),
            well(letters[i], "2013-01-10"), bad[[i]])
    }),
    list(report("e", day, start = day - 5, option = "still_ill"),
         well("e", c("2013-01-01", "2013-01-10", "2013-01-31")))
  ))
  days <- c("2013-01-05", "2013-01-10", "2013-01-11", "2013-01-17",
            "2013-01-18")
  res <- contact_illness(
    contact(rep(letters[1:4], each = 5), rep(days, 4)), symptoms
  )
  data.table::setkey(res, global_id, contact.date)
  ## healthy up to the previous report, unknown from the day after it to the
  ## report date, healthy again after
  expect_equal(res$current_symptoms[1:5],
               c("no symptoms", "no symptoms", NA, NA, "no symptoms"))
  expect_equal(res$bout_ili[1:5],
               c("no ILI", "no ILI", NA, NA, "no ILI"))
  expect_equal(res$current_symptoms[6:20],
               rep(c("no symptoms", "no symptoms", NA, NA, "no symptoms"), 3))
  ## a still-ill report establishes its own day, but not recovery
  still <- contact_illness(contact("e", days), symptoms)
  expect_equal(still$current_symptoms,
               c("no symptoms", "no symptoms", "no symptoms", "symptoms", NA))
})

test_that("an unplaced report makes the flags of an overlapping bout unknown", {
  symptoms <- data.table::rbindlist(list(
    followed("a"), well("a", "2013-01-08"),
    report("a", "2013-01-12", start = "2013-01-10", end = "2013-01-12"),
    report("a", "2013-01-12", start = NA, ili = "t")
  ))
  res <- contact_illness(contact("a", c("2013-01-09", "2013-01-11")),
                         symptoms)
  expect_equal(res$current_symptoms, c(NA, "symptoms"))
  ## a positive report of the bout stays positive; only a negative is open
  expect_equal(res$bout_ili, c(NA_character_, "ILI"))
})

test_that("contacts are located by contact date, not report date", {
  ## contact reports are filed the day after the day they describe
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-10", start = "2013-01-09", end = "2013-01-10",
           ili = "t")
  ))
  contacts <- data.table::data.table(
    global_id = "a", date = as.Date(c("2013-01-09", "2013-01-12")),
    contact.date = as.Date(c("2013-01-08", "2013-01-10"))
  )
  res <- contact_illness(contacts, symptoms)
  ## filed on a day in the bout but describing a healthy day, and the reverse
  expect_equal(res$current_symptoms, c("no symptoms", "symptoms"))
})

test_that("a report without symptoms is no acute illness", {
  ## no acute symptoms reported on any day: no bout, healthy throughout
  symptoms <- rbind(followed("a"), well("a", "2013-01-15"))
  res <- contact_illness(contact("a", c("2013-01-02", "2013-01-15")),
                         symptoms)
  expect_equal(res$current_symptoms, rep("no symptoms", 2))
})

test_that("bouts from separate reports merge when they overlap or touch", {
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-03", start = "2013-01-01", end = "2013-01-03"),
    report("a", "2013-01-08", start = "2013-01-04", end = "2013-01-06"),
    report("a", "2013-01-20", start = "2013-01-15", end = "2013-01-16")
  ))
  res <- contact_illness(
    contact("a", c("2013-01-02", "2013-01-05", "2013-01-10", "2013-01-16")),
    symptoms
  )
  expect_equal(res$current_symptoms,
               c("symptoms", "symptoms", "no symptoms", "symptoms"))
})

test_that("same-day reports combine independently of row order", {
  day <- as.Date("2013-01-07")
  symptoms <- data.table::rbindlist(list(
    followed(letters[1:3]),
    report("a", day, start = day, option = "still_ill", ili = "t", ari = "t"),
    report("a", day, start = day, option = "still_ill", ili = "f", ari = "f"),
    report("b", day, start = day, option = "still_ill", ili = NA),
    report("b", day, start = day, option = "still_ill", ili = "f"),
    report("c", day, start = day, option = "still_ill")
  ))
  forward <- contact_illness(contact(letters[1:3], day), symptoms)
  backward <- contact_illness(contact(letters[1:3], day),
                              symptoms[nrow(symptoms):1])
  expect_equal(forward, backward)
  expect_equal(forward$current_ili, c("ILI", NA, "no ILI"))
  expect_equal(forward$current_ari, c("ARI", "no ARI", "no ARI"))
})

test_that("contact_illness does not match missing participant IDs or dates", {
  contacts <- data.table::data.table(
    global_id = c("a", NA, NA, "a"),
    contact.date = as.Date(c(NA, "2013-01-01", NA, "2013-01-01"))
  )
  symptoms <- rbind(
    followed("a"), well("a", "2013-01-15"),
    report(c("a", NA, NA, "a"), c(NA, "2013-01-01", NA, "2013-01-01"),
           start = "2013-01-01", option = "still_ill", ili = "t", ari = "t")
  )
  res <- contact_illness(contacts, symptoms)
  matched <- !is.na(res$global_id) & !is.na(res$contact.date)
  expect_equal(nrow(res), nrow(contacts))
  expect_true(all(is.na(res$current_symptoms[!matched])))
  expect_equal(res$current_symptoms[matched], "symptoms")
  expect_equal(res$current_ili[matched], "ILI")
})

test_that("extraction preserves same-day symptom reports for contact illness", {
  day <- as.Date("2013-01-07")
  reports <- data.table::data.table(
    global_id = "a", date = day, no.symptoms = c("f", "t"),
    ili.self = c("t", "f"),
    symptoms.start.date = day, symptoms.end.date = as.Date(NA),
    symptoms.end.option = "still_ill", fever = c("t", "f"),
    cough = c("t", "f")
  )
  symptoms <- extract_data(
    list(`2013` = list(symptom = reports)), surveys = "symptom",
    clean = c("unsuccessful.join", "remove.bad.symptom.dates")
  )
  expect_equal(nrow(symptoms), 2)
  res <- contact_illness(contact("a", day), symptoms, min.reports = 1)
  expect_equal(res$current_symptoms, "symptoms")
  expect_equal(res$current_ili_self, "ILI")
  expect_equal(res$current_ari, "ARI")
})

test_that("contact_illness uses symptom reports that cleaning removes", {
  contacts <- extract_data(
    simulated_data, years = 2013:2014, surveys = c("background", "contact")
  )
  contacts[, contact.date := date]
  symptoms <- extract_data(
    simulated_data, years = 2013:2014, surveys = "symptom",
    clean = c("unsuccessful.join", "remove.bad.symptom.dates")
  )
  cleaned <- extract_data(
    simulated_data, years = 2013:2014, surveys = "symptom"
  )
  expect_lt(nrow(cleaned), nrow(symptoms))
  res <- contact_illness(contacts, symptoms)
  expect_equal(nrow(res), nrow(contacts))
  res_cleaned <- contact_illness(contacts, cleaned)
  expect_gt(
    sum(!is.na(res$current_symptoms)),
    sum(!is.na(res_cleaned$current_symptoms))
  )
})

test_that("contact analysis cleans invalid dates without dropping reports", {
  clean <- c("unsuccessful.join", "remove.bad.symptom.dates")
  analysis_path <- test_path("..", "..", "analysis", "contacts.R")
  if (file.exists(analysis_path)) {
    expressions <- parse(analysis_path)
    assignment <- expressions[[which(vapply(expressions, function(x) {
      is.call(x) && identical(x[[1]], as.name("<-")) &&
        identical(x[[2]], as.name("dt_symptoms"))
    }, logical(1)))]]
    expect_equal(eval(assignment[[3]][["clean"]]), clean)
  }
  day <- as.Date("2013-01-17")
  reports <- rbind(
    well("a", c("2013-01-01", "2013-01-10", "2013-01-31")),
    report("a", day, start = day - 1, end = day + 2, ili = "t")
  )
  symptoms <- extract_data(list(`2013` = list(symptom = reports)),
                           surveys = "symptom", clean = clean)
  expect_equal(nrow(symptoms), 4)
  res <- contact_illness(contact("a", c(as.Date("2013-01-05"), day)), symptoms)
  ## cleaning clears the invalid end date, leaving no usable interval, so the
  ## days since the previous report are unknown rather than healthy
  expect_equal(res$current_symptoms, c("no symptoms", NA))
})

test_that("follow-up can end at the end of the last illness", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-10", "2013-01-20", "2013-01-30")),
    report("a", "2013-02-03", start = "2013-02-02", end = "2013-02-03",
           ili = "t")
  ))
  days <- c("2013-01-31", "2013-02-02", "2013-02-04")
  res <- contact_illness(contact("a", days), symptoms)
  expect_equal(res$follow_up, rep("after follow-up", 3))
  res <- contact_illness(contact("a", days), symptoms,
                         follow_up_end = "end_of_illness")
  expect_equal(res$follow_up,
               c("in follow-up", "in follow-up", "after follow-up"))
  expect_equal(res$current_symptoms, c("no symptoms", "symptoms", NA))
  expect_error(contact_illness(contact("a", days), symptoms,
                               follow_up_end = "never"))
})

test_that("a still-ill report ends follow-up at its date, not at a recovery", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-10", "2013-01-20", "2013-01-30")),
    report("a", "2013-02-03", start = "2013-02-02", option = "still_ill",
           ili = "t")
  ))
  days <- c("2013-02-03", "2013-02-04")
  res <- contact_illness(contact("a", days), symptoms,
                         follow_up_end = "end_of_illness")
  expect_equal(res$follow_up, c("in follow-up", "after follow-up"))
  expect_equal(res$current_symptoms, c("symptoms", NA))
})

test_that("follow-up uses established illness days from shared bouts", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-10", "2013-01-20", "2013-01-30")),
    report("a", "2013-02-03", option = "still_ill")
  ))
  days <- c("2013-02-03", "2013-02-04")
  res <- contact_illness(contact("a", days), symptoms,
                         follow_up_end = "end_of_illness")
  expect_equal(res$follow_up, c("in follow-up", "after follow-up"))
  expect_equal(res$current_symptoms, c("symptoms", NA))
})

test_that("chronic reports do not extend follow-up", {
  symptoms <- data.table::rbindlist(list(
    well("a", c("2013-01-01", "2013-01-10", "2013-01-20")),
    report("a", "2013-02-10", start = "2013-01-21", end = "2013-02-10")
  ))
  day <- contact("a", "2013-02-10")
  expect_equal(contact_illness(day, symptoms, follow_up_end = "end_of_illness")$follow_up,
               "in follow-up")
  expect_equal(contact_illness(day, symptoms, follow_up_end = "end_of_illness",
                              chronic = "duration", chronic.days = 14)$follow_up,
               "after follow-up")
})

test_that("long reports are treated as no acute illness only on request", {
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-20", start = "2013-01-02", end = "2013-01-20"),
    report("a", "2013-01-25", start = "2013-01-24", end = "2013-01-25")
  ))
  days <- as.Date(c("2013-01-10", "2013-01-24"))
  status <- function(...) {
    contact_illness(contact("a", days), symptoms, ...)$current_symptoms
  }
  expect_equal(status(), c("symptoms", "symptoms"))
  expect_equal(status(chronic = "duration", chronic.days = 14),
               c("no symptoms", "symptoms"))
  expect_equal(status(chronic = "duration", chronic.days = 30),
               c("symptoms", "symptoms"))
  expect_error(status(chronic = "duration"), "chronic.days")
  ## a chronic report covers the days before it like a healthy one
  expect_equal(
    status(chronic = "duration", chronic.days = 14, healthy = "report"),
    c("no symptoms", "symptoms")
  )
})

test_that("a chronic condition does not remove acute illness", {
  symptoms <- data.table::rbindlist(list(
    followed("a"),
    report("a", "2013-01-12", start = "2013-01-10", end = "2013-01-12")
  ))
  symptoms[, risk.asthma := "t"]
  day <- contact("a", as.Date("2013-01-11"))
  expect_equal(contact_illness(day, symptoms)$current_symptoms, "symptoms")
  expect_equal(
    contact_illness(day, symptoms, chronic = "duration",
                    chronic.days = 14)$current_symptoms,
    "symptoms"
  )
})

test_that("an unknown chronic option is rejected", {
  symptoms <- followed("a")
  expect_error(
    contact_illness(contact("a", as.Date("2013-01-11")), symptoms,
                    chronic = "condition"),
    "should be one of"
  )
})

test_that("healthy = 'report' needs a later report without symptoms", {
  symptoms <- rbind(
    well("a", c("2013-01-01", "2013-01-08", "2013-01-31")),
    report("a", "2013-01-15", start = "2013-01-12", end = "2013-01-13",
           ili = "t")
  )
  days <- c("2013-01-01", "2013-01-05", "2013-01-12", "2013-01-14",
            "2013-01-20", "2013-01-30")
  default <- contact_illness(contact("a", days), symptoms)
  expect_equal(
    default$current_symptoms,
    c("no symptoms", "no symptoms", "symptoms", "no symptoms", "no symptoms",
      "no symptoms")
  )
  res <- contact_illness(contact("a", days), symptoms, healthy = "report")
  expect_equal(
    res$current_symptoms,
    c("no symptoms", "no symptoms", "symptoms", NA, "no symptoms",
      "no symptoms")
  )
  expect_equal(res$bout_ili, c("no ILI", "no ILI", "ILI", NA, "no ILI",
                               "no ILI"))
  expect_error(contact_illness(contact("a", days), symptoms, healthy = "x"))
})

test_that("healthy = 'report' leaves days outside report coverage unknown", {
  symptoms <- well("a", c("2013-01-05", "2013-01-10", "2013-01-20"))
  days <- c("2013-01-04", "2013-01-05", "2013-01-19", "2013-01-20")
  res <- contact_illness(contact("a", days), symptoms, healthy = "report")
  expect_equal(res$current_symptoms,
               c(NA, "no symptoms", "no symptoms", NA))
})

test_that("neither healthy rule calls a still-ill day without onset healthy", {
  ## the report day is a day of illness; the days before it are unknown
  symptoms <- rbind(
    well("a", c("2013-01-01", "2013-01-08", "2013-01-31")),
    report("a", "2013-01-15", option = "still_ill", ili = "t")
  )
  days <- c("2013-01-09", "2013-01-14", "2013-01-15", "2013-01-20")
  for (rule in c("follow-up", "report")) {
    res <- contact_illness(contact("a", days), symptoms, healthy = rule)
    expect_equal(res$current_symptoms[2:3], c(NA_character_, "symptoms"))
  }
})
