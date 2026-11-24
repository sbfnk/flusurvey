## Simulated FluSurvey data for examples and tests.
##
## The script uses only the questionnaire definitions in the package
## (`questions` and `options`) and the postcode lookup tables. Every
## distribution below is an arbitrary choice, not an estimate from survey
## responses. The choices make sure that the cases the package handles occur
## often enough to be tested: bouts that run over several reports, reporting
## at irregular intervals, and symptom reports with missing dates.
##
## The answers are first written as one CSV file per questionnaire and season
## with the question codes as column names, which is the form read_data()
## expects. read_data() then turns them into the list that extract_data() takes.
##
## Run from the root of the repository.

library(data.table)
pkgload::load_all(here::here(), quiet = TRUE)

set.seed(20240101)

seasons <- c(2013L, 2014L)
n_participants <- 320L
return_probability <- 0.7
n_new_participants <- 80L

hh_groups <- c("0.4", "5.18", "19.44", "45.64", "65+")
contact_age_groups <- c("0-4", "5-18", "19-44", "45-64", "65+")

## ---- helpers ---------------------------------------------------------------

pick <- function(n, values, prob = NULL) {
  values[sample.int(length(values), n, replace = TRUE, prob = prob)]
}

## checkbox questions: one 0/1 column per answer, at least one ticked. If
## `exclusive` is given, that answer (e.g. "none") excludes all the others.
checkboxes <- function(n, probs, exclusive = NULL) {
  m <- matrix(as.integer(runif(n * length(probs)) < rep(probs, each = n)),
              nrow = n)
  if (!is.null(exclusive)) {
    m[rowSums(m) == 0, exclusive] <- 1L
    m[m[, exclusive] == 1L, -exclusive] <- 0L
  } else {
    none <- which(rowSums(m) == 0)
    m[cbind(none, sample.int(ncol(m), length(none), replace = TRUE))] <- 1L
  }
  colnames(m) <- names(probs)
  as.data.table(m)
}

date_string <- function(x) format(x, "%Y-%m-%d")

## NA where a question is not asked
when <- function(condition, value) ifelse(condition, value, NA)

how_soon <- function() {
  sample(0:7, 1, prob = c(0.3, 0.25, 0.15, 0.1, 0.05, 0.05, 0.05, 0.05))
}

## ---- participants ----------------------------------------------------------

## children are at school, over-65s retired, everyone else works or not
pick_main_activity <- function(age) {
  n <- length(age)
  working_age <- pick(n, c(0L, 1L, 2L, 4L, 5L, 8L),
                      c(0.5, 0.2, 0.1, 0.07, 0.07, 0.06))
  activity <- rep(7L, n)
  activity[age < 65] <- working_age[age < 65]
  activity[age < 18] <- 3L
  activity
}

make_people <- function(ids) {
  n <- length(ids)
  group <- pick(n, c("child", "adult", "older"), c(0.15, 0.7, 0.15))
  age <- integer(n)
  age[group == "child"] <- sample(0:17, sum(group == "child"), replace = TRUE)
  age[group == "adult"] <- sample(18:64, sum(group == "adult"), replace = TRUE)
  age[group == "older"] <- sample(65:85, sum(group == "older"), replace = TRUE)
  birth <- as.Date("2013-01-01") - age * 365.25 -
    sample(0:364, n, replace = TRUE)

  ## postcode districts that are in both lookup tables
  postcodes <- intersect(as.character(urban_rural$postcode),
                         as.character(regions$postcode))

  hh_lambda <- list(
    child = c(0.5, 0.9, 1.2, 0.3, 0.1),
    adult = c(0.3, 0.5, 0.8, 0.4, 0.15),
    older = c(0.05, 0.1, 0.3, 0.6, 0.5)
  )
  household <- t(vapply(group, function(g) rpois(5, hh_lambda[[g]]),
                        integer(5), USE.NAMES = FALSE))

  people <- data.table(
    global_id = sprintf("sim%04d", ids),
    age = age,
    birthmonth = format(birth, "%Y-%m"),
    gender = sample(0:1, n, replace = TRUE),
    postcode = pick(n, postcodes),
    work.postcode = pick(n, postcodes),
    main.activity = pick_main_activity(age),
    education = ifelse(
      age < 18, 5L, pick(n, 0:4, c(0.05, 0.25, 0.2, 0.3, 0.2))
    ),
    susceptibility = rgamma(n, shape = 4, rate = 4),
    baseline.health = pmin(100, round(rnorm(n, 88, 6))),
    vaccine.propensity = runif(n)
  )
  for (j in seq_along(hh_groups)) {
    people[[paste0("nb.household.", hh_groups[j])]] <- household[, j]
  }
  people
}

## ---- background questionnaire ----------------------------------------------

make_background <- function(people, join_date, season) {
  n <- nrow(people)
  working <- people$main.activity %in% c(0, 1, 2)
  vaccinated <-
    people$vaccine.propensity < ifelse(people$age >= 65, 0.7, 0.25)
  vacc_reason <- checkboxes(
    n, c(riskgroup = 0.3, protected = 0.5, protect.others = 0.3,
         given.at.school = 0.05, doctor = 0.2, work.recommended = 0.15,
         convenient = 0.2, free = 0.3, nomiss.work = 0.1, always = 0.2,
         other = 0.05)
  )
  no_vacc_reason <- checkboxes(
    n, c(notyet = 0.1, notoffered = 0.15, norisk = 0.3, natural = 0.1,
         noteffective = 0.1, minor = 0.15, unlikely = 0.1, cause = 0.03,
         side.effects = 0.05, dont.like = 0.1, unavailable = 0.05,
         not.free = 0.05, no.reason = 0.1, doctor = 0.02, other = 0.05)
  )
  checks <- list(
    frequent.contact = checkboxes(
      n,
      c(children = 0.3, elderly = 0.2, patients = 0.1, people = 0.3,
        none = 0.3),
      exclusive = 5
    ),
    risk = checkboxes(
      n,
      c(none = 0.7, asthma = 0.1, diabetes = 0.04, lung = 0.02,
        heart = 0.04, kidney = 0.01, immune = 0.02),
      exclusive = 1
    ),
    allergy = checkboxes(
      n,
      c(hayfever = 0.2, dust = 0.1, animals = 0.08, other = 0.05,
        none = 0.6),
      exclusive = 5
    ),
    diet = checkboxes(
      n,
      c(none = 0.8, vegetarian = 0.1, vegan = 0.02, `low-calorie` = 0.05,
        other = 0.03),
      exclusive = 1
    ),
    pets = checkboxes(
      n,
      c(none = 0.5, dogs = 0.25, cats = 0.25, birds = 0.03, other = 0.07),
      exclusive = 1
    ),
    howhear = checkboxes(
      n,
      c(radio.tv = 0.2, paper.magazine = 0.15, internet = 0.3,
        poster = 0.03, school.work = 0.1, bsa = 0.05, family.friends = 0.2,
        other = 0.05)
    )
  )
  education_levels <- c("no.education", "education.gcse", "education.alevels",
                        "education.bsc", "education.msc", "education.stillin")
  pregnant <- when(people$gender == 1 & people$age >= 18 & people$age < 45,
                   pick(n, 0:2, c(0.12, 0.86, 0.02)))

  res <- data.table(
    global_id = people$global_id,
    date = join_date,
    self = pick(n, 0:2, c(0.85, 0.1, 0.05)),
    gender = people$gender,
    birthmonth = people$birthmonth,
    postcode = people$postcode,
    main.activity = people$main.activity,
    work.postcode.option = when(working, 0L),
    work.postcode = when(working, people$work.postcode),
    occupation = when(working, pick(n, 0:5, c(0.3, 0.25, 0.1, 0.1, 0.1, 0.15))),
    education = people$education,
    children.school = pick(n, 0:6, c(0.6, 0.15, 0.12, 0.06, 0.03, 0.02, 0.02)),
    transport = pick(n, 0:5, c(0.15, 0.05, 0.02, 0.45, 0.28, 0.05)),
    howlong.transport = pick(n, 0:4, c(0.2, 0.3, 0.3, 0.15, 0.05)),
    howoften.flulike = pick(n, 0:5, c(0.2, 0.4, 0.2, 0.05, 0.02, 0.13)),
    vaccine.last.year = pick(n, 0:2, c(0.3, 0.65, 0.05)),
    vaccine.this.year = as.integer(!vaccinated),
    date.vaccine.option = when(vaccinated, pick(n, 0:1, c(0.2, 0.8))),
    date.vaccine = when(vaccinated,
                        date_string(as.Date(paste0(season - 1, "-09-15")) +
                                      sample(0:40, n, replace = TRUE))),
    smoke = pick(n, 0:4, c(0.75, 0.08, 0.1, 0.05, 0.02)),
    pregnant = pregnant,
    pregnant.trimester = when(!is.na(pregnant) & pregnant == 0,
                              sample(0:2, n, replace = TRUE)),
    howhear.who = pick(n, 0:3),
    activity.vigorous = pick(n, 0:3),
    activity.moderate = pick(n, 0:3),
    activity.winter = pick(n, 0:3)
  )

  for (group in hh_groups) {
    nb <- people[[paste0("nb.household.", group)]]
    res[[paste0("household.", group)]] <- as.integer(nb > 0)
    res[[paste0("nb.household.", group)]] <- ifelse(nb > 0, nb, NA)
  }
  for (j in seq_along(education_levels)) {
    res[[education_levels[j]]] <- as.integer(people$education == j - 1L)
  }
  for (prefix in names(checks)) {
    for (nm in names(checks[[prefix]])) {
      res[[paste(prefix, nm, sep = ".")]] <- checks[[prefix]][[nm]]
    }
  }
  for (nm in names(vacc_reason)) {
    res[[paste0("why.vaccine.", nm)]] <- when(vaccinated, vacc_reason[[nm]])
  }
  for (nm in names(no_vacc_reason)) {
    res[[paste0("why.not.vaccine.", nm)]] <-
      when(!vaccinated, no_vacc_reason[[nm]])
  }
  res
}

## ---- infections and symptom reports ----------------------------------------

## Episodes of three kinds (influenza-like, common cold, gastrointestinal) over
## the season. Influenza-like episodes follow an epidemic curve that peaks at a
## different time in each season; the others occur at a constant rate. Some
## episodes linger for weeks (a cough that does not clear), so that a bout
## runs over several weekly reports.
simulate_episodes <- function(days, peak, age, susceptibility) {
  epidemic <- dnorm(as.numeric(days - peak), sd = 18) / dnorm(0, sd = 18)
  child <- if (age < 18) 2 else 1
  p_ili <- 0.015 * epidemic * susceptibility * child
  p_cold <- 0.004 * child
  p_gi <- 0.0007
  episodes <- list()
  d <- 1L
  while (d <= length(days)) {
    u <- runif(1)
    type <- NA_character_
    if (u < p_ili[d]) {
      type <- "ili"
    } else if (u < p_ili[d] + p_cold) {
      type <- "cold"
    } else if (u < p_ili[d] + p_cold + p_gi) {
      type <- "gi"
    }
    if (is.na(type)) {
      d <- d + 1L
      next
    }
    duration <- switch(type, ili = sample(4:14, 1), cold = sample(3:14, 1),
                       gi = sample.int(4, 1))
    if (type != "gi" && runif(1) < 0.1) duration <- duration + sample(7:28, 1)
    episodes[[length(episodes) + 1L]] <-
      list(type = type, start = days[d], end = days[d] + duration - 1L)
    d <- d + duration + 1L
  }
  episodes
}

symptom_probabilities <- list(
  ili = c(fever = 0.65, chills = 0.5, blocked.runny.nose = 0.5, sneezing = 0.3,
          sore.throat = 0.6, cough = 0.85, shortness.breath = 0.25,
          headache = 0.6, muscle.and.or.joint.pain = 0.6, chest.pain = 0.15,
          tired = 0.8, loss.appetite = 0.5, phlegm = 0.35, watery.eyes = 0.2,
          nausea = 0.1, vomiting = 0.03, diarrhoea = 0.05, stomach.ache = 0.05,
          other.symptoms = 0.05),
  cold = c(fever = 0.05, chills = 0.1, blocked.runny.nose = 0.9, sneezing = 0.7,
           sore.throat = 0.6, cough = 0.5, shortness.breath = 0.03,
           headache = 0.25, muscle.and.or.joint.pain = 0.15, chest.pain = 0.03,
           tired = 0.3, loss.appetite = 0.1, phlegm = 0.3, watery.eyes = 0.3,
           nausea = 0.02, vomiting = 0, diarrhoea = 0.02, stomach.ache = 0.02,
           other.symptoms = 0.05),
  gi = c(fever = 0.1, chills = 0.1, blocked.runny.nose = 0, sneezing = 0,
         sore.throat = 0, cough = 0.02, shortness.breath = 0,
         headache = 0.3, muscle.and.or.joint.pain = 0.2, chest.pain = 0,
         tired = 0.5, loss.appetite = 0.6, phlegm = 0, watery.eyes = 0,
         nausea = 0.8, vomiting = 0.7, diarrhoea = 0.8, stomach.ache = 0.7,
         other.symptoms = 0.05)
)
symptom_names <- names(symptom_probabilities$ili)

draw_symptoms <- function(type) {
  p <- symptom_probabilities[[type]]
  s <- as.integer(runif(length(p)) < p)
  names(s) <- names(p)
  ## an episode always involves at least one symptom
  if (sum(s) == 0) s[which.max(p)] <- 1L
  s
}

## Mostly weekly report dates for one participant: some weeks are missed,
## some reports come a few days after the previous one, some after a break of
## several weeks, and participants drop out at random.
make_report_dates <- function(join_date, end_date) {
  dates <- as.Date(character())
  d <- join_date
  while (d <= end_date) {
    if (length(dates) == 0 || runif(1) > 0.15) dates <- c(dates, d)
    if (runif(1) > 0.94) break
    d <- d + sample(c(3L, 6L, 7L, 8L, 14L, 21L), 1,
                    prob = c(0.05, 0.1, 0.6, 0.1, 0.1, 0.05))
  }
  unique(dates)
}

## The answers to the questions that are only asked at the first report of an
## episode of symptoms
first_report_answers <- function(ans, e, symptom_profile) {
  known_start <- runif(1) < 0.85
  ans$symptoms.start.option <- if (known_start) 0L else 1L
  if (known_start) ans$symptoms.start.date <- date_string(e$start)
  sudden <- c(ili = 0.8, cold = 0.3, gi = 0.6)[[e$type]]
  ans$symptoms.suddenly <- if (runif(1) < sudden) 0L else 1L
  if (symptom_profile[["fever"]] == 1L) {
    ans$fever.start.option <- 1L
    ans$fever.start.date <- date_string(e$start + sample(0:1, 1))
    ans$fever.suddenly <- if (runif(1) < 0.8) 0L else 1L
    ans$fever.temperature <- pick(1, 0:2, c(0.6, 0.35, 0.05))
    if (ans$fever.temperature == 0L) {
      ans$fever.temperature.value <-
        pick(1, 1:5, c(0.1, 0.25, 0.45, 0.15, 0.05))
    }
  }

  visit <- runif(1) < c(ili = 0.3, cold = 0.1, gi = 0.15)[[e$type]]
  ans$visit.medical.service.no <- as.integer(!visit)
  ans$visit.medical.service.gp <- as.integer(visit && runif(1) < 0.8)
  ans$visit.medical.service.ae <- as.integer(visit && runif(1) < 0.1)
  ans$visit.medical.service.hospital <- as.integer(visit && runif(1) < 0.05)
  ans$visit.medical.service.other <- as.integer(visit && runif(1) < 0.05)
  ans$visit.medical.service.appointment <- as.integer(visit && runif(1) < 0.3)
  if (visit) {
    ans$visit.medical.service.howsoon.gp.receptionist <- how_soon()
    ans$visit.medical.service.howsoon.gp.doctor.nurse <- how_soon()
    ans$visit.medical.service.howsoon.nhs <- how_soon()
    ans$visit.medical.service.howsoon.other <- how_soon()
  }
  contact <- !visit && runif(1) < 0.15
  ans$contact.medical.service.no <- as.integer(!(visit || contact))
  ans$contact.medical.service.gp.receptionist <-
    as.integer(contact && runif(1) < 0.4)
  ans$contact.medical.service.gp.doctor <- as.integer(contact && runif(1) < 0.3)
  ans$contact.medical.service.nhs <- as.integer(contact && runif(1) < 0.3)
  ans$contact.medical.service.npfs <- as.integer(contact && runif(1) < 0.1)
  ans$contact.medical.service.other <- as.integer(contact && runif(1) < 0.1)
  if (contact) {
    ans$contact.medical.service.howsoon.gp.receptionist <- how_soon()
    ans$contact.medical.service.howsoon.gp.doctor.nurse <- how_soon()
    ans$contact.medical.service.howsoon.nhs <- how_soon()
    ans$contact.medical.service.howsoon.other <- how_soon()
  }

  meds <- runif(5) < c(0.6, 0.35, 0.05, 0.06, 0.07)
  ans$no.medication <- as.integer(!any(meds))
  ans$medication.painkillers <- as.integer(meds[1])
  ans$medication.cough <- as.integer(meds[2])
  ans$medication.antiviral <- as.integer(meds[3])
  ans$medication.antibiotic <- as.integer(meds[4])
  ans$medication.other <- as.integer(meds[5])
  ans$medication.dontknow <- as.integer(runif(1) < 0.02)
  if (any(meds)) ans$medication.howsoon <- how_soon()
  ans
}

make_symptom_rows <- function(person, dates, episodes) {
  rows <- vector("list", length(dates))
  current <- 0L
  for (i in seq_along(dates)) {
    d <- dates[i]
    ## symptoms since the previous report (or today, for the first)
    prev <- if (i == 1L) d else dates[i - 1L]
    ep <- 0L
    for (k in seq_along(episodes)) {
      if (episodes[[k]]$start <= d && episodes[[k]]$end >= prev) {
        ep <- k
        break
      }
    }
    ans <- list(global_id = person$global_id, date = d)
    ans[symptom_names] <- 0L
    health <- person$baseline.health + round(rnorm(1, 0, 3))
    if (ep == 0L) {
      ans$no.symptoms <- 1L
      current <- 0L
    } else {
      e <- episodes[[ep]]
      first <- ep != current
      current <- ep
      ans$no.symptoms <- 0L
      if (first) {
        episodes[[ep]]$profile <- draw_symptoms(e$type)
      }
      symptom_profile <- episodes[[ep]]$profile
      ans[names(symptom_profile)] <- as.list(symptom_profile)
      ans$same <- if (first) 1L else 0L
      if (first) {
        ans <- first_report_answers(ans, e, symptom_profile)
      } else if (runif(1) < 0.85) {
        ## the start answer is filled in again from the first report
        ans[names(episodes[[ep]]$start_answer)] <- episodes[[ep]]$start_answer
      } else {
        ans$symptoms.start.option <- 1L
      }

      if (e$end <= d) {
        ans$symptoms.end.option <- 0L
        ans$symptoms.end.date <- date_string(e$end)
      } else {
        ans$symptoms.end.option <- if (runif(1) < 0.8) 2L else 1L
      }
      ## occasional start dates after the report, for the cleaning to deal with
      if (first && runif(1) < 0.02) {
        ans$symptoms.start.option <- 0L
        ans$symptoms.start.date <- date_string(d + sample.int(5, 1))
      }
      if (first) {
        episodes[[ep]]$start_answer <-
          ans[intersect(c("symptoms.start.option", "symptoms.start.date"),
                        names(ans))]
      }

      health <- health - round(runif(1, 10, 45))
      ans$alter.routine <- pick(1, 0:2, c(0.35, 0.3, 0.35))
      if (ans$alter.routine > 0L) {
        ans$still.altered <- if (e$end <= d) 1L else 0L
        ans$howlong.altered <-
          sample(0:7, 1, prob = c(0.2, 0.2, 0.2, 0.1, 0.1, 0.1, 0.07, 0.03))
      }
      ans$what.do.you.think <- switch(
        e$type,
        ili = pick(1, c(0L, 1L, 5L), c(0.6, 0.3, 0.1)),
        cold = pick(1, c(1L, 2L, 5L, 0L), c(0.7, 0.1, 0.1, 0.1)),
        gi = pick(1, c(3L, 4L, 5L), c(0.8, 0.1, 0.1))
      )
    }
    ans$health.score <- as.integer(max(0, min(100, health)))
    rows[[i]] <- ans
  }
  rbindlist(rows, fill = TRUE)
}

## ---- contact questionnaire --------------------------------------------------

make_contact_rows <- function(person, dates, background_row) {
  hh <- unlist(background_row[, paste0("nb.household.", hh_groups),
                              with = FALSE])
  hh[is.na(hh)] <- 0
  works <- person$main.activity %in% c(0, 1, 2) || person$age < 18
  group_weights <- c(0.1, 0.2, 0.4, 0.2, 0.1)
  settings <- c("home", "work", "other")
  rows <- vector("list", length(dates))
  for (i in seq_along(dates)) {
    ans <- list(global_id = person$global_id, date = dates[i])
    for (j in seq_along(contact_age_groups)) {
      conversational <- c(
        home = rpois(1, hh[[j]] + 0.5),
        work = rpois(1, (if (works) 6 else 0.5) * group_weights[j] + 0.5),
        other = rpois(1, 3 * group_weights[j] + 0.5)
      )
      physical <- rbinom(3, conversational, 0.5)
      for (s in seq_along(settings)) {
        values <- list(conversational = conversational[[s]],
                       physical = physical[s])
        for (type in names(values)) {
          value <- values[[type]]
          ## large numbers are sometimes given as a range
          if (value >= 5 && runif(1) < 0.3) {
            value <- paste0(value, "-", value + sample(2:5, 1))
          }
          ans[[paste(type, settings[s], contact_age_groups[j], sep = ".")]] <-
            value
        }
      }
    }
    ans$public.transport <- pick(1, 0:4, c(0.5, 0.2, 0.15, 0.1, 0.05))
    ans$enclosed.indoor.space <- pick(1, 0:4, c(0.3, 0.2, 0.2, 0.2, 0.1))
    ans$furthest.travelled <- pick(1, 0:5, c(0.2, 0.2, 0.2, 0.2, 0.15, 0.05))
    rows[[i]] <- ans
  }
  rbindlist(rows, fill = TRUE)
}

## ---- cases for regression tests -------------------------------------------

## Set `col` in rows `i` of `dt`, keeping the type of the column
set_value <- function(dt, i, col, value) {
  if (is.character(dt[[col]])) value <- as.character(value)
  set(dt, i, col, value)
}

## Make the reports in rows `i` of the symptom table symptomatic, with the
## given answers to the questions on whether it is the same illness and on the
## start of the symptoms
make_symptomatic <- function(symptom, i, same, start_date = NA_character_) {
  for (x in symptom_names) set(symptom, i, x, 0L)
  set(symptom, i, "cough", 1L)
  set(symptom, i, "tired", 1L)
  set(symptom, i, "no.symptoms", 0L)
  set(symptom, i, "same", same)
  set(symptom, i, "symptoms.start.option", if (is.na(start_date)) 1L else 0L)
  set(symptom, i, "symptoms.start.date", start_date)
  set(symptom, i, "symptoms.end.option", 2L)
}

## Participants and reports that exercise behaviour the functions handle badly
## or have to handle. They are few. They are changed after the ordinary
## answers have been simulated, to keep the rest of the data plausible.
## Participants are drawn without replacement so that each case is the only
## unusual thing about its participants.
add_edge_cases <- function(background, contact, symptom) {
  ## participants with at least five reports and no symptoms, whose reports
  ## can be changed one by one
  reports <- symptom[, list(n = .N, ill = any(no.symptoms == 0)),
                     by = global_id]
  symptom_free <- reports[reports$n >= 5 & !reports$ill, global_id]
  contact_counts <- contact[, .N, by = global_id]
  ## participants with at least three contact reports
  with_contacts <- contact_counts[N >= 3, global_id]
  chosen <- sample(symptom_free, 8L)
  without_background <- sample(contact_counts$global_id, 3L)
  late_contact <- sample(setdiff(with_contacts, c(chosen, without_background)),
                         3L)

  ## health scores below 0 and above 100, outside the range of the scale
  rows <- sample(setdiff(seq_len(nrow(symptom)),
                         which(symptom$global_id %in% chosen)), 6L)
  set(symptom, rows, "health.score", c(-5L, -20L, -1L, 101L, 120L, 150L))

  ## an age band ticked as present in the household with no number given
  ## (the band is `household.<group>` and the number `nb.household.<group>`)
  present <- which(background$household.19.44 == 1L)
  set(background, sample(present, 4L), "nb.household.19.44", NA_integer_)

  ## the first symptomatic report of a participant says that the symptoms are
  ## the same as in an earlier report ("yes", coded 0, or "dont_know", coded 2)
  ## with no start date, though there is no earlier report with symptoms, and
  ## a report without symptoms comes later in the season. This exercises how
  ## bouts_of_illness() starts a bout when there is nothing to continue
  for (k in 1:3) {
    i <- which(symptom$global_id == chosen[k])
    make_symptomatic(symptom, i[2], same = c(0L, 0L, 2L)[k])
  }

  ## a symptomatic report that follows another one and gives a start date
  ## after the date of the previous report, with the symptoms reported as the
  ## same illness. This exercises how bouts_of_illness() treats a start date
  ## that cannot belong to the earlier report
  for (k in 4:5) {
    i <- which(symptom$global_id == chosen[k])
    make_symptomatic(symptom, i[2], same = 1L,
                     start_date = date_string(symptom$date[i[2]] - 3L))
    make_symptomatic(symptom, i[3], same = 0L,
                     start_date = date_string(symptom$date[i[2]] + 2L))
  }

  ## symptom start dates months or years before the report, which
  ## bouts_of_illness() and get_incidence() take at face value
  for (k in 6:8) {
    i <- which(symptom$global_id == chosen[k])[2]
    make_symptomatic(
      symptom, i, same = 1L,
      start_date = date_string(symptom$date[i] - c(90L, 400L, 1000L)[k - 5L])
    )
  }

  ## contact reports with more physical than conversational contacts, though
  ## a physical contact is also a conversational one
  columns <- c("home", "work", "other")
  for (k in 1:5) {
    i <- sample(nrow(contact), 1L)
    col_conversational <- paste("conversational", columns[k %% 3L + 1L],
                                "19-44", sep = ".")
    col_physical <- sub("^conversational", "physical", col_conversational)
    set_value(contact, i, col_conversational, 1L)
    set_value(contact, i, col_physical, 4L)
  }

  ## contact reports from participants with no background report, which
  ## merge_data() has to join to nothing
  background <- background[!global_id %in% without_background]

  ## symptom reports before the first contact report of a participant: the
  ## first two contact reports are dropped
  late <- contact[global_id %in% late_contact,
                  .I[order(date)[1:2]], by = global_id]$V1
  contact <- contact[-late]

  list(background = background, contact = contact, symptom = symptom)
}

## ---- one season -------------------------------------------------------------

simulate_season <- function(people, season) {
  first_day <- as.Date(paste0(season - 1, "-11-01"))
  last_day <- as.Date(paste0(season, "-04-10"))
  days <- seq(first_day, last_day, by = "day")
  peak <- if (season == 2013L) as.Date("2013-01-06") else as.Date("2014-02-16")

  n <- nrow(people)
  ## Many participants join on the day the season is launched, and the rest
  ## spread over the following weeks. merge_data() takes the date with the most
  ## reports to define the season and fails if two dates tie, so the launch day
  ## has to be the clear maximum.
  launch_day <- runif(n) < 0.3
  soon <- runif(n) < 0.7
  early <- sample.int(20, n, replace = TRUE)
  later <- sample(21:100, n, replace = TRUE)
  delay <- early
  delay[!soon] <- later[!soon]
  delay[launch_day] <- 0L
  join_dates <- first_day + 4L + delay

  background <- make_background(people, join_dates, season)

  symptom <- vector("list", n)
  contact <- vector("list", n)
  for (i in seq_len(n)) {
    participant <- people[i]
    dates <- make_report_dates(join_dates[i], last_day)
    episodes <- simulate_episodes(days, peak, participant$age,
                                  participant$susceptibility)
    symptom[[i]] <- make_symptom_rows(participant, dates, episodes)
    ## contacts are reported at the first two reports and now and again later
    keep <- union(seq_len(min(2L, length(dates))),
                  which(runif(length(dates)) < 0.15))
    contact[[i]] <- make_contact_rows(participant, dates[sort(keep)],
                                      background[i])
  }

  ## the order matters: merge_data() joins the last questionnaire onto the
  ## earlier ones, so the symptom reports must come last
  add_edge_cases(background, rbindlist(contact, fill = TRUE),
                 rbindlist(symptom, fill = TRUE))
}

## ---- write answers as CSV with question codes, then read them back ----------

write_raw <- function(tables, season, dir) {
  files <- list()
  for (survey in names(tables)) {
    dt <- copy(tables[[survey]])
    seconds <- sample((8 * 3600):(21 * 3600 - 1), nrow(dt), replace = TRUE)
    dt[, timestamp := format(as.POSIXct(date) + seconds,
                             "%Y-%m-%d %H:%M:%S", tz = "UTC")]
    dt[, date := NULL]
    dt[, id := seq_len(.N)]

    ## the question codes are the column names in the files read_data() expects
    codes <- questions[[as.character(season)]][[survey]]
    absent <- setdiff(codes, colnames(dt))
    if (length(absent) > 0) {
      stop("No simulated answers for: ", toString(absent),
           " (", survey, ", ", season, ")", call. = FALSE)
    }
    ## read_data() drops columns without any answers and then cannot rename them
    empty <- codes[vapply(codes, function(x) all(is.na(dt[[x]])), TRUE)]
    if (length(empty) > 0) {
      stop("No answers at all for: ", toString(empty),
           " (", survey, ", ", season, ")", call. = FALSE)
    }
    out <- dt[, c("id", "global_id", "timestamp", codes), with = FALSE]
    setnames(out, codes, names(codes))
    ## read_data() turns columns that only hold 0 and 1 into factors
    ## ("f"/"t"), which is wrong for counts
    for (x in intersect(c(grep("^nb\\.", codes, value = TRUE),
                          grep("^(conversational|physical)\\.", codes,
                               value = TRUE)),
                        codes)) {
      values <- unique(na.omit(out[[names(codes)[codes == x]]]))
      if (length(setdiff(values, c(0, 1))) == 0) {
        stop("Counts in '", x, "' are only 0 and 1 (", survey, ", ", season,
             ")", call. = FALSE)
      }
    }
    path <- file.path(dir, paste0(survey, "_", season - 2000, ".csv"))
    write.csv(out, path, row.names = FALSE, na = "")
    files[[survey]] <- path
  }
  files
}

dir <- tempfile("flusurvey_simulated")
dir.create(dir)

people <- make_people(seq_len(n_participants))
next_id <- n_participants
simulated_data <- list()
for (season in seasons) {
  tables <- simulate_season(people, season)
  files <- write_raw(tables, season, dir)
  simulated_data[[as.character(season)]] <-
    read_data(files = files, year = season)

  ## some participants return the next season, and some are new
  returning <- people[runif(nrow(people)) < return_probability]
  new <- make_people(next_id + seq_len(n_new_participants))
  next_id <- next_id + n_new_participants
  people <- rbind(returning, new)
}

save(simulated_data, file = here::here("data", "simulated_data.rda"),
     compress = "xz")
