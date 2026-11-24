#' Illness status of contact reports
##'
##' Classifies the day described by each contact report (\code{contact.date},
##' the day before the report) as ill, healthy or unknown, following a
##' longitudinal model of each participant in each season. A symptom report is
##' not needed on the contact day itself.
##'
##' \emph{Eligibility and follow-up.} A participant is eligible in a season if
##' they filed symptom reports on at least \code{min.reports} different days
##' and at least one of them reported no symptoms. Follow-up runs from the
##' first to the last report without symptoms. Bouts are reconstructed from all
##' symptom reports, including those of ineligible participants and those
##' outside follow-up; eligibility and follow-up only decide which contact days
##' are classified. The column \code{follow_up} records the result:
##' "in follow-up", "before follow-up", "after follow-up" or "not eligible".
##' Contact days outside follow-up have an unknown status and are never
##' called healthy.
##'
##' \emph{Bouts.} Bouts and the days of illness come from
##' \code{\link{bout_evidence}}, the same reconstruction as in
##' \code{\link{bouts_of_illness}}: the answer to whether the symptoms are the
##' same as at the previous report, the dates of onset and recovery, and
##' intervening reports without symptoms decide which reports form a bout. A
##' report with a start and an end date (the end counted no later than the
##' report) supports the days between them. A report of continuing illness
##' supports the days from its start date to the report date, or only the report
##' date when it gives no start date: the onset is then unknown and the days
##' before it are unknown, but the report date is a day of illness, and a
##' contact describing it is classified as ill. Recovery on that date, or
##' illness on the following one, is not established. Days between the
##' intervals of a bout are not called ill. A symptomatic report that belongs
##' to no bout forms a bout of its own.
##'
##' \emph{Case definitions of bouts.} The symptoms of all reports of a bout are
##' combined first and the definitions applied to the result, so symptoms
##' reported in different questionnaires of one bout can jointly meet a
##' definition (see \code{\link{bout_evidence}}). Where the data lack the
##' symptom columns a definition is built from, it is positive if any report of
##' the bout is. Two adjacent illnesses that the answers separate keep their
##' own definitions.
##'
##' \emph{Daily status.} Within follow-up, \code{current_symptoms} is
##' "symptoms" on days in a bout and "no symptoms" on every other day, with or
##' without a report that day. The exception is a report with symptoms (or
##' without a known symptom status) whose dates give no usable interval:
##' missing, inconsistent, or an unknown end that is not "still ill". Its
##' illness cannot be placed, so every day since the participant's previous
##' report, up to and including its own date, is unknown (missing) unless
##' it lies in a bout. The same holds for the days before the report date of a
##' continuing illness without a start date. A report without symptoms is taken
##' as no acute illness. The data hold no indicator of chronic symptoms and the
##' maintainers have not defined one, or a duration after which symptoms count
##' as chronic.
##' No rule for chronic symptoms is applied.
##'
##' The case-definition flags are known per report, not per day. A bout meeting
##' a definition at some point need not have had every symptom present on
##' every day of it. The \code{bout_} columns therefore describe the whole
##' bout and are the ones for bout-based exposure: positive on every day of a
##' bout that meets the definition on the combined symptoms of its reports,
##' negative if it does not and no report is unresolved, and missing otherwise.
##' A positive bout stays positive when one of its reports is unresolved. The
##' daily \code{current_} columns are positive or negative within a bout only
##' on days with a symptom report whose interval includes that day, assuming
##' that such a report describes its own day, and are missing on other days
##' of a bout:
##' the symptoms of those days are not known, but the participant is known to
##' be ill. Several reports on a day are combined: positive if any is
##' positive, otherwise missing if any is unknown. Outside bouts, within
##' follow-up and where status is not unknown, both are negative. The four
##' definitions (symptom-based ILI, with fever, self-assessed ILI and ARI) stay
##' separate.
##'
##' Read the symptom data with a \code{clean} argument in
##' \code{\link{extract_data}} that cannot drop symptom reports, as the default
##' cleaning (e.g., \code{"remove.first"}) can remove reports that support a
##' bout. Use \code{clean = c("unsuccessful.join",
##' "remove.bad.symptom.dates")} to clear invalid symptom dates without
##' dropping reports when reading a single survey. Cleared dates leave the
##' report without a usable interval, which is handled as above. If both
##' tables have a \code{season} column, eligibility, follow-up and bouts are
##' determined per participant and season.
##' @param contacts a data table of contact reports with columns
##' \code{global_id} and \code{contact.date}, and \code{season} if the
##' symptom reports have one
##' @param symptoms a data table of symptom reports, as returned by
##' \code{\link{extract_data}}, with columns \code{global_id}, \code{date},
##' \code{no.symptoms}, \code{symptoms.start.date}, \code{symptoms.end.date}
##' and \code{symptoms.end.option}. The columns \code{ili} (symptom-based
##' ILI), \code{ili.fever} (symptom-based ILI with fever), \code{ili.self}
##' (self-assessed ILI) and \code{resp.symptoms} (ARI) are used where present;
##' a missing column gives missing flags. The column \code{same} (the answer to
##' whether the symptoms are the same as in the previous report) is used where
##' present, and the symptom columns that \code{\link{aggregate_symptoms}}
##' derives the definitions from, so that bouts can be reconstructed and
##' classified as in \code{\link{bouts_of_illness}}.
##' @param min.reports the minimum number of days with symptom reports for a
##' participant to be eligible in a season
##' @return the contact reports with added columns \code{follow_up} (see
##' above), \code{current_symptoms}
##' ("symptoms" or "no symptoms"), and a daily column \code{current_*} and a
##' bout column \code{bout_*} for each of \code{ili} (symptom-based),
##' \code{ili_fever} (symptom-based with fever), \code{ili_self}
##' (self-assessed), all "ILI" or "no ILI", and \code{ari} ("ARI" or
##' "no ARI"). Each is missing where the information is lacking
##' @examples
##' contacts <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = c("background", "contact")
##' )
##' symptoms <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = "symptom",
##'   clean = c("unsuccessful.join", "remove.bad.symptom.dates")
##' )
##' table(contact_illness(contacts, symptoms)$current_symptoms, useNA = "ifany")
##' @import data.table
##' @export
contact_illness <- function(contacts, symptoms, min.reports = 3) {
  flag_cols <- c(
    ili = "ili", ili_fever = "ili.fever", ili_self = "ili.self",
    ari = "resp.symptoms"
  )
  flag_labels <- list(
    ili = c("ILI", "no ILI"), ili_fever = c("ILI", "no ILI"),
    ili_self = c("ILI", "no ILI"), ari = c("ARI", "no ARI")
  )
  contacts <- data.table(contacts)
  symptoms <- data.table(symptoms)
  ## participants are followed up per season where both tables have one
  group_by <- "global_id"
  if ("season" %in% colnames(contacts) && "season" %in% colnames(symptoms)) {
    group_by <- c(group_by, "season")
  }
  has_key <- function(x) {
    Reduce(`&`, lapply(x[, group_by, with = FALSE], function(col) !is.na(col)))
  }
  symptoms <- symptoms[has_key(symptoms) & !is.na(date)]
  for (col in flag_cols) {
    if (!col %in% colnames(symptoms)) symptoms[, (col) := NA_character_]
  }
  symptoms[, row_id := seq_len(.N)]
  symptoms[, healthy := as.character(no.symptoms) == "t"]
  symptoms[is.na(healthy), healthy := FALSE]

  ## bouts and the days they establish come from the shared reconstruction
  work <- copy(symptoms)
  work[, participant_id := .GRP, by = group_by]
  ids <- unique(work[, c("participant_id", group_by), with = FALSE])
  work[, season := if ("season" %in% group_by) season else "all"]
  work[, no.symptoms := factor(as.character(no.symptoms), levels = c("f", "t"))]
  work[, same := if ("same" %in% colnames(work)) {
    factor(as.character(same), levels = c("yes", "no", "dont_know"))
  } else {
    factor(NA_character_, levels = c("yes", "no", "dont_know"))
  }]
  work <- reconstruct_bouts(work, singletons = TRUE)
  evidence <- bout_evidence_from(work)
  usable <- evidence$reports[basis != "unresolved"]
  symptoms[, basis := usable$basis[match(row_id, usable$row_id)]]
  symptoms[, valid := !is.na(basis)]

  ## eligibility and follow-up: first to last report without symptoms
  follow_up <- symptoms[, list(
    n_reports = uniqueN(date),
    fu_start = if (any(healthy)) min(date[healthy]) else as.Date(NA),
    fu_end = if (any(healthy)) max(date[healthy]) else as.Date(NA)
  ), by = group_by]
  follow_up[, eligible := n_reports >= min.reports & !is.na(fu_start)]

  ## days of illness with the case definitions of their bout
  bout_cols <- paste0("bout_", names(flag_cols))
  bout_flag_values <- copy(evidence$bouts[
    , c("participant_id", "season", "bout", unname(flag_cols)), with = FALSE
  ])
  setnames(bout_flag_values, unname(flag_cols), bout_cols)
  bout_days <- merge(evidence$days, bout_flag_values,
                     by = c("participant_id", "season", "bout"))
  bout_days[, season := NULL]
  bout_days <- merge(bout_days, ids, by = "participant_id")
  bout_days[, c("participant_id", "bout") := NULL]
  setnames(bout_days, "day", "contact.date")

  ## a report that cannot be placed in time leaves every day since the
  ## previous report unknown
  setorderv(symptoms, c(group_by, "date"))
  symptoms[, previous_date := c(as.Date(NA), unique(date))[
    match(date, unique(date))
  ], by = group_by]
  ## A continuing illness without a start date places only the report day:
  ## the days before it are unknown, as the onset is.
  unresolved <- symptoms[
    !healthy & (!valid | basis == "still.ill.unknown.onset"),
    list(unknown_start = fifelse(is.na(previous_date), date, previous_date + 1),
         unknown_end = fifelse(valid, date - 1, date)),
    by = group_by
  ]

  key_cols <- c(group_by, "contact.date")
  keys <- unique(contacts[has_key(contacts) & !is.na(contact.date),
                          key_cols, with = FALSE])
  keys <- merge(keys, follow_up[, c(group_by, "eligible", "fu_start", "fu_end"),
                                with = FALSE], by = group_by, all.x = TRUE)
  keys[, follow_up := fcase(
    is.na(eligible) | !eligible, "not eligible",
    contact.date < fu_start, "before follow-up",
    contact.date > fu_end, "after follow-up",
    default = "in follow-up"
  )]

  ## a contact day in two bouts takes the combined flags of both
  in_bout <- merge(keys[, key_cols, with = FALSE], bout_days, by = key_cols)
  in_bout <- in_bout[, c(list(bouted = TRUE), lapply(.SD, combine_flags)),
                     by = key_cols, .SDcols = bout_cols]

  unknown <- merge(keys[, key_cols, with = FALSE], unresolved,
                   by = group_by, allow.cartesian = TRUE)
  unknown <- unique(unknown[
    contact.date >= unknown_start & contact.date <= unknown_end,
    key_cols, with = FALSE
  ])
  unknown[, unresolved := TRUE]

  ## reports of the contact day whose interval includes that day
  same_day <- merge(
    symptoms[, c("row_id", group_by, "date", unname(flag_cols)), with = FALSE],
    usable[first.day <= date & last.day >= date, "row_id", with = FALSE],
    by = "row_id"
  )[, lapply(.SD, combine_flags), by = c(group_by, "date"),
    .SDcols = unname(flag_cols)]
  setnames(same_day, c("date", unname(flag_cols)),
           c("contact.date", paste0("day_", names(flag_cols))))

  status <- merge(keys, in_bout, by = key_cols, all.x = TRUE)
  status <- merge(status, unknown, by = key_cols, all.x = TRUE)
  status <- merge(status, same_day, by = key_cols, all.x = TRUE)
  ## days outside follow-up are unknown, never healthy
  status[, in_follow_up := follow_up == "in follow-up"]
  status[is.na(bouted) | !in_follow_up, bouted := FALSE]
  status[is.na(unresolved), unresolved := FALSE]
  status[, healthy_day := in_follow_up & !bouted & !unresolved]
  status[, current_symptoms := fcase(
    bouted, "symptoms",
    healthy_day, "no symptoms",
    default = NA_character_
  )]
  for (nm in names(flag_cols)) {
    lab <- flag_labels[[nm]]
    day_flag <- status[[paste0("day_", nm)]]
    bout_flag <- status[[paste0("bout_", nm)]]
    ## a bout overlapping an unplaced report may have met the definition
    status[, (paste0("current_", nm)) := fcase(
      bouted & day_flag == "t", lab[1],
      bouted & day_flag != "t", lab[2],
      healthy_day, lab[2],
      default = NA_character_
    )]
    status[, (paste0("bout_", nm)) := fcase(
      bouted & bout_flag == "t", lab[1],
      bouted & !unresolved & bout_flag != "t", lab[2],
      healthy_day, lab[2],
      default = NA_character_
    )]
  }
  keep <- c(key_cols, "follow_up", "current_symptoms",
            paste0("current_", names(flag_cols)),
            paste0("bout_", names(flag_cols)))
  merge(contacts, status[, keep, with = FALSE], by = key_cols, all.x = TRUE)
}

## Positive if any report is positive, otherwise unknown if any is unknown.
combine_flags <- function(x) {
  x <- as.character(x)
  if (any(x == "t", na.rm = TRUE)) {
    "t"
  } else if (anyNA(x)) {
    NA_character_
  } else {
    "f"
  }
}
