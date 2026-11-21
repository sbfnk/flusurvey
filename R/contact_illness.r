##' Illness status of contact reports
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
##' \emph{Bouts.} A bout is built from all symptom reports of a participant
##' in a season: each report with symptoms supports the interval from its
##' \code{symptoms.start.date} to its \code{symptoms.end.date}, or to the
##' report \code{date} if the participant was still ill. Overlapping or
##' adjacent intervals are merged into one bout. This differs from
##' \code{\link{bouts_of_illness}}, which splits and joins reports by the
##' answer to whether the symptoms are the same as at the previous report and
##' by intervening reports without symptoms, and keeps one row per bout. Here
##' a bout is a period of days, so that contact days can be located within it,
##' and a participant reporting no symptoms between two overlapping intervals
##' does not split them.
##'
##' \emph{Daily status.} Within follow-up, \code{current_symptoms} is
##' "symptoms" on days in a bout and "no symptoms" on every other day, with or
##' without a report that day. The exception is a report with symptoms (or
##' without a known symptom status) whose dates give no usable interval:
##' missing, inconsistent, or an unknown end that is not "still ill". Its
##' illness cannot be placed, so every day since the participant's previous
##' report, up to and including its own date, is unknown (missing) unless
##' it lies in a bout. A report without symptoms is taken as no acute illness.
##' By default (\code{chronic = "none"}) no rule for chronic symptoms is
##' applied. Otherwise reports flagged as chronic are treated as no acute
##' illness: they support no bout and leave no day unknown.
##'
##' The case-definition flags are known per report, not per day. A bout meeting
##' a definition at some point need not have had every symptom present on
##' every day of it. The \code{bout_} columns therefore describe the whole
##' bout and are the ones for bout-based exposure: positive on every day of a
##' bout if any report in it meets the definition, negative if all reports have
##' it unmet and none is unresolved, and missing otherwise. The daily
##' \code{current_} columns are positive or negative within a bout only on days
##' with a symptom report whose interval includes that day, assuming that such
##' a report describes its own day, and are missing on other days of a bout:
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
##' a missing column gives missing flags.
##' @param min.reports the minimum number of days with symptom reports for a
##' participant to be eligible in a season
##' @param chronic how to identify chronic symptom reports: \code{"none"}
##' (the default) flags none; \code{"duration"} flags reports with a valid
##' interval of more than \code{chronic.days} days; \code{"condition"} flags
##' reports for which any of the \code{chronic.cols} is "t"
##' @param chronic.days the number of days beyond which a report counts as
##' chronic, for \code{chronic = "duration"}
##' @param chronic.cols columns of \code{symptoms} holding chronic medical
##' conditions, for \code{chronic = "condition"}. Absent columns are ignored,
##' but at least one must be present.
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
contact_illness <- function(contacts, symptoms, min.reports = 3,
                            chronic = c("none", "duration", "condition"),
                            chronic.days = NULL,
                            chronic.cols = c("chronic.heart.disease",
                                             "other.chronic.lung.disease",
                                             "other.chronic")) {
  chronic <- match.arg(chronic)
  flag_cols <- c(
    ili = "ili", ili_fever = "ili.fever", ili_self = "ili.self",
    ari = "resp.symptoms"
  )
  flag_labels <- list(
    ili = c("ILI", "no ILI"), ili_fever = c("ILI", "no ILI"),
    ili_self = c("ILI", "no ILI"), ari = c("ARI", "no ARI")
  )
  ## positive if any report is positive, otherwise unknown if any is unknown
  combine <- function(x) {
    x <- as.character(x)
    if (any(x == "t", na.rm = TRUE)) {
      "t"
    } else if (anyNA(x)) {
      NA_character_
    } else {
      "f"
    }
  }

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
  symptoms[, bstart := symptoms.start.date]
  symptoms[, bend := fcase(
    !is.na(symptoms.end.date), pmin(symptoms.end.date, date),
    !is.na(symptoms.end.option) & symptoms.end.option == "still_ill", date,
    default = as.Date(NA)
  )]
  symptoms[, healthy := as.character(no.symptoms) == "t"]
  symptoms[is.na(healthy), healthy := FALSE]
  symptoms[, valid := as.character(no.symptoms) == "f" &
             !is.na(bstart) & !is.na(bend) & bstart <= bend]
  symptoms[is.na(valid), valid := FALSE]
  symptoms[, is_chronic := chronic_reports(
    symptoms, chronic, chronic.days, chronic.cols
  )]
  symptoms[(is_chronic), valid := FALSE]

  ## eligibility and follow-up: first to last report without symptoms
  follow_up <- symptoms[, list(
    n_reports = uniqueN(date),
    fu_start = if (any(healthy)) min(date[healthy]) else as.Date(NA),
    fu_end = if (any(healthy)) max(date[healthy]) else as.Date(NA)
  ), by = group_by]
  follow_up[, eligible := n_reports >= min.reports & !is.na(fu_start)]

  ## bouts from all reports with a valid interval
  reports <- symptoms[(valid)]
  setorderv(reports, c(group_by, "bstart", "bend"))
  reports[, bout := cumsum(
    c(TRUE, bstart[-1] > head(cummax(as.numeric(bend)), -1) + 1)
  ), by = group_by]
  bouts <- reports[, c(
    ## reports are sorted by start; the first start is the earliest
    list(bout_start = bstart[1], bout_end = bend[which.max(as.numeric(bend))]),
    lapply(.SD, combine)
  ), by = c(group_by, "bout"), .SDcols = unname(flag_cols)]
  setnames(bouts, unname(flag_cols), paste0("bout_", names(flag_cols)))

  ## a report that cannot be placed in time leaves every day since the
  ## previous report unknown
  setorderv(symptoms, c(group_by, "date"))
  symptoms[, previous_date := c(as.Date(NA), unique(date))[
    match(date, unique(date))
  ], by = group_by]
  unresolved <- symptoms[
    !healthy & !valid & !is_chronic,
    list(unknown_start = fifelse(is.na(previous_date), date, previous_date + 1),
         unknown_end = date),
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

  in_bout <- merge(keys[, key_cols, with = FALSE], bouts,
                   by = group_by, allow.cartesian = TRUE)
  in_bout <- in_bout[contact.date >= bout_start & contact.date <= bout_end]
  in_bout[, c("bout", "bout_start", "bout_end") := NULL]
  in_bout[, bouted := TRUE]

  unknown <- merge(keys[, key_cols, with = FALSE], unresolved,
                   by = group_by, allow.cartesian = TRUE)
  unknown <- unique(unknown[
    contact.date >= unknown_start & contact.date <= unknown_end,
    key_cols, with = FALSE
  ])
  unknown[, unresolved := TRUE]

  ## reports of the contact day whose interval includes that day
  same_day <- reports[bstart <= date & bend >= date,
                      lapply(.SD, combine), by = c(group_by, "date"),
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

## Flags chronic (as opposed to acute) reports, using the `chronic` rule.
chronic_reports <- function(symptoms, chronic, chronic.days, chronic.cols) {
  if (chronic == "duration") {
    if (!(is.numeric(chronic.days) && length(chronic.days) == 1 &&
            !is.na(chronic.days))) {
      stop("'chronic.days' must be a single number when chronic = \"duration\"",
           call. = FALSE)
    }
    return(symptoms$valid & as.numeric(symptoms$bend - symptoms$bstart) + 1 >
             chronic.days)
  }
  if (chronic == "condition") {
    present <- intersect(chronic.cols, colnames(symptoms))
    if (length(present) == 0) {
      stop("none of 'chronic.cols' is a column of 'symptoms'", call. = FALSE)
    }
    conditions <- lapply(present, function(col) {
      as.character(symptoms[[col]]) %in% "t"
    })
    return(Reduce(`|`, conditions) & as.character(symptoms$no.symptoms) == "f")
  }
  rep(FALSE, nrow(symptoms))
}
