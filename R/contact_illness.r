##' Illness status of contact reports
##'
##' Locates each contact report, by \code{contact.date}, within the illness
##' bouts of the same participant. A bout is built from all symptom reports of
##' a participant: each report with symptoms supports the interval from its
##' \code{symptoms.start.date} to its \code{symptoms.end.date}, or to the
##' report \code{date} if the participant was still ill. Overlapping or
##' adjacent intervals are merged into one bout. A report whose dates are
##' missing or inconsistent, or whose end is not known, supports no interval.
##' A symptom report is not needed on the contact day itself.
##'
##' \code{current_symptoms} is "symptoms" if the contact day lies in a bout. It
##' is "no symptoms" if it does not and the participant filed a usable report
##' (no symptoms, or symptoms with a valid interval) on the contact day, and
##' missing otherwise. Missing means there is no information: a contact day
##' without a matching bout is not taken as evidence of health.
##'
##' The case-definition flags are known per report, not per day. A bout meeting
##' a definition at some point need not have had every symptom present on
##' every day of it. The \code{bout_} columns therefore describe the whole
##' bout: positive if any report in the bout meets the definition, negative if
##' all reports have it unmet and missing otherwise. Outside a bout they equal
##' the daily columns. The daily \code{current_} columns are positive or
##' negative only on days with a symptom report whose interval includes that
##' day, assuming that such a report describes its own day, and are missing on
##' other days of a bout. Outside a bout they are negative where the
##' participant was observed, as for \code{current_symptoms}. Several reports
##' on a day are combined: positive if any is positive, otherwise missing if
##' any is unknown.
##'
##' Read the symptom data with a \code{clean} argument in
##' \code{\link{extract_data}} that cannot drop symptom reports, as the default
##' cleaning (e.g., \code{"remove.first"}) can remove reports that support a
##' bout. Use \code{clean = c("unsuccessful.join",
##' "remove.bad.symptom.dates")} to clear invalid symptom dates without
##' dropping reports when reading a single survey.
##' @param contacts a data table of contact reports with columns
##' \code{global_id} and \code{contact.date}
##' @param symptoms a data table of symptom reports, as returned by
##' \code{\link{extract_data}}, with columns \code{global_id}, \code{date},
##' \code{no.symptoms}, \code{symptoms.start.date}, \code{symptoms.end.date}
##' and \code{symptoms.end.option}. The columns \code{ili} (symptom-based
##' ILI), \code{ili.fever} (symptom-based ILI with fever), \code{ili.self}
##' (self-assessed ILI) and \code{resp.symptoms} (ARI) are used where present;
##' a missing column gives missing flags.
##' @return the contact reports with added columns \code{current_symptoms}
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
contact_illness <- function(contacts, symptoms) {
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

  symptoms <- data.table(symptoms)
  symptoms <- symptoms[!is.na(global_id) & !is.na(date)]
  for (col in flag_cols) {
    if (!col %in% colnames(symptoms)) symptoms[, (col) := NA_character_]
  }
  symptoms[, bstart := symptoms.start.date]
  symptoms[, bend := fcase(
    !is.na(symptoms.end.date), pmin(symptoms.end.date, date),
    !is.na(symptoms.end.option) & symptoms.end.option == "still_ill", date,
    default = as.Date(NA)
  )]
  symptoms[, valid := as.character(no.symptoms) == "f" &
             !is.na(bstart) & !is.na(bend) & bstart <= bend]
  symptoms[is.na(valid), valid := FALSE]
  symptoms[, healthy := as.character(no.symptoms) == "t"]
  symptoms[is.na(healthy), healthy := FALSE]

  ## bouts from all reports with a valid interval
  reports <- symptoms[(valid)]
  setorder(reports, global_id, bstart, bend)
  reports[, bout := cumsum(
    c(TRUE, bstart[-1] > head(cummax(as.numeric(bend)), -1) + 1)
  ), by = global_id]
  bouts <- reports[, c(
    ## reports are sorted by start; the first start is the earliest
    list(bout_start = bstart[1], bout_end = bend[which.max(as.numeric(bend))]),
    lapply(.SD, combine)
  ), by = list(global_id, bout), .SDcols = unname(flag_cols)]
  setnames(bouts, unname(flag_cols), paste0("bout_", names(flag_cols)))

  keys <- unique(data.table(contacts)[
    !is.na(global_id) & !is.na(contact.date),
    list(global_id, contact.date)
  ])
  in_bout <- merge(keys, bouts, by = "global_id", allow.cartesian = TRUE)
  in_bout <- in_bout[contact.date >= bout_start & contact.date <= bout_end]
  in_bout[, c("bout", "bout_start", "bout_end") := NULL]
  in_bout[, bouted := TRUE]

  ## reports of the contact day whose interval includes that day
  same_day <- reports[bstart <= date & bend >= date,
                      lapply(.SD, combine), by = list(global_id, date),
                      .SDcols = unname(flag_cols)]
  setnames(same_day, c("date", unname(flag_cols)),
           c("contact.date", paste0("day_", names(flag_cols))))

  ## days on which the participant filed a usable report
  observed <- unique(symptoms[healthy | valid, list(global_id, date)])
  setnames(observed, "date", "contact.date")
  observed[, observed := TRUE]

  status <- merge(keys, in_bout, by = c("global_id", "contact.date"),
                  all.x = TRUE)
  status <- merge(status, observed, by = c("global_id", "contact.date"),
                  all.x = TRUE)
  status <- merge(status, same_day, by = c("global_id", "contact.date"),
                  all.x = TRUE)
  status[is.na(bouted), bouted := FALSE]
  status[is.na(observed), observed := FALSE]
  status[, current_symptoms := fcase(
    bouted, "symptoms",
    observed, "no symptoms",
    default = NA_character_
  )]
  for (nm in names(flag_cols)) {
    lab <- flag_labels[[nm]]
    for (prefix in c("current", "bout")) {
      flag <- status[[paste0(if (prefix == "current") "day" else "bout",
                             "_", nm)]]
      status[, (paste0(prefix, "_", nm)) := fcase(
        bouted & flag == "t", lab[1],
        bouted & flag != "t", lab[2],
        !bouted & observed, lab[2],
        default = NA_character_
      )]
    }
  }
  keep <- c("global_id", "contact.date", "current_symptoms",
            paste0("current_", names(flag_cols)),
            paste0("bout_", names(flag_cols)))
  merge(data.table(contacts), status[, keep, with = FALSE],
        by = c("global_id", "contact.date"), all.x = TRUE)
}
