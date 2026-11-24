## columns that flag a case definition or one of its components
bout_flag_columns <- c(
  "suddenly", "fever.symptoms", "systemic.symptoms", "resp.symptoms",
  "general.symptoms", "ili.symptoms", "ili", "ili.fever", "ili.self", "gi"
)

## three-valued logic on "t"/"f"/NA text
or3 <- function(a, b) {
  fcase(a == "t" | b == "t", "t", a == "f" & b == "f", "f",
        default = NA_character_)
}
and3 <- function(a, b) {
  fcase(a == "f" | b == "f", "f", a == "t" & b == "t", "t",
        default = NA_character_)
}
## positive if any report is positive, otherwise unknown if any is unknown
any_t <- function(x) {
  x <- as.character(x)
  if (any(x == "t", na.rm = TRUE)) {
    "t"
  } else if (anyNA(x)) {
    NA_character_
  } else {
    "f"
  }
}
## how the onset of a bout is recorded: the most informative answer of its
## reports
onset_order <- c("sudden", "not.sudden", "dont_know", "not.answered",
                 "no.question")
merge_onset <- function(x) {
  x <- as.character(x)
  found <- onset_order[onset_order %in% x]
  if (length(found) == 0) NA_character_ else found[1]
}

## Case definitions of whole bouts. The symptom columns of all reports of a
## bout are combined first and the definitions applied to the result, so that
## symptoms reported in different questionnaires of one illness count
## together. A definition whose components are not in the data falls back on
## "any report of the bout meets it". Returns one row per bout.
bout_flags <- function(reports, by = c("participant_id", "season", "bout")) {
  present <- intersect(bout_flag_columns, colnames(reports))
  merged <- reports[, lapply(.SD, any_t), by = by, .SDcols = present]
  has <- function(...) all(c(...) %in% colnames(merged))
  if (has("general.symptoms", "fever.symptoms", "systemic.symptoms")) {
    merged[, general.symptoms := or3(fever.symptoms, systemic.symptoms)]
  }
  if (has("ili.symptoms", "general.symptoms", "resp.symptoms")) {
    merged[, ili.symptoms := and3(general.symptoms, resp.symptoms)]
  }
  if (has("ili", "ili.symptoms", "suddenly")) {
    merged[, ili := and3(ili.symptoms, suddenly)]
  }
  if (has("ili.fever", "fever.symptoms", "ili")) {
    merged[, ili.fever := and3(fever.symptoms, ili)]
  }
  if ("onset" %in% colnames(reports)) {
    onset <- reports[, list(onset = merge_onset(onset)), by = by]
    merged <- merge(merged, onset, by = by, sort = FALSE)
  }
  merged
}

## Assign each symptomatic report to a bout, in place. The reports need to be
## sorted by participant, season and date, and to have an id column.
assign_bouts <- function(dt) {
  dt[, new.bout := (same == "no")]
  dt[, part.symptom.id := seq_len(.N), by = list(participant_id, season)]
  ## any() alone would ignore missing values next to a "t"; a participant with
  ## a missing value gets a missing first.no.symptoms
  dt[, first.no.symptoms := ifelse(
    if (anyNA(no.symptoms)) NA else any(no.symptoms == "t"),
    min(which(no.symptoms == "t")),
    0
  ),
  by = list(participant_id, season)
  ]
  dt[,
    min.symptoms.start.date := as.Date(ifelse(
      all(is.na(symptoms.start.date)),
      NA_character_,
      as.character(min(symptoms.start.date, na.rm = TRUE))
    )),
    by = list(participant_id, season)
  ]
  dt[
    part.symptom.id < first.no.symptoms & !is.na(symptoms.start.date) &
      symptoms.start.date == min.symptoms.start.date,
    new.bout := c(TRUE, new.bout[-1]),
    by = list(participant_id, season)
  ]
  dt[
    first.no.symptoms == 0 & part.symptom.id == min(part.symptom.id),
    new.bout := TRUE
  ]
  dt[is.na(new.bout), new.bout := FALSE]
  dt[, previous.no.symptoms := factor(
    c(NA_character_, as.character(no.symptoms[-.N])),
    levels = levels(no.symptoms)
  )]
  dt[part.symptom.id == 1, previous.no.symptoms := NA]
  dt[
    !is.na(previous.no.symptoms) & previous.no.symptoms == "t" &
      no.symptoms == "f",
    new.bout := TRUE
  ]
  dt[, previous.no.symptoms := NULL]
  dt[, previous.symptoms.end.date := as.Date(
    c(NA_character_, as.character(symptoms.end.date[-.N]))
  ), by = list(participant_id, season)]
  dt[
    !new.bout & !is.na(previous.symptoms.end.date) &
      !is.na(symptoms.start.date) & no.symptoms == "f" &
      previous.symptoms.end.date < symptoms.start.date,
    new.bout := TRUE
  ]
  dt[, previous.symptoms.end.date := NULL]
  dt[
    no.symptoms == "f",
    bout := cumsum(new.bout),
    by = list(participant_id, season)
  ]
  dt[bout == 0, bout := NA]
  dt[, new.bout := NULL]
  invisible(dt)
}

## Set the start and end date of each bout on its reports, in place.
fill_bout_dates <- function(dt) {
  last <- function(x) x[length(x)]
  last_not_na <- function(x) x[!is.na(x)][sum(!is.na(x))]
  dt[!is.na(bout), last.symptoms.start.date := last(symptoms.start.date),
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout),
    last.not.na.symptoms.start.date := last_not_na(symptoms.start.date),
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout), last.symptoms.end.date := last(symptoms.end.date),
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout),
    last.not.na.symptoms.end.date := last_not_na(symptoms.end.date),
    by = list(participant_id, season, bout)
  ]

  dt[!is.na(bout) & !is.na(last.symptoms.start.date),
    symptoms.start.date := last.symptoms.start.date,
    by = list(participant_id, season, bout)
  ]
  dt[
    !is.na(bout) & is.na(last.symptoms.start.date) &
      !is.na(last.not.na.symptoms.start.date),
    symptoms.start.date := last.not.na.symptoms.start.date,
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout) & is.na(last.not.na.symptoms.start.date),
    symptoms.start.date := date[1],
    by = list(participant_id, season, bout)
  ]
  dt[
    !is.na(bout) & is.na(last.symptoms.end.date) &
      !is.na(last.not.na.symptoms.end.date),
    symptoms.end.date := last.not.na.symptoms.end.date,
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout) & is.na(last.not.na.symptoms.end.date),
    symptoms.end.date := last(date),
    by = list(participant_id, season, bout)
  ]

  dt[, last.symptoms.start.date := NULL]
  dt[, last.not.na.symptoms.start.date := NULL]
  dt[, last.symptoms.end.date := NULL]
  dt[, last.not.na.symptoms.end.date := NULL]
  invisible(dt)
}

## The days of illness a report supports. A report with a start date and an
## end date supports the days between them (the end date counted no later
## than the report). A report of continuing illness supports the days from its
## start date, or its own date alone when it gives no start. The recovery of a
## continuing illness is unknown, as is the onset of one without a start date.
## A report with no usable interval supports no day.
report_evidence <- function(dt) {
  option <- if ("symptoms.end.option" %in% colnames(dt)) {
    as.character(dt$symptoms.end.option)
  } else {
    rep(NA_character_, nrow(dt))
  }
  still <- !is.na(option) & option == "still_ill"
  onset <- dt$symptoms.start.date
  recovery <- dt$symptoms.end.date
  rdate <- dt$date
  capped <- fifelse(!is.na(recovery), pmin(recovery, rdate), as.Date(NA))
  basis <- fcase(
    !is.na(onset) & !is.na(capped) & onset <= capped, "reported",
    !is.na(onset) & is.na(recovery) & still & onset <= rdate, "still.ill",
    is.na(onset) & is.na(recovery) & still, "still.ill.unknown.onset",
    default = "unresolved"
  )
  first.day <- fcase(
    basis == "reported" | basis == "still.ill", onset,
    basis == "still.ill.unknown.onset", rdate,
    default = as.Date(NA)
  )
  last.day <- fcase(
    basis == "reported", capped,
    basis == "still.ill" | basis == "still.ill.unknown.onset", rdate,
    default = as.Date(NA)
  )
  res <- data.table(
    participant_id = dt$participant_id, season = dt$season, bout = dt$bout,
    date = rdate, report.start = onset, report.end = recovery, basis = basis,
    onset.known = !is.na(onset),
    recovery.known = !is.na(recovery),
    first.day = first.day, last.day = last.day
  )
  if ("row_id" %in% colnames(dt)) res[, row_id := dt$row_id]
  res
}

## One row per day of illness supported by the reports of a bout.
evidence_days <- function(reports) {
  usable <- reports[basis != "unresolved"]
  if (nrow(usable) == 0) {
    return(data.table(
      participant_id = reports$participant_id[0], season = reports$season[0],
      bout = integer(), day = as.Date(character()), source = character()
    ))
  }
  days <- Map(function(from, to) seq(from, to, by = "day"),
              usable$first.day, usable$last.day)
  res <- usable[rep(seq_len(nrow(usable)), lengths(days)),
                c("participant_id", "season", "bout", "basis"), with = FALSE]
  res[, day := as.Date(unlist(days), origin = "1970-01-01")]
  res[, source := ifelse(basis == "still.ill.unknown.onset",
                         "report.date", "reported")]
  ## a day backed by a reported interval is not downgraded by a report
  ## that only places the illness on its own date
  res <- res[order(participant_id, season, bout, day, source == "report.date")]
  res <- res[!duplicated(res[, c("participant_id", "season", "bout", "day")])]
  res[, basis := NULL]
  res[]
}

## Prepare reports for bout assignment: sorted, numbered, with the bout of
## each symptomatic report. Returns the table before any dates are filled in.
reconstruct_bouts <- function(x, singletons = FALSE) {
  dt <- data.table(x)
  setorderv(dt, c("participant_id", "season", "date"))
  dt[, id := seq_len(.N)]
  assign_bouts(dt)
  if (singletons) {
    ## symptomatic reports outside any bout stay as episodes of their own
    dt[
      no.symptoms == "f" & is.na(bout),
      bout := (if (all(is.na(bout))) 0L else max(bout, na.rm = TRUE)) +
        seq_len(.N),
      by = list(participant_id, season)
    ]
  }
  dt
}

##' Evidence behind the bouts of illness
##'
##' \code{\link{bouts_of_illness}} gives one row per bout. The evidence it
##' rests on is in the list returned here, from the same reconstruction. The
##' bouts are those of \code{bouts_of_illness(x)}; \code{x} can be the same
##' data.
##'
##' \code{reports} has a row for each symptomatic report that belongs to a
##' bout, with the bout it belongs to, the dates it reported
##' (\code{report.start}, \code{report.end}) and the days of illness it
##' supports, \code{first.day} to \code{last.day}. The column \code{basis} says
##' how: "reported" (start and end date given, the end counted no later than
##' the report), "still.ill" (continuing illness from the start date to the
##' report), "still.ill.unknown.onset" (continuing illness with no start
##' date: only the day of the report is established, and neither the onset nor
##' the recovery) or "unresolved" (no usable interval, so no day).
##'
##' \code{days} has a row for each day of illness the reports establish, with
##' \code{source} "reported" for days within a reported or continuing
##' interval and "report.date" for days established only by a report of
##' continuing illness without a start date. Days between the intervals of a
##' bout are not added, and the bout dates of \code{bouts_of_illness} are not
##' spread over days.
##'
##' \code{bouts} has a row for each bout with the dates
##' \code{bouts_of_illness} gives it, and \code{onset.known} and
##' \code{recovery.known}, which say whether any report of the bout gave a
##' start or an end date. Where one is \code{FALSE} the date is that of a
##' report. The columns
##' \code{n.reports} and the case definitions follow: the symptom columns of
##' all reports of a bout are combined (a symptom counts if any report has it,
##' the onset if any report records it as sudden) and the definitions applied
##' to the result. The classification belongs to the bout and does not say
##' that the symptoms were present together on a day. The definitions are
##' \code{ili}, \code{ili.fever}, \code{ili.self}, \code{resp.symptoms} (ARI)
##' and the others of \code{\link{aggregate_symptoms}} that are in the data;
##' \code{onset} keeps the most informative onset answer ("sudden",
##' "not.sudden", "dont_know", "not.answered", "no.question").
##' Where the columns that a definition is built from are not in the data,
##' it is positive if any report of the bout is.
##'
##' @param x the data to extract bouts from, as for
##'   \code{\link{bouts_of_illness}}
##' @return a list of data tables: \code{bouts}, \code{reports} and
##'   \code{days}
##' @examples
##' dt <- extract_data(simulated_data, years = 2013:2014)
##' evidence <- bout_evidence(dt)
##' head(evidence$days)
##' @import data.table
##' @export
bout_evidence <- function(x) {
  dt <- reconstruct_bouts(x)
  bout_evidence_from(dt)
}

bout_evidence_from <- function(dt) {
  reports <- report_evidence(dt[!is.na(bout)])
  fill_bout_dates(dt)
  keys <- c("participant_id", "season", "bout")
  in_bout <- dt[!is.na(bout)]
  bouts <- in_bout[, list(
    symptoms.start.date = symptoms.start.date[1],
    symptoms.end.date = symptoms.end.date[1],
    n.reports = .N
  ), by = keys]
  known <- reports[, list(
    onset.known = any(onset.known), recovery.known = any(recovery.known)
  ), by = keys]
  bouts <- merge(bouts, known, by = keys, sort = FALSE)
  bouts <- merge(bouts, bout_flags(in_bout, keys), by = keys, sort = FALSE)
  setorderv(bouts, keys)
  list(bouts = bouts[], reports = reports[], days = evidence_days(reports))
}

## One row per day from symptoms.start.date to symptoms.end.date of each bout
## report. The flags are carried forward, taken from the bout or taken from
## the report, as daily.flags says.
expand_to_days <- function(dt, daily.flags, report.flags, flag.columns) {
  dt <- dt[!is.na(bout)]
  starts <- dt$symptoms.start.date
  ends <- pmax(dt$symptoms.end.date, starts)
  days <- Map(function(from, to) seq(from, to, by = "day"), starts, ends)
  dt <- dt[rep(seq_len(nrow(dt)), lengths(days))]
  dt[, day := as.Date(unlist(days), origin = "1970-01-01")]
  if (daily.flags != "bout" && length(flag.columns) > 0) {
    flags <- report.flags[
      dt,
      flag.columns,
      on = c("participant_id", "season", "bout", date = "day"),
      roll = if (daily.flags == "carry.forward") TRUE else FALSE,
      mult = "last",
      with = FALSE
    ]
    dt[, (flag.columns) := flags]
  }
  dt
}

##' Extract bouts of illness from a flusurvey data table
##'
##' @param x the data to extract bouts from
##' @param symptomatic.only whether to only include symptomatic periods
##' @param as.data.frame currently has no effect
##' @param daily.flags how to assign the case-definition flags set by
##'   \code{\link{aggregate_symptoms}} to the days of a bout.
##'   \code{"none"} returns one row per bout. The other values return one row
##'   per day, in the column \code{day}, from \code{symptoms.start.date} to
##'   \code{symptoms.end.date}. \code{"carry.forward"} takes the flags from
##'   the last report on or before the day. \code{"bout"} gives every day the
##'   flags of the bout. \code{"report"} sets flags only on days with a
##'   report.
##' @details The flags of a bout (those of the case definitions in
##'   \code{\link{aggregate_symptoms}}) are computed from the symptoms of all
##'   its reports together: a
##'   bout meets the symptom-based ILI definition if its reports jointly have
##'   the symptoms and a sudden onset, even if no single report does. The
##'   flags describe the bout and do not say that the symptoms were present
##'   together on one day. The days of illness that the reports establish, and
##'   which dates are reported and which only the date of a report, are in
##'   \code{\link{bout_evidence}}.
##' @return a data table of bouts of illness, or of their days
##' @author seb
##' @examples
##' dt <- extract_data(simulated_data, years = 2013:2014)
##' bouts <- bouts_of_illness(dt)
##' cols <- c("participant_id", "season", "symptoms.start.date",
##'           "symptoms.end.date")
##' head(as.data.frame(bouts)[, cols])
##' @import data.table
##' @importFrom stats median
##' @export
bouts_of_illness <- function(x, symptomatic.only = TRUE, as.data.frame = TRUE,
                             daily.flags = c(
                               "none", "carry.forward", "bout", "report"
                             )) {
  daily.flags <- match.arg(daily.flags)
  dt <- data.table(x)

  if ("health.score" %in% colnames(dt)) {
    baselines <-
      dt[
        no.symptoms == "t",
        list(
          baseline.health.score = as.numeric(median(health.score, na.rm = TRUE))
        ),
        by = list(participant_id, season)
      ]
    dt <- merge(dt, baselines, by = c("participant_id", "season"), all.x = TRUE)
  }

  ## the lags below need reports ordered by date within participant and season
  dt <- reconstruct_bouts(dt)

  tf <-
    colnames(dt)[vapply(colnames(dt), function(x) {
      length(setdiff(c("t", "f"), levels(dt[[x]]))) == 0
    }, TRUE)]

  fill_bout_dates(dt)

  flag.columns <- intersect(bout_flag_columns, colnames(dt))
  report.flags <- dt[!is.na(bout),
    c("participant_id", "season", "bout", "date", flag.columns),
    with = FALSE
  ]
  bout.flags <- bout_flags(
    dt[!is.na(bout)], c("participant_id", "season", "bout")
  )

  symptoms.id.column <- which(colnames(dt) == "symptom.id")
  if (symptoms.id.column > 1) {
    dt[
      !is.na(bout),
      min.part.symptom.id := min(part.symptom.id),
      by = list(participant_id, season, bout)
    ]
    dt[
      !is.na(bout),
      max.part.symptom.id := max(part.symptom.id),
      by = list(participant_id, season, bout)
    ]

    tf_columns <- intersect(colnames(dt), tf)

    cat("Merging columns:\n")
    for (column in tf_columns) {
      cat("  ", column, "\n")
      if (grepl("\\.no(ne)?$", column)) {
        dt[,
          paste(column) := factor(
            as.integer(all(get(column) == "t")),
            levels = 0:1,
            labels = c("f", "t")
          ),
          by = list(participant_id, season, bout)
        ]
      } else {
        dt[,
          paste(column) := factor(
            as.integer(any(get(column) == "t")),
            levels = 0:1,
            labels = c("f", "t")
          ),
          by = list(participant_id, season, bout)
        ]
      }
    }
    if ("health.score" %in% colnames(dt)) {
      dt[!is.na(bout),
        min.health.score := ifelse(
          all(is.na(health.score)),
          NA_integer_,
          min(health.score, na.rm = TRUE)
        ),
        by = list(participant_id, season, bout)
      ]
    }
    ## the case definitions of a bout come from its combined symptoms
    key.columns <- c("participant_id", "season", "bout")
    in.bout <- which(!is.na(dt$bout))
    bout.index <- match(
      do.call(paste, c(dt[in.bout, key.columns, with = FALSE], sep = "\r")),
      do.call(paste, c(bout.flags[, key.columns, with = FALSE], sep = "\r"))
    )
    for (column in setdiff(colnames(bout.flags), key.columns)) {
      if (!column %in% colnames(dt)) next
      current <- dt[[column]]
      updated <- as.character(current)
      updated[in.bout] <- bout.flags[[column]][bout.index]
      if (is.factor(current)) {
        updated <- factor(updated, levels = levels(current))
      }
      set(dt, j = column, value = updated)
    }
    dt <- dt[
      is.na(min.part.symptom.id) | part.symptom.id == min.part.symptom.id
    ]
  }
  if (symptomatic.only) {
    dt <- dt[!is.na(min.part.symptom.id)]
  }
  dt[, id := NULL]
  if (daily.flags != "none") {
    dt <- expand_to_days(dt, daily.flags, report.flags, flag.columns)
  }
  dt[]
}
