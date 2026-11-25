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
  groups <- list(
    fever.symptoms = c("fever", "fever.symptom"),
    systemic.symptoms = c("tired", "weakness", "headache"),
    resp.symptoms = c("sore.throat", "cough", "shortness.breath"),
    gi = c("vomiting", "diarrhoea")
  )
  for (flag in names(groups)) {
    cols <- intersect(groups[[flag]], colnames(reports))
    if (length(cols) == 0) next
    raw_flags <- reports[, list(value = any_t(unlist(lapply(.SD, function(x) {
      x <- as.character(x)
      fcase(is.na(x), NA_character_, x %in% c("t", "1"), "t", default = "f")
    })))), by = by, .SDcols = cols]
    merged[raw_flags, on = by, (flag) := i.value]
  }
  has <- function(...) all(c(...) %in% colnames(merged))
  if (has("fever.symptoms", "systemic.symptoms")) {
    merged[, general.symptoms := or3(fever.symptoms, systemic.symptoms)]
  }
  if (has("general.symptoms", "resp.symptoms")) {
    merged[, ili.symptoms := and3(general.symptoms, resp.symptoms)]
  }
  if (has("ili.symptoms", "suddenly")) {
    merged[, ili := and3(ili.symptoms, suddenly)]
  }
  if (has("fever.symptoms", "ili")) {
    merged[, ili.fever := and3(fever.symptoms, ili)]
  }
  if ("onset" %in% colnames(reports)) {
    onset <- reports[, list(
      onset = merge_onset(onset),
      onset.conflict = all(c("sudden", "not.sudden") %in% onset)
    ), by = by]
    merged <- merge(merged, onset, by = by, sort = FALSE)
  }
  merged
}

## Assign each symptomatic report to a bout, in place. The reports need to be
## sorted by participant, season and date, and to have an id column. A report
## not answered "no" also starts a new bout where its start date is after the
## previous end date ("dates"; not if it is answered "yes" with "continuation")
## or after the date of the previous report ("report.date").
assign_bouts <- function(dt, precedence = "dates") {
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
  ## A continuing illness at the first observed symptomatic report is still
  ## a bout; its onset may predate observation or remain unknown.
  dt[no.symptoms == "f", new.bout := new.bout | seq_len(.N) == 1L,
     by = list(participant_id, season)]
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
  dt[, previous.boundary := as.Date(
    c(NA_character_, as.character(
      if (precedence == "report.date") date[-.N] else symptoms.end.date[-.N]
    ))
  ), by = list(participant_id, season)]
  dt[
    !new.bout & !is.na(previous.boundary) &
      !is.na(symptoms.start.date) & no.symptoms == "f" &
      previous.boundary < symptoms.start.date &
      !(precedence == "continuation" & !is.na(same) & same == "yes"),
    new.bout := TRUE
  ]
  dt[, previous.boundary := NULL]
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
fill_bout_dates <- function(dt, start.date = "earliest",
                            missing.start = "first.report",
                            inverted.dates = "keep",
                            unresolved = "fill") {
  last <- function(x) x[length(x)]
  last_not_na <- function(x) x[!is.na(x)][sum(!is.na(x))]
  dt[!is.na(bout), last.symptoms.start.date := last(symptoms.start.date),
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout),
    last.not.na.symptoms.start.date := last_not_na(symptoms.start.date),
    by = list(participant_id, season, bout)
  ]
  dt[!is.na(bout),
    last.not.na.symptoms.end.date := last_not_na(symptoms.end.date),
    by = list(participant_id, season, bout)
  ]

  if (start.date == "earliest") {
    dt[!is.na(bout),
      first.not.na.symptoms.start.date := as.Date(ifelse(
        all(is.na(symptoms.start.date)),
        NA_character_,
        as.character(min(symptoms.start.date, na.rm = TRUE))
      )),
      by = list(participant_id, season, bout)
    ]
    dt[!is.na(bout) & !is.na(first.not.na.symptoms.start.date),
      symptoms.start.date := first.not.na.symptoms.start.date
    ]
    dt[, first.not.na.symptoms.start.date := NULL]
  } else {
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
  }
  if (missing.start == "first.report") {
    dt[!is.na(bout) & is.na(last.not.na.symptoms.start.date),
      symptoms.start.date := date[1],
      by = list(participant_id, season, bout)
    ]
  }
  dt[
    !is.na(bout) & !is.na(last.not.na.symptoms.end.date),
    symptoms.end.date := last.not.na.symptoms.end.date,
    by = list(participant_id, season, bout)
  ]
  if (unresolved == "fill") {
    dt[!is.na(bout) & is.na(last.not.na.symptoms.end.date),
      symptoms.end.date := last(date),
      by = list(participant_id, season, bout)
    ]
  }
  if (inverted.dates == "clear") {
    dt[!is.na(bout) & !is.na(symptoms.start.date) &
         !is.na(symptoms.end.date) & symptoms.end.date < symptoms.start.date,
       c("symptoms.start.date", "symptoms.end.date") := list(
         as.Date(NA_character_), as.Date(NA_character_)
       )]
  } else if (inverted.dates == "reset.end") {
    dt[!is.na(bout), last.report.date := last(date),
      by = list(participant_id, season, bout)
    ]
    dt[!is.na(bout) & !is.na(symptoms.start.date) &
        !is.na(symptoms.end.date) & symptoms.end.date < symptoms.start.date,
      symptoms.end.date := last.report.date
    ]
    dt[, last.report.date := NULL]
  }

  dt[, last.symptoms.start.date := NULL]
  dt[, last.not.na.symptoms.start.date := NULL]
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
  original_onset <- dt$symptoms.start.date
  recovered <- dt[, list(onset = if (all(is.na(symptoms.start.date))) {
    as.Date(NA)
  } else {
    min(symptoms.start.date, na.rm = TRUE)
  }), by = list(participant_id, season, bout)]
  onset <- recovered[dt, on = c("participant_id", "season", "bout")]$onset
  onset[!is.na(original_onset)] <- original_onset[!is.na(original_onset)]
  recovery <- dt$symptoms.end.date
  rdate <- dt$date
  capped <- fifelse(!is.na(recovery), pmin(recovery, rdate), as.Date(NA))
  basis <- fcase(
    !is.na(onset) & !is.na(capped) & onset <= capped, "reported",
    !is.na(onset) & is.na(recovery) & still & onset <= rdate, "still.ill",
    is.na(recovery) & still, "still.ill.unknown.onset",
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
    date = rdate, report.start = original_onset, report.end = recovery,
    basis = basis,
    onset.known = !is.na(original_onset),
    recovery.known = !is.na(recovery),
    first.day = first.day, last.day = last.day
  )
  res[, row_id := if ("row_id" %in% colnames(dt)) dt$row_id else dt$id]
  if ("onset" %in% colnames(dt)) res[, onset := as.character(dt$onset)]
  res
}

## A bout is continuous between its established illness days. The inferred
## gap days are labelled separately from direct report evidence.
evidence_days <- function(reports) {
  keys <- c("participant_id", "season", "bout")
  usable <- reports[basis != "unresolved"]
  if (nrow(usable) == 0) {
    return(data.table(
      participant_id = reports$participant_id[0], season = reports$season[0],
      bout = integer(), day = as.Date(character()), source = character()
    ))
  }
  spans <- usable[, list(first.day = min(first.day), last.day = max(last.day)),
                  by = keys]
  days <- Map(function(from, to) seq(from, to, by = "day"),
              spans$first.day, spans$last.day)
  res <- spans[rep(seq_len(nrow(spans)), lengths(days)), keys, with = FALSE]
  res[, day := as.Date(unlist(days), origin = "1970-01-01")]
  res[, source := "within.bout"]
  ## A direct interval takes precedence over a report-day observation.
  for (basis_type in c("still.ill.unknown.onset", "still.ill", "reported")) {
    intervals <- usable[basis == basis_type]
    res[intervals, on = c(keys, "day>=first.day", "day<=last.day"),
        source := if (basis_type == "still.ill.unknown.onset") {
          "report.date"
        } else {
          "reported"
        }]
  }
  res[]
}

## Prepare reports for bout assignment: sorted, numbered, with the bout of
## each symptomatic report. Returns the table before any dates are filled in.
reconstruct_bouts <- function(x, precedence = "dates") {
  dt <- copy(data.table(x))
  dt[, no.symptoms := factor(as.character(no.symptoms), levels = c("f", "t"))]
  if (!"same" %in% colnames(dt)) dt[, same := NA_character_]
  setorderv(dt, c("participant_id", "season", "date"))
  dt[, id := seq_len(.N)]
  assign_bouts(dt, precedence)

  dt
}

##' Evidence behind the bouts of illness
##'
##' \code{\link{bouts_of_illness}} gives one row per bout. The evidence it
##' rests on is in the list returned here, from the same reconstruction. The
##' bout membership is the same as in \code{bouts_of_illness(x)}. Date
##' policies affect selected boundaries, not the original report evidence.
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
##' bout are included with source "within.bout": illness persists between
##' established illness days of the same bout. Selected fallback boundaries
##' are not used to establish illness. The \code{intervals} table contains
##' the first and last established day per bout without daily expansion.
##'
##' \code{bouts} has a row per bout with dates selected by the supplied
##' date policies, and \code{onset.known} and
##' \code{recovery.known}, which say whether any report of the bout gave a
##' matching start or end date for the selected boundary. The columns
##' \code{onset.source} and \code{recovery.source} distinguish reported
##' dates, missing boundaries and first/last-report fallbacks. The columns
##' \code{n.reports} and the case definitions follow: the symptom columns of
##' all reports of a bout are combined (a symptom counts if any report has it,
##' the onset if any report records it as sudden) and the definitions applied
##' to the result. The classification belongs to the bout and does not say
##' that the symptoms were present together on a day. The definitions are
##' \code{ili}, \code{ili.fever}, \code{ili.self}, \code{resp.symptoms} (ARI)
##' and the others of \code{\link{aggregate_symptoms}} that are in the data;
##' \code{onset.conflict} records conflicting explicit onset answers;
##' \code{onset} keeps the highest-precedence onset answer ("sudden",
##' "not.sudden", "dont_know", "not.answered", "no.question").
##' Where the columns that a definition is built from are not in the data,
##' it is positive if any report of the bout is.
##'
##' @param x the data to extract bouts from, as for
##'   \code{\link{bouts_of_illness}}
##' @param start.date,missing.start,inverted.dates Date policies as in
##'   \code{\link{bouts_of_illness}}; \code{start.date} defaults to
##'   "earliest" here as there. Evidence defaults to leaving an
##'   unreported onset missing; the summary interface defaults to the first
##'   report. These policies do not replace the original report evidence.
##' @param precedence,unresolved How bouts are formed and what happens to a
##'   bout with no reported end date, as in \code{\link{bouts_of_illness}}.
##' @param expand.days whether to expand established illness intervals to
##'   individual days. Set FALSE for interval matching without daily expansion.
##' @return a list of data tables: \code{bouts}, \code{reports},
##'   \code{intervals} and \code{days}
##' @examples
##' dt <- extract_data(simulated_data, years = 2013:2014)
##' evidence <- bout_evidence(dt)
##' head(evidence$days)
##' @import data.table
##' @export
bout_evidence <- function(
  x, start.date = c("earliest", "last"),
  missing.start = c("missing", "first.report"),
  inverted.dates = c("keep", "clear", "reset.end"),
  precedence = c("dates", "continuation", "report.date"),
  unresolved = c("fill", "missing"), expand.days = TRUE
) {
  dt <- reconstruct_bouts(x, match.arg(precedence))
  bout_evidence_from(dt, match.arg(start.date), match.arg(missing.start),
                     match.arg(inverted.dates), match.arg(unresolved),
                     expand.days)
}

bout_evidence_from <- function(dt, start.date, missing.start, inverted.dates,
                               unresolved, expand.days) {
  reports <- report_evidence(dt[!is.na(bout)])
  fill_bout_dates(dt, start.date, missing.start, inverted.dates, unresolved)
  keys <- c("participant_id", "season", "bout")
  in_bout <- dt[!is.na(bout)]
  bouts <- in_bout[, list(
    symptoms.start.date = symptoms.start.date[1],
    symptoms.end.date = symptoms.end.date[1],
    n.reports = .N
  ), by = keys]
  selected <- merge(reports, bouts, by = keys)
  known <- selected[, list(
    onset.known = any(!is.na(symptoms.start.date) &
                        symptoms.start.date == report.start, na.rm = TRUE),
    recovery.known = any(!is.na(symptoms.end.date) &
                           symptoms.end.date == report.end, na.rm = TRUE)
  ), by = keys]
  bouts <- merge(bouts, known, by = keys, sort = FALSE)
  bouts[, onset.source := fcase(
    is.na(symptoms.start.date), "missing", onset.known, "reported",
    default = "first.report"
  )]
  bouts[, recovery.source := fcase(
    is.na(symptoms.end.date), "missing", recovery.known, "reported",
    default = "last.report"
  )]
  bouts <- merge(bouts, bout_flags(in_bout, keys), by = keys, sort = FALSE)
  setorderv(bouts, keys)
  intervals <- reports[basis != "unresolved",
    if (.N) list(first.day = min(first.day), last.day = max(last.day)) else
      list(first.day = as.Date(character()), last.day = as.Date(character())),
    by = keys
  ]
  list(bouts = bouts[], reports = reports[], intervals = intervals[],
       days = evidence_days(if (expand.days) reports else reports[0]))
}

## One row per day from symptoms.start.date to symptoms.end.date of each bout
## report. The flags are carried forward, taken from the bout or taken from
## the report, as daily.flags says.
expand_to_days <- function(dt, daily.flags, report.flags, flag.columns) {
  dt <- dt[!is.na(bout) & !is.na(symptoms.start.date) &
             !is.na(symptoms.end.date) &
             symptoms.start.date <= symptoms.end.date]
  starts <- dt$symptoms.start.date
  ends <- dt$symptoms.end.date
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
##' @param precedence how a report joins the previous bout. A report answered
##' "no" always starts a new bout. Otherwise, with "dates", a report whose start
##' date is after the previous end date starts a new bout, even if it is
##' answered "yes". With "continuation", a report answered "yes" stays in the
##' bout whatever its dates. With "report.date", a report whose start date is
##' after the previous report date starts a new bout.
##' @param unresolved what to do with a bout with no reported end date: "fill"
##' its end with the last report date, or leave it "missing"
##' @param start.date which reported start date to use for a bout: the
##' "earliest" one reported (the default, as a bout started at the last
##' reported date can start after a report that already gives symptoms of it),
##' or the "last" one
##' @param missing.start what to use as start date if none was reported for a
##' bout: the date of its "first.report", or "missing"
##' @param inverted.dates what to do if the end date is before the start date:
##' "keep" both, set both to "clear" (missing), or "reset.end" to the last
##' report date of the bout
##' @param daily.flags how to assign the case-definition flags set by
##'   \code{\link{aggregate_symptoms}} to the days of a bout.
##'   \code{"none"} returns one row per bout. The other values return one row
##'   per day, in the column \code{day}, from \code{symptoms.start.date} to
##'   \code{symptoms.end.date}. \code{"carry.forward"} takes the flags from
##'   the last report on or before the day. \code{"bout"} gives every day the
##'   flags of the bout. \code{"report"} sets flags only on days with a
##'   report. Missing or inverted selected intervals are not expanded.
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
bouts_of_illness <- function(
  x, symptomatic.only = TRUE, as.data.frame = TRUE,
  start.date = c("earliest", "last"),
  missing.start = c("first.report", "missing"),
  inverted.dates = c("keep", "clear", "reset.end"),
  daily.flags = c("none", "carry.forward", "bout", "report"),
  precedence = c("dates", "continuation", "report.date"),
  unresolved = c("fill", "missing")
) {
  precedence <- match.arg(precedence)
  unresolved <- match.arg(unresolved)
  start.date <- match.arg(start.date)
  missing.start <- match.arg(missing.start)
  inverted.dates <- match.arg(inverted.dates)
  daily.flags <- match.arg(daily.flags)
  dt <- copy(data.table(x))

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
  dt <- reconstruct_bouts(dt, precedence)

  tf <-
    colnames(dt)[vapply(colnames(dt), function(x) {
      length(setdiff(c("t", "f"), levels(dt[[x]]))) == 0
    }, TRUE)]

  fill_bout_dates(dt, start.date, missing.start, inverted.dates, unresolved)

  flag.columns <- intersect(bout_flag_columns, colnames(dt))
  report.flags <- dt[!is.na(bout),
    c("participant_id", "season", "bout", "date", flag.columns),
    with = FALSE
  ]
  bout.flags <- bout_flags(
    dt[!is.na(bout)], c("participant_id", "season", "bout")
  )

  dt[
    !is.na(bout),
    min.part.symptom.id := part.symptom.id[1L],
    by = list(participant_id, season, bout)
  ]
  dt[
    !is.na(bout),
    max.part.symptom.id := part.symptom.id[.N],
    by = list(participant_id, season, bout)
  ]

  tf_columns <- intersect(colnames(dt), tf)

  cat("Merging columns:\n")
  for (column in tf_columns) {
    cat("  ", column, "\n")
    if (grepl("\\.no(ne)?$", column)) {
      dt[!is.na(bout),
        paste(column) := factor(
          as.integer(all(get(column) == "t")),
          levels = 0:1,
          labels = c("f", "t")
        ),
        by = list(participant_id, season, bout)
      ]
    } else {
      dt[!is.na(bout),
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
      updated <- factor(updated, levels = union(levels(current),
                                                na.omit(updated)))
    }
    set(dt, j = column, value = updated)
  }
  dt <- dt[
    is.na(min.part.symptom.id) | part.symptom.id == min.part.symptom.id
  ]
  if (symptomatic.only) {
    dt <- dt[!is.na(min.part.symptom.id)]
  }
  dt[, id := NULL]
  if (daily.flags != "none") {
    dt <- expand_to_days(dt, daily.flags, report.flags, flag.columns)
  }
  dt[]
}
