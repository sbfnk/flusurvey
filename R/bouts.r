##' Extract bouts of illness from a flusurvey data table
##'
##' The end date of a bout is the last reported end date, or the last report
##' date if none was reported. This applies to every row of the bout, whatever
##' the other arguments are. The earlier rule only filled in the end date of a
##' bout whose last report had none. The other arguments set how the start date
##' is chosen where the reports leave a choice. Where no report of a bout gives
##' a start date, \code{symptoms.start.date} is \code{NA} by default.
##'
##' @param x the data to extract bouts from
##' @param symptomatic.only whether to only include symptomatic periods
##' @param as.data.frame currently has no effect
##' @param start.date which reported start date to use for a bout: the
##' "last" one reported, or the "earliest"
##' @param missing.start what to use as start date if none was reported for a
##' bout: leave it "missing" (the default), or use the date of its
##' "first.report", which can fall after the end date
##' @param inverted.dates what to do if the end date is before the start date:
##' "keep" both, set both to "clear" (missing), or "reset.end" to the last
##' report date of the bout
##' @return a data table of bouts of illness
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
  x,
  symptomatic.only = TRUE,
  as.data.frame = TRUE,
  start.date = c("last", "earliest"),
  missing.start = c("missing", "first.report"),
  inverted.dates = c("keep", "clear", "reset.end")
) {
  start.date <- match.arg(start.date)
  missing.start <- match.arg(missing.start)
  inverted.dates <- match.arg(inverted.dates)
  dt <- copy(data.table(x))
  if (!"same" %in% colnames(dt)) dt[, same := NA_character_]

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
  setorderv(dt, c("participant_id", "season", "date"))
  dt[, id := seq_len(.N)]

  tf <-
    colnames(dt)[vapply(colnames(dt), function(x) {
      length(setdiff(c("t", "f"), levels(dt[[x]]))) == 0
    }, TRUE)]


  last <- function(x) x[length(x)]
  last_not_na <- function(x) x[!is.na(x)][sum(!is.na(x))]

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
  dt[!is.na(bout) & is.na(last.not.na.symptoms.end.date),
    symptoms.end.date := last(date),
    by = list(participant_id, season, bout)
  ]
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
    dt <- dt[
      is.na(min.part.symptom.id) | part.symptom.id == min.part.symptom.id
    ]
  }
  if (symptomatic.only) {
    dt <- dt[!is.na(min.part.symptom.id)]
  }
  dt[, id := NULL]
  dt[]
}
