##' Clean and merge flusurvey data tables
##'
##' @param data the data to merge and clean, usually the result of
##' \code{read_data}. The surveys are joined in a fixed order (background,
##' contact, symptom), regardless of their order in \code{data}. The last one
##' present drives a rolling join. The result has one row per report of that
##' survey (the symptom reports, if present), each taking the latest report of
##' the other surveys on or before its date. Any other survey name gives an
##' error.
##'
##' Joining the contact and symptom surveys gives a warning. The join pairs
##' each symptom report with the latest earlier contact report, however long
##' before it was filed, and a contact report describes one day whereas a
##' symptom report describes the week before it. With
##' 'unsuccessful.join' the symptom reports without an earlier contact report
##' are dropped, shortening episodes of illness. To link the two, extract
##' the symptom and contact surveys separately, get the episodes with
##' \code{\link{bouts_of_illness}} and flag the contact reports made during an
##' episode with \code{\link{flag_ill_contacts}}.
##' @param clean cleaning options, NULL for no cleaning, otherwise a vector of
##' cleans to perform (by default all):
##' - 'remove.first', whether to remove everyone's first report,
##' - 'remove.bad.symptom.dates', whether to remove bad symptom dates: symptom
##' start before the date of the participant's first report in the season or
##' more than \code{max.symptom.days} days before the report that gives it, end
##' of symptoms before start of symptoms, symptoms reported to have started or
##' ended after date of report. Bad start dates are set to missing.
##' - 'remove.bad.health.scores', whether to remove health scores <0 or >100
##' - 'limit.season', whether to limit a flu season to November -> April
##' - 'remove.postcodes', whether to remove postcodes
##' - 'n.reports', whether to exclude those with fewer than \code{min.reports}
##' reports
##' - 'unsuccessful.join', whether to exclude those with unsuccesful joins (e.g.
##' if symptoms are reported without a background survey present; the web site
##' should have prevented this, but doesn't appear to have done so)
##' - 'only.symptoms', whether to exclude those that have no report without
##' symptoms
##' @param min.reports minimum number of reports per user (ignored if
##' 'min.reports' is not given as a cleaning option)
##' @param ranges what to do if a number of contacts is given as a range: sample
##' from the range ("sample"), or apply a function, given as character string
##' (e.g., "mean")
##' @param age.breaks a vector of the highest age (in whole years) in each age
##' group except the last (first age group starts at 0 years of age). The last
##' group is open-ended. The default gives the groups 0-18, 19-44, 45-64 and
##' 65+, matching the age bands of the contact questions.
##' @param max.symptom.days maximum number of days before a report that
##' symptoms can have started (ignored if 'remove.bad.symptom.dates' is
##' not given as a cleaning option); use \code{Inf} for no limit
##' @return a rolling-joined data table. \code{day.of.week} (1 for Monday to 7
##' for Sunday) is that of the report date. If the contact survey is included,
##' \code{contact.date} is the day the contacts describe (the day before the
##' report) and \code{contact.day.of.week} is its day of the week. Use these
##' rather than \code{day.of.week} when analysing contacts.
##' @author seb
##' @examples
##' dt <- merge_data(simulated_data[["2013"]])
##' table(dt$ili)
##'
##' ## only remove each participant's first report
##' merge_data(simulated_data[["2013"]], clean = "remove.first")
##' @import data.table
##' @importFrom lubridate interval years
##' @importFrom plyr revalue
##' @importFrom stats runif
##' @export
merge_data <- function(
  data,
  clean = c(
    "remove.first",
    "remove.bad.symptom.dates",
    "remove.bad.health.scores",
    "limit.season",
    "remove.postcodes",
    "n.reports",
    "unsuccessful.join",
    "only.symptoms"
  ),
  min.reports = 3,
  ranges = "sample",
  age.breaks = c(18, 44, 64),
  max.symptom.days = 28
) {
  survey_order <- c("background", "contact", "symptom")
  if (!all(names(data) %in% survey_order)) {
    stop(
      "Unknown survey(s) ",
      toString(paste0("'", setdiff(names(data), survey_order), "'")),
      "; expected some of ", toString(paste0("'", survey_order, "'")), ".",
      call. = FALSE
    )
  }
  if (anyDuplicated(names(data))) {
    stop("Each survey can be given only once.", call. = FALSE)
  }
  ## put the surveys in order first: the last table drives the rolling join
  data <- data[intersect(survey_order, names(data))]
  if (all(c("contact", "symptom") %in% names(data))) {
    warning(
      "The contact and symptom surveys are joined by report date, which ",
      "pairs each symptom report with the latest earlier contact report ",
      "however long before it was filed, and (with 'unsuccessful.join') ",
      "drops symptom reports that have none. To link contacts to illness, ",
      "extract them separately and use flag_ill_contacts().",
      call. = FALSE
    )
  }
  dt_list <- list()
  ## Check for an empty `clean` first, because match.arg() maps NULL to
  ## the first choice
  if (length(clean) == 0) {
    clean <- character(0)
  } else {
    clean <- match.arg(clean, several.ok = TRUE)
  }
  for (name in names(data)) {
    ## only keep last report per day
    dt <- copy(data.table(data[[name]]))
    dt <- dt[!duplicated(dt[, list(global_id, date)], fromLast = TRUE)]

    if ("limit.season" %in% clean) {
      dt <- limit_to_season(dt)
    }

    dt <- switch(name,
      symptom = clean_symptom(dt, clean, min.reports, max.symptom.days),
      background = clean_background(dt, age.breaks),
      contact = clean_contact(dt, ranges),
      dt
    )

    if ("id" %in% colnames(dt)) {
      setnames(dt, "id", paste(name, "id", sep = "."))
    }
    if ("timestamp" %in% colnames(dt)) dt[, timestamp := NULL]

    setkey(dt, global_id, date)
    dt_list[[name]] <- dt
  }

  res <- join_surveys(dt_list)

  if ("unsuccessful.join" %in% clean) {
    res <- remove_unsuccessful_joins(res)
  }

  setkey(res, date, global_id)

  if ("remove.postcodes" %in% clean) {
    res <- res[,
      setdiff(colnames(res), grep("postcode$", colnames(res), value = TRUE)),
      with = FALSE
    ]
  }

  ## take the columns to convert from the joined table, because a t/f column
  ## can appear in an earlier survey only
  convert_logical_columns(res, res)
}

## Keep the reports from the season that contains the date with most reports.
limit_to_season <- function(dt) {
  ## figure out date with most reports, this defines the season
  reports <- dt[, .N, date]
  max_date <- reports[N == max(N), date]
  ## season begins on 1 November before the maximum date
  if (lubridate::month(max_date) >= 11) {
    season_start <- as.Date(paste(year(max_date), 11, 1, sep = "-"))
  } else {
    season_start <- as.Date(paste(year(max_date) - 1, 11, 1, sep = "-"))
  }
  ## season ends on 1 April after the maximum date
  if (lubridate::month(max_date) <= 4) {
    season_end <- as.Date(paste(year(max_date), 4, 1, sep = "-"))
  } else {
    season_end <- as.Date(paste(year(max_date) + 1, 4, 1, sep = "-"))
  }
  dt[date >= season_start & date <= season_end]
}

## Joins the surveys, each onto the one before it, by the closest earlier date.
join_surveys <- function(dt_list) {
  res <- dt_list[[length(dt_list)]]
  if (length(dt_list) > 1) {
    for (i in rev(seq_len(length(dt_list) - 1))) {
      res <- dt_list[[i]][res, roll = TRUE]
    }
  }

  res <- res[nzchar(as.character(global_id), keepNA = TRUE)]

  res[, day.of.week := (wday(date) - 2) %% 7 + 1]
  res[, month := month(date)]
  res
}

## Drops the rows in which any of the surveys is missing.
remove_unsuccessful_joins <- function(res) {
  id_cols <- grep("\\.id$", colnames(res), value = TRUE)
  for (col in id_cols) {
    res <- res[!is.na(get(col))]
  }
  res
}

## Turns the character columns of `reference` that only hold "t" and "f" into
## factors in `res`.
convert_logical_columns <- function(res, reference) {
  char_columns <-
    which(
      vapply(colnames(res), function(x) class(reference[[x]]), "") ==
        "character",
      TRUE
    )

  tf <-
    names(char_columns)[vapply(names(char_columns), function(x) {
      length(setdiff(
        c("t", "f"), setdiff(unique(reference[[x]]), NA_character_)
      )) == 0
    }, TRUE)]

  for (column in tf) res[, paste(column) := factor(get(column))]

  res
}

## ---- symptom survey ---------------------------------------------------------

clean_symptom <- function(dt, clean, min.reports, max.symptom.days) {
  dt <- aggregate_symptoms(dt)
  ## calculate min.reports, max.reports, nReports
  dt[, nReports := .N, by = global_id]
  if ("n.reports" %in% clean) {
    dt <- dt[nReports >= min.reports]
  }

  dt[, min.date := min(date), by = global_id]
  dt[, max.date := max(date), by = global_id]

  ## remove first survey completed by everyone
  if ("remove.first" %in% clean) {
    dt <- dt[duplicated(global_id)]
  }

  if ("remove.bad.symptom.dates" %in% clean) {
    remove_bad_symptom_dates(dt, max.symptom.days)
  }

  if ("remove.bad.health.scores" %in% clean) {
    dt <- remove_bad_health_scores(dt)
  }

  if ("only.symptoms" %in% clean) {
    dt <- keep_symptom_free_reporters(dt)
  }
  dt
}

## Sets symptom dates that cannot be right to missing, by reference.
remove_bad_symptom_dates <- function(dt, max.symptom.days) {
  ## remove end date before start date
  if (length(
    intersect(c("symptoms.start.date", "symptoms.end.date"), colnames(dt))
  ) == 2) {
    dt[
      !is.na(symptoms.end.date) & !is.na(symptoms.start.date) &
        symptoms.end.date < symptoms.start.date,
      c("symptoms.start.date", "symptoms.end.date") := list(
        as.Date(NA_character_),
        as.Date(NA_character_)
      )
    ]
  }
  ## remove start date after reporting date
  if ("symptoms.start.date" %in% colnames(dt)) {
    dt[
      !is.na(symptoms.start.date) & symptoms.start.date > date,
      symptoms.start.date := as.Date(NA_character_)
    ]
  }
  ## remove start dates before the first report or too long before the
  ## report that gives them, as they would extend an episode of illness
  ## over all intervening reports
  if ("symptoms.start.date" %in% colnames(dt)) {
    dt[
      !is.na(symptoms.start.date) &
        (symptoms.start.date < min.date |
           as.numeric(date - symptoms.start.date) > max.symptom.days),
      symptoms.start.date := as.Date(NA_character_)
    ]
  }
  ## remove end date after reporting date
  if ("symptoms.end.date" %in% colnames(dt)) {
    dt[
      !is.na(symptoms.end.date) & symptoms.end.date > date,
      symptoms.end.date := as.Date(NA_character_)
    ]
  }
  invisible(dt)
}

remove_bad_health_scores <- function(dt) {
  if (!"health.score" %in% colnames(dt)) {
    return(dt)
  }
  dt[
    health.score < 0 | health.score > 100,
    health.score := NA_real_
  ]
}

## Keeps the participants with at least one report without symptoms.
keep_symptom_free_reporters <- function(dt) {
  no.symptoms <-
    dt[, list(no.symptoms.reports = sum(no.symptoms == "t")),
      by = global_id
    ]
  dt <- merge(dt, no.symptoms, by = "global_id", all.x = TRUE)
  dt <- dt[no.symptoms.reports > 0]
  dt[, no.symptoms.reports := NULL]
  dt
}

## ---- background survey ------------------------------------------------------

clean_background <- function(dt, age.breaks) {
  dt <- add_age(dt, age.breaks)
  convert_household_counts(dt)
  add_living_with_children(dt)
  dt <- add_postcode_regions(dt)
  add_highest_education(dt)
  add_household_members(dt)
  dt
}

## calculate birthdates, age and agegroup
add_age <- function(dt, age.breaks) {
  if ("birthmonth" %in% colnames(dt)) {
    dt[, birthdate := as.Date(paste0(birthmonth, "-01"))]
  } else {
    dt[, birthyear := gsub("-.*$", "", birthyear)]
    dt[, birthdate := as.Date(paste0(birthyear, "-01-01"))]
  }
  dt[, age := interval(birthdate, date) %/% years(1)]
  ## remove negative ages
  dt[age < 0, birthdate := NA]
  dt[age < 0, age := NA]

  ## ages are whole years. Each group includes its upper limit and the next
  ## group starts one year later, as in the contact age bands
  age.breaks <- sort(age.breaks, na.last = TRUE)
  agegroup_labels <- paste0(
    c(0, age.breaks + 1), "-", c(age.breaks, "")
  )
  agegroup_labels[length(agegroup_labels)] <-
    paste0(if (length(age.breaks)) max(age.breaks) + 1 else 0, "+")
  dt[, agegroup := cut(
    age,
    breaks = c(-Inf, age.breaks, Inf),
    labels = agegroup_labels
  )]
  dt
}

convert_household_counts <- function(dt) {
  for (
    household.column in grep("^nb\\.household", colnames(dt), value = TRUE)
  ) {
    if (is.factor(dt[, get(household.column)])) {
      dt[
        ,
        paste(household.column) :=
          as.integer(as.character(get(household.column)))
      ]
    }
  }
  invisible(dt)
}

## Set missing household counts to zero. In the 2009/10 and 2010/11 seasons the
## background questionnaire only asks for the number of household members in
## each age band, and a blank answer means nobody in the household is in that
## band. Modifies the counts by reference.
fill_household_counts <- function(dt) {
  for (nb.name in grep("^nb\\.", colnames(dt), value = TRUE)) {
    dt[is.na(get(nb.name)), paste(nb.name) := 0]
  }
  invisible(dt)
}

## calculate auxiliary variable: living with children
add_living_with_children <- function(dt) {
  if ("household.0.4" %in% colnames(dt)) {
    dt[
      ,
      living.with.children :=
        as.integer((household.0.4 == "t" | household.5.18 == "t"))
    ]
  } else {
    fill_household_counts(dt)
    dt[
      ,
      living.with.children :=
        as.integer((nb.household.0.4 > 0 | nb.household.5.18 > 0))
    ]
  }
  invisible(dt)
}

## clean postcodes and add regional and urban/rural information
add_postcode_regions <- function(dt) {
  urban_rural_data <- copy(urban_rural)
  urban_rural_names <- colnames(urban_rural_data)
  regions_data <- copy(regions)
  regions_data[region == "M99999999", region := "Isle of Man"]
  regions_names <- colnames(regions_data)
  for (col in grep("postcode$", colnames(dt), value = TRUE)) {
    ## clean postcode
    dt[, paste(col) := toupper(sub("[[:blank:]]+$", "", get(col)))]
    dt[!nzchar(as.character(get(col))), paste(col) := NA]
    ## set settlement type and country information
    col_prefix <- sub("postcode$", "", col)
    setnames(
      urban_rural_data, seq_along(urban_rural_names),
      paste0(col_prefix, urban_rural_names)
    )
    dt <- merge(dt, urban_rural_data, by = col, all.x = TRUE)
    dt[, paste0(col_prefix, "country") := factor(revalue(
      x = get(paste0(col_prefix, "country")),
      replace = c(
        "W" = "wales",
        "N" = "northern_ireland",
        "S" = "scotland",
        "E" = "england"
      ),
      warn_missing = FALSE
    ))]
    ## set regional information
    setnames(
      regions_data, seq_along(regions_names),
      paste0(col_prefix, regions_names)
    )
    dt <- merge(dt, regions_data, by = col, all.x = TRUE)
    dt[, paste0(col_prefix, "region") := factor(tolower(gsub(
      " ", "_",
      get(paste0(col_prefix, "region")),
      fixed = TRUE
    )))]

    add_urban_rural(dt, col_prefix)
  }
  dt
}

## Classifies settlements as urban or rural; the codes depend on the country.
add_urban_rural <- function(dt, col_prefix) {
  settlement_type <- paste0(col_prefix, "settlement.type")
  urban_rural_col <- paste0(col_prefix, "urban.rural")
  ## the urban and rural settlement types of each country
  settlement_types <- list(
    list(c("england", "wales"), c(1, 5), c(2, 3, 4, 6, 7, 8)),
    list("scotland", c(1, 2), c(3, 4, 5, 6, 7)),
    list("northern_ireland", c(1, 2, 3, 4), c(5, 6, 7))
  )
  for (types in settlement_types) {
    dt[
      country %in% types[[1]] & get(settlement_type) %in% types[[2]],
      paste(urban_rural_col) := 1
    ]
    dt[
      country %in% types[[1]] & get(settlement_type) %in% types[[3]],
      paste(urban_rural_col) := 0
    ]
  }

  dt[, paste(urban_rural_col) := factor(
    get(urban_rural_col),
    levels = c(0, 1),
    labels = c("rural", "urban")
  )]
  invisible(dt)
}

## highest education level
add_highest_education <- function(dt) {
  dt[, highest.education := NA_character_]
  edu_columns <-
    grep("^(no\\.)?education(\\.|$)", colnames(dt), value = TRUE)
  for (col in edu_columns) {
    dt[get(col) == "t", highest.education := col]
  }
  dt[, highest.education := factor(highest.education,
    levels = c(
      "no.education", "education.gcse",
      "education.alevels", "education.bsc",
      "education.msc", "education.stillin"
    )
  )]
  if ("education" %in% colnames(dt)) {
    dt[
      is.na(highest.education) & !is.na(education),
      highest.education := education + 1L
    ]
  }
  invisible(dt)
}

## household members
add_household_members <- function(dt) {
  hh_columns <- grep("^nb.household\\.", value = TRUE, colnames(dt))
  for (col in hh_columns) {
    indicator_col <- sub("nb.", "", col, fixed = TRUE)
    if (indicator_col %in% colnames(dt)) {
      dt[get(indicator_col) == "f", paste(col) := 0]
    }
  }
  dt[, nb.household := rowSums(.SD),
    .SDcols = hh_columns
  ]
  dt[, nb.household.children := rowSums(.SD),
    .SDcols = c("nb.household.0.4", "nb.household.5.18")
  ]
  invisible(dt)
}

## ---- contact survey ---------------------------------------------------------

clean_contact <- function(dt, ranges) {
  ## loop over conversational/physical variables, check for dashes, remove text
  types <- c("conversational", "physical")
  contact_columns <-
    grep(paste0("^", "(", paste(types, collapse = "|"), ")\\."),
      value = TRUE, colnames(dt)
    )

  for (column in contact_columns) {
    resolve_contact_ranges(dt, column, ranges)
  }
  add_contact_totals(dt, types, contact_columns)

  ## the survey asks about the previous day, so contacts date from the day
  ## before the report
  dt[, contact.date := date - 1]
  dt[, contact.day.of.week := (wday(contact.date) - 2) %% 7 + 1]
  dt
}

## Turns the answers of one contact column into integers; a range such as
## "5-10" is replaced by a sample from it or by a summary of it.
resolve_contact_ranges <- function(dt, column, ranges) {
  dt[, paste(column) := as.character(get(column))]
  dt[get(column) == "NULL", paste(column) := NA_character_]
  if (ranges == "sample") {
    dt[
      grepl("-", get(column), fixed = TRUE),
      paste(column) := as.character(as.integer(runif(
        .N,
        as.integer(sub("-.*$", "", get(column))),
        as.integer(sub("^.*-", "", get(column))) + 1
      )))
    ]
  } else {
    ## apply a function
    dt[
      grepl("-", get(column), fixed = TRUE),
      paste(column) := as.character(as.integer(do.call(
        ranges,
        list(c(
          as.integer(sub("-.*$", "", get(column))),
          as.integer(sub("^.*-", "", get(column))) + 1
        ))
      )))
    ]
  }
  dt[, paste(column) := as.integer(gsub("[^0-9]", "", get(column)))]
  invisible(dt)
}

## Adds the number of contacts by type, by type and setting, and by type and
## age group.
add_contact_totals <- function(dt, types, contact_columns) {
  settings <-
    unique(sub("^.*\\.([^.]*)\\..*$", "\\1", contact_columns))
  agegroups <-
    unique(sub("^.*\\.[^.]*\\.([^.]*)$", "\\1", contact_columns))

  columns <- list()
  for (type in types) {
    type_columns <-
      grep(paste0("^", type), contact_columns, value = TRUE)
    columns[[type]] <- type_columns
    for (setting in settings) {
      setting_columns <-
        grep(paste0("^", type, "\\.", setting), contact_columns,
          value = TRUE
        )
      columns[[paste(type, setting, sep = ".")]] <- setting_columns
    }
    for (agegroup in agegroups) {
      agegroup_columns <-
        grep(paste0("^", type, "\\.[^.]*\\.", agegroup),
          contact_columns,
          value = TRUE
        )
      columns[[paste(type, agegroup, sep = ".")]] <- agegroup_columns
    }
  }

  for (column_name in names(columns)) {
    dt[,
      paste(column_name) := as.integer(rowSums(.SD, na.rm = TRUE)) *
        ifelse(rowSums(is.na(.SD)) == ncol(.SD), NA, 1L),
      .SDcols = columns[[column_name]]
    ]
  }
  invisible(dt)
}
