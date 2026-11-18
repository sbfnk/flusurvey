##' This reads in flusurvey data for a given year
##'
##' This reads different surveys and returns a list of data tables
##' containing the surveys. It does minimal cleaning and conversion
##' into R data formats. The csv files that should be provided can be
##' obtained via SQL dump,  e.g.
##' \\copy pollster_results_intake to 'filename.csv' csv header
##'
##' and similarly for weekly surveys
##'
##' @param files a (named!) list of files. The names stand for the surveys
##' (e.g., "symptom", "background", "contact", etc.)
##' @param year the year from which the data come
##' @param ... options to be passed to \code{read.csv}
##' @return a list of data tables with the data
##' @author seb
##' @import data.table
##' @importFrom lubridate parse_date_time
##' @importFrom stats na.omit
##' @importFrom utils read.csv
##' @export
read_data <- function(files, year, ...) {
  year <- as.character(year)
  res <- list()
  for (name in names(files)) {
    res[[name]] <- read_survey(files[[name]], name, year, ...)
  }

  res <- use_global_id(res)
  if (all_have_global_id(res)) {
    for (name in names(res)) {
      setkey(res[[name]], global_id, date)
    }
  }
  res
}

## Reads one survey file and converts it into R data formats.
read_survey <- function(file, name, year, ...) {
  dt <- data.table(read.csv(file, ...))
  remove_empty_columns(dt)

  ## convert question name
  setnames(
    dt, names(flusurvey::questions[[year]][[name]]),
    flusurvey::questions[[year]][[name]]
  )

  convert_date_columns(dt)
  convert_option_columns(dt)
  convert_binary_columns(dt)

  ## remove year-specificity in column names
  colnames(dt) <- sub(paste0("\\.", year, "$"), "", colnames(dt))
  dt
}

## Removes the columns without any answers, by reference.
remove_empty_columns <- function(dt) {
  fields <- copy(colnames(dt))
  for (col in fields) {
    if (all(is.na(dt[, get(col)])) || all(is.null(dt[, get(col)]))) {
      dt[, paste(col) := NULL]
    }
  }
  invisible(dt)
}

## Converts the timestamp and the columns holding dates into dates, by
## reference.
convert_date_columns <- function(dt) {
  if ("timestamp" %in% colnames(dt)) dt[, date := as.Date(timestamp)]

  for (col in grep("date$", colnames(dt), value = TRUE)) {
    if (is.character(dt[, get(col)]) || is.factor(dt[, get(col)])) {
      dt[!nzchar(as.character(get(col))), paste(col) := NA_character_]
      dt[get(col) == "None", paste(col) := NA_character_]
      dt[, paste(col) := as.Date(
        parse_date_time(
          get(col),
          orders = c("Ymd", "dmY", "Ymd HMS")
        )
      )]
    }
  }
  invisible(dt)
}

## Converts the answers to multiple choice questions into factors, by
## reference.
convert_option_columns <- function(dt) {
  for (option in intersect(colnames(dt), names(flusurvey::options))) {
    dt[, paste(option) := factor(
      get(option),
      levels = names(flusurvey::options[[option]]),
      labels = flusurvey::options[[option]]
    )]
  }
  invisible(dt)
}

## Converts integer 0/1 to character t/f, by reference.
convert_binary_columns <- function(dt) {
  for (col in colnames(dt)) {
    if (length(na.omit(setdiff(unique(dt[[col]]), c(0, 1)))) == 0) {
      dt[, paste(col) := factor(get(col), levels = 0:1, labels = c("f", "t"))]
    }
  }
  invisible(dt)
}

all_have_global_id <- function(res) {
  all(vapply(res, function(x) "global_id" %in% colnames(x), TRUE))
}

## Makes sure every survey has a column global_id, taking an alternative if
## need be. Warns if there is none.
use_global_id <- function(res) {
  if (all_have_global_id(res)) {
    return(res)
  }

  for (alternative_id in c("uid", "user_id")) {
    ids_present <- vapply(
      res,
      function(x) (alternative_id %in% colnames(x)),
      TRUE
    )
    if (all(ids_present)) {
      for (name in names(res)) {
        if ("global_id" %in% colnames(res[[name]])) {
          res[[name]][, global_id := NULL]
        }
        setnames(res[[name]], alternative_id, "global_id")
      }
      return(res)
    }
  }

  warning("No global ID found. Won't be able to merge", call. = FALSE)
  res
}
