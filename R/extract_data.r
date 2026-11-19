##' Extract flusurvey data
##'
##' This extracts flusurvey data for a given number of surveys and years (all by
##' default).
##' @param data either a (named) list of (annual, named) lists of surveys, or
##' (much easier) a filename that contains said list.
##' @param surveys the surveys to extract (e.g., "background", "symptom",
##' "contact" or a combination thereof), "all" for all of surveys that can be
##' found and joinable by \code{merge_data} (others are dropped with a warning);
##' surveys that don't exist in all years are dropped too.
##' The order given does not matter. The surveys are always joined in the order
##' background, contact, symptom (see \code{\link{merge_data}}), with one row
##' per report of the last survey in that order.
##' @param years the years to extract, "all" for all years
##' @param join whether to join the data into one big data.table in the end
##' @param ... parameters to be passed to merge_data, especially 'clean' (see
##' documentation of \code{\link{merge_data}}, by default all cleaning will be
##' performed, set 'clean' to NULL to get the raw data).
##' @return a data.table (if \code{join} is TRUE) or a list of data.tables (if
##' \code{join} is FALSE) with the data
##' @author seb
##' @examples
##' dt <- extract_data(simulated_data, years = 2013:2014)
##' table(dt$season)
##'
##' ## without the contact questionnaire
##' extract_data(simulated_data, years = 2013:2014,
##'              surveys = c("background", "symptom"))
##' @import data.table
##' @export
extract_data <- function(
  data,
  surveys = "all",
  years = "all",
  join = TRUE,
  ...
) {
  ## if data is given as filename, read them in
  if (is.character(data)) {
    data <- readRDS(data)
  }

  if ("all" %in% years) {
    years <- names(data)
  }
  ## The season label needs integer years, but names(data) is character
  years <- as.integer(years)

  if ("all" %in% surveys) {
    surveys <- Reduce(union, sapply(data, names))
    ## merge_data can only order these surveys
    unsupported <- setdiff(surveys, c("background", "contact", "symptom"))
    if (length(unsupported) > 0) {
      warning(
        "Not extracting ", toString(paste0("'", unsupported, "'")),
        ", which cannot be merged.",
        call. = FALSE
      )
      surveys <- setdiff(surveys, unsupported)
    }
  }

  years <- drop_missing_years(data, years)
  surveys <- drop_missing_surveys(data, years, surveys)

  if (length(years) == 0) {
    stop("No years to extract data from.", call. = FALSE)
  }

  if (length(surveys) == 0) {
    stop("No surveys to extract.", call. = FALSE)
  }

  res <- list()
  for (year in years) {
    year <- as.character(year)
    year_data <- data[[year]][surveys]
    res[[year]] <- merge_data(year_data, ...)
  }

  if (join) {
    res <- join_years(res, years)
  }

  res
}

## check that all the years exist
drop_missing_years <- function(data, years) {
  missing_years <- setdiff(years, names(data))
  if (length(missing_years) > 0) {
    warning(
      "Not extracting year(s) ", toString(missing_years),
      " which don't exist in 'data'.",
      call. = FALSE
    )
    years <- setdiff(years, missing_years)
  }
  years
}

## check that all the surveys exist in all the years
drop_missing_surveys <- function(data, years, surveys) {
  missing_surveys <- NULL
  for (year in years) {
    year <- as.character(year)
    if (!(all(surveys %in% names(data[[year]])))) {
      missing_surveys <-
        c(missing_surveys, setdiff(surveys, names(data[[year]])))
    }
  }
  if (length(missing_surveys) > 0) {
    missing_surveys <- unique(missing_surveys)
    warning(
      "Not extracting ", toString(paste0("'", missing_surveys, "'")),
      " survey(s) which don't exist for all years.",
      call. = FALSE
    )
    surveys <- setdiff(surveys, missing_surveys)
  }
  surveys
}

## Combines the tables of the years into one, with the season and numbers that
## identify the participants.
join_years <- function(res, years) {
  for (year in years) {
    id_cols <- grep("\\.id$", colnames(res[[as.character(year)]]),
      value = TRUE
    )
    for (id_name in id_cols) {
      res[[as.character(year)]][
        ,
        paste(id_name) := paste(year, get(id_name), sep = ".")
      ]
    }
    res[[as.character(year)]][
      ,
      season := sprintf("%s/%s", year - 1, year - 2000)
    ]
  }
  res <- rbindlist(res, use.names = TRUE, fill = TRUE)

  id_table <- data.table(
    global_id = unique(res[, global_id]),
    participant_id = seq_along(unique(res[, global_id]))
  )
  merge(res, id_table, all.x = TRUE, by = "global_id")
}
