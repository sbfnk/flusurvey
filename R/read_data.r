##' This reads in flusurvey data for a given year
##'
##' This reads different surveys and returns a list of data tables
##' containing the surveys. It does minimal cleaning and conversion
##' into R data formats. The csv files that should be provided can be
##' obtained via SQL dump,  e.g.
##' \copy pollster_results_intake to 'filename.csv' csv header
##'
##' and similarly for weekly surveys
##'
##' @param files a (named!) list of files. The names stand for the surveys (e.g., "symptom", "background", "contact", etc.)
##' @param year the year from which the data come
##' @param ... options to be passed to \code{read.csv}
##' @return a list of data tables with the data
##' @author seb
##' @import data.table
##' @importFrom lubridate parse_date_time
##' @export
read_data <- function(files, year, ...)
{
    res <- list()
    for (name in names(files))
    {
        dt <- data.table(read.csv(files[[name]], ...))

        ## remove empty variables
        fields <- copy(colnames(dt))
        for (col in fields)
        {
            if (all(is.na(dt[, get(col)])) || all(is.null(dt[, get(col)])))
            {
                dt[, paste(col) := NULL]
            }
        }

        year <- as.character(year)

        ## convert question name
        setnames(dt, names(flusurvey::questions[[year]][[name]]),
                 flusurvey::questions[[year]][[name]])

        ## convert dates
        if ("timestamp" %in% colnames(dt)) dt[, date := as.Date(timestamp)]

        for (col in grep("date$", colnames(dt), value = TRUE))
        {
            if (is.character(dt[, get(col)]) | is.factor(dt[, get(col)])) {
                dt[get(col) == "", paste(col) := NA_character_]
                dt[get(col) == "None", paste(col) := NA_character_]
                dt[, paste(col) := as.Date(lubridate::parse_date_time(get(col), orders=c("Ymd", "dmY", "Ymd HMS")))]
            }
        }

        ## convert options
        for (option in intersect(colnames(dt), names(flusurvey::options)))
        {
          dt[, paste(option) :=
                 factor(get(option),
                        levels=names(flusurvey::options[[option]]),
                        labels=flusurvey::options[[option]])]
        }

        ## convert integer 0/1 to character t/f
        for (col in colnames(dt)) {
            if (length(na.omit(setdiff(unique(dt[[col]]), c(0, 1)))) == 0) {
                dt[, paste(col) := factor(get(col), levels=0:1, labels=c("f", "t"))]
            }
        }

        ## remove year-specificity in column names
        colnames(dt) <- sub(paste0("\\.", year, "$"), "", colnames(dt))

        res[[name]] <- dt
    }

    ids_found <- TRUE
    global_ids_present <- vapply(res, function(x) ("global_id" %in% colnames(x)), TRUE)

    if (!all(global_ids_present)) {

      ids_found <- FALSE
      alternative_ids <- c("uid", "user_id")

      i <- 0

      while (!ids_found && i < length(alternative_ids)) {
        i <- i + 1
        ids_present <- vapply(res, function(x) (alternative_ids[i] %in% colnames(x)), TRUE)
        if (all(ids_present)) {
          ids_found <- TRUE
          for (name in names(res)) {
            if ("global_id" %in% colnames(res[[name]])) res[[name]][, global_id := NULL]
            setnames(res[[name]], alternative_ids[i], "global_id")
          }
        }
      }
    }

    if (ids_found) {
      for (name in names(res)) {
        setkey(res[[name]], global_id, date)
      }
    } else {
      warning("No global ID found. Won't be able to merge")
    }

    return(res)
}

