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

        if (!("global_id" %in% colnames(dt))) {
          if ("uid" %in% colnames(dt)) {
            setnames(dt, "uid", "global_id")
          } else if ("user_id" %in% colnames(dt))  {
            setnames(dt, "user_id", "global_id")
          }
        }
        setkey(dt, global_id, date)

        res[[name]] <- dt
    }

    return(res)
}

