##' Extract flusurvey data
##'
##' This extracts flusurvey data for a given number of surveys and years (all by default).
##' @param data either a (named) list of (annual, named) lists of surveys, or (much easier) a filename that contains said list.
##' @param surveys the surveys to extract (e.g., "background", "symptom", "contact" or a combination thereof), "all" for all of surveys that can be found; note that any surveys that don't exist in all years will be dropped
##' @param years the years to extract, "all" for all years
##' @param join whether to join the data into one big data.table in the end
##' @param ... parameters to be passed to merge_data, especially 'clean' (see documentation of \code{\link{read_data}}, by default all cleaning will be performed, set 'clean' to NULL to get the raw data).
##' @return a data.table (if \code{join} is TRUE) or a list of data.tables (if \code{join} is FALSE) with the data
##' @author seb
##' @import data.table
##' @export
extract_data <- function(data, surveys = "all", years = "all", join = TRUE, ...)
{
    ## if data is given as filename, read them in
    if (is.character(data))
    {
        data <- readRDS(data)
    }

    if ("all" %in% years)
    {
        years <- names(data)
    }

    if ("all" %in% surveys)
    {
        surveys <- Reduce(union, sapply(data, names))
    }

    ## check that all the surveys/years exist
    if (!all(years %in% names(data)))
    {
        missing <- setdiff(years, names(data))
        warning("Not extracting year(s) ", paste(missing, collapse = ", "), " which don't exist in 'data'.")
        years <- setdiff(years, missing)
    }

    missing_surveys <- c()
    for (year in years)
    {
        year <- as.character(year)
        if (!(all(surveys %in% names(data[[year]]))))
        {
            missing_surveys <-
                c(missing_surveys, setdiff(surveys, names(data[[year]])))
        }
    }
    if (length(missing_surveys) > 0)
    {
        missing_surveys <- unique(missing_surveys)
        warning("Not extracting ", paste(paste0("'", missing_surveys, "'"), collapse = ", "), " survey(s) which don't exist for all years.")
        surveys <- setdiff(surveys, missing_surveys)
    }

    if (length(years) == 0)
    {
        stop("No years to extract data from.")
    }

    if (length(surveys) == 0)
    {
        stop("No surveys to extract.")
    }

    res <- list()
    for (year in years)
    {
        year <- as.character(year)
        year_data <- data[[year]][surveys]
        res[[year]] <- merge_data(year_data, ...)
    }

    if (join)
    {
        for (year in years)
        {
            id_cols <- grep("\\.id$", colnames(res[[as.character(year)]]),
                            value=TRUE)
            for (id_name in id_cols) {
              res[[as.character(year)]][, paste(id_name) := paste(year, get(id_name), sep=".")]
            }
            res[[as.character(year)]][, season := year]
        }
        res <- rbindlist(res, use.names = TRUE, fill = TRUE)

        id_table <- data.table(global_id = unique(res[, global_id]),
                               participant_id = seq_along(unique(res[, global_id])))
        res <- merge(res, id_table, all.x = TRUE, by = "global_id")
        res[, global_id := NULL]
    }

    return(res)
}
