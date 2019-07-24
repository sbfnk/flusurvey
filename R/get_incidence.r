##' Calculate incidence from flusurvey data
##'
##' This calculates incidence according to a given column in the supplied data.
##' @param data a data table, usually generated by \code{\link{merge_data}}
##' @param incidence.columns one or more columns which specificy incidence; default: "ili"
##' @param aggregation the timescale of aggregation, by default "week"
##' @param denominator what to use as denominator, active members or all reports in a week
##' @param min.N the minimum denominator, by default 1
##' @param by one or more variables by which to group
##' @import data.table
##' @importFrom lubridate floor_date
##' @return a data.table with the incidence
##' @author seb
##' @export
get_incidence <- function(data, incidence.columns = "ili", aggregation = c("week", "day", "month", "year"), denominator = c("active.members", "reports"), min.N = 1, by = NULL)
{
    aggregation <- match.arg(aggregation)
    denominator <- match.arg(denominator)

    dt <- copy(data)
    bouts <- bouts_of_illness(data)
    ## required date columns
    columns <- c("symptoms.start.date", "date", "min.date", "max.date")
    names(columns) <- columns

    ## create new date columns if aggregation is not by day
    if (aggregation != "day")
    {
        columns <- sub("date$", paste(aggregation, "date", sep = "."), columns)

        for (col_id in seq_along(columns))
        {
          col <- columns[col_id]
          dt[, paste(col) := floor_date(get(names(col)), unit=aggregation)]
          bouts[, paste(col) := floor_date(get(names(col)), unit=aggregation)]
        }
    }

    id_column <- grep("_id$", colnames(dt), value = TRUE)

    ## calculate incidence based on symptom start date
    incidence <- list()
    for (incidence_column in incidence.columns)
    {
      id_incidence <-
        bouts[get(incidence_column) == "t", list(bouts = .N),
              by = c(columns[["symptoms.start.date"]], id_column, "season", by)]
      incidence[[incidence_column]] <-
        id_incidence[, list(new.cases = .N),
                     by = c(columns[["symptoms.start.date"]], "season", by)]
      incidence[[incidence_column]][, type := incidence_column]
    }
    incidence <- rbindlist(incidence)
    setnames(incidence, columns[["symptoms.start.date"]], aggregation)
    incidence <-
      dcast(incidence,
            as.formula(paste0(paste(aggregation, by, "season", sep="+"), "~ type")),
            value.var="new.cases")
    for (incidence_column in intersect(incidence.columns, colnames(incidence)))
    {
      incidence[is.na(get(incidence_column)), paste(incidence_column) := 0]
    }

    ## work out denominator of active members
    if (denominator == "active.members")
    {
        active_reports <- lapply(unique(incidence[, get(aggregation)]), function(x)
        {
                        id_active <- dt[get(columns[["min.date"]]) <= x &
                                        get(columns[["max.date"]]) >= x,
                                        list(reports = .N),
                                        by = c(id_column, "season", by)]
                        id_active <- id_active[, .N, by = c("season", by)]
                        id_active[, paste(aggregation) := x]
                        id_active
        })
        active_reports <- rbindlist(active_reports)
        incidence <-
            merge(incidence, active_reports,
                  by = c(aggregation, "season", by), all.y=TRUE)
    } else
    {
        id_reports <- dt[, list(reports = .N), by = c(columns[["date"]], id_column, by)]
        reports <- id_reports[, .N, by = c(columns[["date"]], by)]
        setnames(reports, columns[["date"]], aggregation)
        incidence <- merge(incidence, reports, by = c(aggregation, by))
    }

    incidence <- incidence[N >= min.N]
    
    setkeyv(incidence, aggregation)

    return(incidence)
}
