##' Extract bouts of illness from a flusurvey data table
##'
##' @param x the data to extract bouts from
##' @param symptomatic.only whether to only include symptomatic periods 
##' @return a data table of bouts of illness
##' @author seb
##' @import data.table
##' @importFrom utils
##' @export
bouts_of_illness <- function(x, symptomatic.only=TRUE, as.data.frame=TRUE)
{
    dt <- data.table(x)

    if ("health.score" %in% colnames(dt))
    {
        baselines <-
            dt[no.symptoms == "t",
               list(baseline.health.score =
                        as.numeric(median(health.score, na.rm = TRUE))),
               by = list(participant_id, season)]
        dt <- merge(dt, baselines, by = c("participant_id", "season"), all.x = TRUE)
    }

    dt[, id := 1:.N]
    ids <- unique(dt$participant_id)

    tf <-
      colnames(dt)[vapply(colnames(dt), function(x) {
        length(setdiff(c("t", "f"), levels(dt[[x]]))) ==  0
      }, TRUE)]

    keep_rows <- c()

    last <- function(x) x[length(x)]
    last_not_na <- function(x) x[!is.na(x)][sum(!is.na(x))]

    dt[, new.bout := (same == "no")]
    dt[, part.symptom.id := 1:.N, by=list(participant_id, season)]
    dt[, first.no.symptoms := ifelse(sum(no.symptoms == "t") > 0,
                                     min(which(no.symptoms == "t")),
                                     0),
       by=list(participant_id, season)]
    dt[, min.symptoms.start.date := as.Date(ifelse(any(!is.na(symptoms.start.date)), as.character(min(symptoms.start.date, na.rm=TRUE)), NA_character_)), by=list(participant_id, season)]
    dt[part.symptom.id < first.no.symptoms & !is.na(symptoms.start.date) &
       symptoms.start.date == min.symptoms.start.date, new.bout := TRUE,
       by=list(participant_id, season)]
    dt[first.no.symptoms == 0 & part.symptom.id == min(part.symptom.id), new.bout := TRUE]
    dt[is.na(new.bout), new.bout := FALSE]
    dt[, previous.no.symptoms := factor(c(NA_character_, as.character(no.symptoms[-.N])),
                                        levels=levels(no.symptoms))]
    dt[part.symptom.id == 1, previous.no.symptoms := NA]
    dt[!is.na(previous.no.symptoms) & previous.no.symptoms == "t" & no.symptoms == "f",
       new.bout := TRUE]
    dt[, previous.no.symptoms := NULL]
    dt[, previous.symptoms.end.date := as.Date(c(NA_character_, as.character(symptoms.end.date[-.N])))]
    dt[!new.bout & !is.na(previous.symptoms.end.date) & !is.na(symptoms.start.date) & no.symptoms == "f" & previous.symptoms.end.date < symptoms.start.date, new.bout := TRUE]
    dt[, previous.symptoms.end.date := NULL]
    dt[no.symptoms == "f", bout := cumsum(new.bout), by=list(participant_id, season)]
    dt[bout == 0, bout := NA]
    dt[, new.bout := NULL]

    dt[!is.na(bout), last.symptoms.start.date := last(symptoms.start.date),
       by=list(participant_id, season, bout)]
    dt[!is.na(bout), last.not.na.symptoms.start.date := last_not_na(symptoms.start.date),
       by=list(participant_id, season, bout)]
    dt[!is.na(bout), last.symptoms.end.date := last(symptoms.end.date),
       by=list(participant_id, season, bout)]
    dt[!is.na(bout), last.not.na.symptoms.end.date := last_not_na(symptoms.end.date),
       by=list(participant_id, season, bout)]

    dt[!is.na(bout) & !is.na(last.symptoms.start.date),
       symptoms.start.date := last.symptoms.start.date,
       by=list(participant_id, season, bout)]
    dt[!is.na(bout) & is.na(last.symptoms.start.date) & !is.na(last.not.na.symptoms.start.date),
       symptoms.start.date := last.not.na.symptoms.start.date,
       by=list(participant_id, season, bout)]
    dt[!is.na(bout) & is.na(last.not.na.symptoms.start.date),
       symptoms.start.date := date[1],
       by=list(participant_id, season, bout)]
    dt[!is.na(bout) & is.na(last.symptoms.end.date) & !is.na(last.not.na.symptoms.end.date),
       symptoms.end.date := last.not.na.symptoms.end.date,
       by=list(participant_id, season, bout)]
    dt[!is.na(bout) & is.na(last.not.na.symptoms.end.date) & date != symptoms.start.date,
       symptoms.end.date := last(date),
       by=list(participant_id, season, bout)]

    dt[, last.symptoms.start.date := NULL]
    dt[, last.not.na.symptoms.start.date := NULL]
    dt[, last.symptoms.end.date := NULL]
    dt[, last.not.na.symptoms.end.date := NULL]


    symptoms.id.column <- which(colnames(dt) == "symptom.id")
    if (symptoms.id.column > 1)
    {
        dt[!is.na(bout), min.part.symptom.id := min(part.symptom.id), by=list(participant_id, season, bout)]
        dt[!is.na(bout), max.part.symptom.id := max(part.symptom.id), by=list(participant_id, season, bout)]

        tf_columns <- intersect(colnames(dt), tf)

        cat("Merging columns:\n")
        for (column in tf_columns)
        {
            cat("  ", column, "\n")
            dt[, paste(column) := factor(as.integer(any(get(column) == "t")), levels=0:1, labels=c("f", "t")), by=list(participant_id, season, bout)]
        }
        if ("health.score" %in% colnames(dt))
        {
            dt[!is.na(bout), min.health.score := ifelse(any(!is.na(health.score)), min(health.score, na.rm=TRUE), NA_integer_), by=list(participant_id, season, bout)]
        }
        dt <- dt[is.na(min.part.symptom.id) | part.symptom.id == min.part.symptom.id]
    }
    if (symptomatic.only) {
        dt <- dt[!is.na(min.part.symptom.id)]
    }
    dt[, id := NULL]
    return(dt)
}
