##' Aggregate symptoms in symptom table to work out ILI etc.
##'
##' @param symptoms 
##' @return a data table with added columns
##' @author seb
##' @import data.table
##' @export
aggregate_symptoms <- function(symptoms, ili.sudden.unknown = 1)
{
    dt <- copy(data.table::data.table(symptoms))
    ## calculate ili
    if ("symptoms.suddenly" %in% colnames(dt))
    {
        dt[symptoms.suddenly == "yes", suddenly := 1]
        dt[symptoms.suddenly == "no", suddenly := 0]
        dt[(is.na(suddenly) | suddenly == 0) & fever.suddenly == "yes",
           suddenly := 1]
        dt[is.na(suddenly) & fever.suddenly == "no", suddenly := 0]
    } else
    {
        dt[, suddenly := 1]
    }
    if (!("fever" %in% colnames(dt)) && "fever.temperature.range" %in% colnames(dt))
    {
        dt[, fever := as.integer(fever.temperature.range > 0)]

    }

    fever.symptoms <- intersect(c("fever", "fever.symptom"), colnames(dt))
    ili.symptoms <- intersect(c("tired", "weakness", "headache"), colnames(dt))
    resp.symptoms <- intersect(c("sore.throat", "cough", "shortness.breath"), colnames(dt))
    gi.symptoms <- intersect(c("vomiting", "diarrhoea"), colnames(dt))

    if (length(c(fever.symptoms, ili.symptoms)) > 0 &&
        length(resp.symptoms) > 0)
    {
        dt[, ili := suddenly == 1 &
                 apply(dt, 1, function(x) {any(x[c(fever.symptoms, ili.symptoms)] %in% c(1, "t"))}) &
                 apply(dt, 1, function(x) {any(x[resp.symptoms] %in% c(1, "t"))})]
        dt[, ili := as.integer(ili)]
    }

    if (length(fever.symptoms) > 0 &&
        length(ili.symptoms) > 0 &&
        length(resp.symptoms) > 0)
    {
        dt[, ili.fever := suddenly == 1 &
                 apply(dt, 1, function(x) {any(x[fever.symptoms] %in% c(1, "t"))}) &
                 apply(dt, 1, function(x) {any(x[ili.symptoms] %in% c(1, "t"))}) &
                 apply(dt, 1, function(x) {any(x[resp.symptoms] %in% c(1, "t"))})]
        dt[, ili.fever := as.integer(ili.fever)]
    }

    if ("what.do.you.think" %in% colnames(dt))
    {
        dt[, ili.self := (what.do.you.think == 0)]
        dt[is.na(ili.self), ili.self := FALSE]
        dt[, ili.self := as.integer(ili.self)]
    }

    if (length(gi.symptoms) > 0)
    {
        dt[, gi := apply(dt, 1, function(x) {any(x[gi.symptoms] %in% c(1, "t"))})]
        dt[, gi := as.integer(gi)]
    }

    dt[, suddenly := NULL]

    return(dt)
}

