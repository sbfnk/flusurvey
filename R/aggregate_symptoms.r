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
    if (any(grepl("\\.(suddenly|quickly)$", colnames(dt))))
    {
      dt[, suddenly := NA_character_]
      if ("symptoms.suddenly" %in% colnames(dt))
      {
        dt[symptoms.suddenly == "yes", suddenly := "t"]
        dt[symptoms.suddenly == "no", suddenly := "f"]
      }
      if ("fever.quickly" %in% colnames(dt)) {
          dt[(is.na(suddenly) | suddenly == "f") & fever.quickly == "yes",
             suddenly := "t"]
          dt[is.na(suddenly) & fever.quickly == "no", suddenly := "f"]
      }
      if ("fever.suddenly" %in% colnames(dt)) {
        dt[(is.na(suddenly) | suddenly == "f") & fever.suddenly == "yes",
           suddenly := "t"]
        dt[is.na(suddenly) & fever.suddenly == "no", suddenly := "f"]
      }
    } else
    {
        dt[, suddenly := "t"]
    }
    if (!("fever" %in% colnames(dt)) && "fever.temperature.range" %in% colnames(dt))
    {
        dt[, fever := as.integer(fever.temperature.range > 0)]
    }

    fever.symptoms <- intersect(c("fever", "fever.symptom"), colnames(dt))
    systemic.symptoms <- intersect(c("tired", "weakness", "headache"), colnames(dt))
    resp.symptoms <- intersect(c("sore.throat", "cough", "shortness.breath"), colnames(dt))
    gi.symptoms <- intersect(c("vomiting", "diarrhoea"), colnames(dt))

    if (length(c(fever.symptoms, systemic.symptoms)) > 0 &&
        length(resp.symptoms) > 0)
    {
        dt[, fever.symptoms := apply(dt, 1, function(x) {any(x[c(fever.symptoms)] %in% c(1, "t"))})]
        dt[, systemic.symptoms := apply(dt, 1, function(x) {any(x[c(systemic.symptoms)] %in% c(1, "t"))})]
        dt[, resp.symptoms := apply(dt, 1, function(x) {any(x[resp.symptoms] %in% c(1, "t"))})]
        dt[, fever.symptoms := ifelse(fever.symptoms, "t", "f")]
        dt[, systemic.symptoms := ifelse(systemic.symptoms, "t", "f")]
        dt[, resp.symptoms := ifelse(resp.symptoms, "t", "f")]
        dt[, general.symptoms := fever.symptoms == "t" | systemic.symptoms == "t"]
        dt[, general.symptoms := ifelse(general.symptoms, "t", "f")]
        dt[, ili.symptoms := general.symptoms == "t" & resp.symptoms == "t"]
        dt[, ili.symptoms := ifelse(ili.symptoms, "t", "f")]
        dt[, ili := ili.symptoms == "t" & suddenly == "t"]
        dt[, ili := ifelse(ili, "t", "f")]
    }

    if (length(fever.symptoms) > 0 &&
        length(systemic.symptoms) > 0 &&
        length(resp.symptoms) > 0)
    {
        dt[, ili.fever := fever.symptoms == "t" & ili == "t"]
        dt[, ili.fever := ifelse(ili.fever, "t", "f")]
    }

    if ("what.do.you.think" %in% colnames(dt))
    {
        dt[, ili.self := (what.do.you.think == 1)]
        dt[is.na(ili.self), ili.self := FALSE]
        dt[, ili.self := ifelse(ili.self, "t", "f")]
    }

    if (length(gi.symptoms) > 0)
    {
        dt[, gi := apply(dt, 1, function(x) {any(x[gi.symptoms] %in% c(1, "t"))})]
        dt[, gi := ifelse(gi, "t", "f")]
    }

    return(dt)
}

