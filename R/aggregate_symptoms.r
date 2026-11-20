## Record the onset as answered: "sudden", "not.sudden", "dont_know" (explicit
## answer), "not.answered" (question asked, no answer) or "no.question" (no
## onset column at all). The fever questions settle onset only where they
## answer it.
onset_status <- function(dt) {
  n <- nrow(dt)
  onset.columns <- grep("\\.(suddenly|quickly)$", colnames(dt), value = TRUE)
  if (length(onset.columns) == 0) {
    return(rep("no.question", n))
  }
  status <- rep("not.answered", n)
  if ("symptoms.suddenly" %in% colnames(dt)) {
    status[which(dt$symptoms.suddenly == "dont_know")] <- "dont_know"
    status[which(dt$symptoms.suddenly == "yes")] <- "sudden"
    status[which(dt$symptoms.suddenly == "no")] <- "not.sudden"
  }
  for (fever.onset in intersect(c("fever.quickly", "fever.suddenly"),
                                colnames(dt))) {
    answer <- dt[[fever.onset]]
    not.yet <- status != "sudden"
    status[which(not.yet & answer == "yes")] <- "sudden"
    settled <- status %in% c("dont_know", "not.answered")
    status[which(settled & answer == "no")] <- "not.sudden"
  }
  status
}

##' Aggregate symptoms in symptom table to work out ILI etc.
##'
##' Aggregate symptoms in symptom table to work out ILI etc.
##'
##' The recorded onset is kept in the column \code{onset}: "sudden",
##' "not.sudden", "dont_know" (an explicit answer), "not.answered" (the question
##' was asked but not answered) or "no.question" (the data have no onset
##' question). None of the last three establishes sudden onset.
##' \code{ili.sudden.unknown} sets how the last three are interpreted in the
##' derived columns \code{suddenly}, \code{ili} and \code{ili.fever}. It leaves
##' \code{onset} and the answers in the data unchanged. A report that already
##' fails another ILI criterion is not ILI, whatever its onset.
##'
##' @param symptoms a data table of symptom reports
##' @param ili.sudden.unknown how to interpret reports where onset is unknown
##'   (see above): \code{NA} (the default) leaves \code{suddenly}, \code{ili}
##'   and \code{ili.fever} missing where onset is needed to decide, \code{1}
##'   counts unknown onset as sudden and \code{0} counts it as not sudden.
##' @return a data table with added columns, including \code{onset}
##' @author seb
##' @import data.table
##' @export
aggregate_symptoms <- function(symptoms, ili.sudden.unknown = NA) {
  dt <- copy(data.table(symptoms))
  ## calculate ili
  onset <- onset_status(dt)
  set(dt, j = "onset", value = onset)
  dt[, suddenly := fcase(
    onset == "sudden", "t",
    onset == "not.sudden", "f",
    default = NA_character_
  )]
  if (!is.na(ili.sudden.unknown)) {
    dt[is.na(suddenly), suddenly := ifelse(ili.sudden.unknown == 1, "t", "f")]
  }
  needs_fever <- !("fever" %in% colnames(dt)) &&
    "fever.temperature.range" %in% colnames(dt)
  if (needs_fever) {
    dt[, fever := as.integer(fever.temperature.range > 0)]
  }

  fever.symptoms <- intersect(c("fever", "fever.symptom"), colnames(dt))
  systemic.symptoms <- intersect(
    c("tired", "weakness", "headache"),
    colnames(dt)
  )
  resp.symptoms <- intersect(
    c("sore.throat", "cough", "shortness.breath"),
    colnames(dt)
  )
  gi.symptoms <- intersect(c("vomiting", "diarrhoea"), colnames(dt))

  has_ili_columns <- length(c(fever.symptoms, systemic.symptoms)) > 0 &&
    length(resp.symptoms) > 0
  if (has_ili_columns) {
    dt[, fever.symptoms := apply(dt, 1, function(x) {
      any(x[c(fever.symptoms)] %in% c(1, "t"))
    })]
    dt[, systemic.symptoms := apply(dt, 1, function(x) {
      any(x[c(systemic.symptoms)] %in% c(1, "t"))
    })]
    dt[, resp.symptoms := apply(dt, 1, function(x) {
      any(x[resp.symptoms] %in% c(1, "t"))
    })]
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

  has_ili_fever_columns <- length(fever.symptoms) > 0 &&
    length(systemic.symptoms) > 0 &&
    length(resp.symptoms) > 0
  if (has_ili_fever_columns) {
    dt[, ili.fever := fever.symptoms == "t" & ili == "t"]
    dt[, ili.fever := ifelse(ili.fever, "t", "f")]
  }

  if ("what.do.you.think" %in% colnames(dt)) {
    dt[, ili.self := (what.do.you.think == "ili")]
    dt[is.na(ili.self), ili.self := FALSE]
    dt[, ili.self := ifelse(ili.self, "t", "f")]
  }

  if (length(gi.symptoms) > 0) {
    dt[, gi := apply(dt, 1, function(x) {
      any(x[gi.symptoms] %in% c(1, "t"))
    })]
    dt[, gi := ifelse(gi, "t", "f")]
  }

  dt
}
