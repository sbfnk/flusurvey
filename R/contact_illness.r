##' Illness status of contact reports
##'
##' Looks up, for each contact report, the symptom report filed by the same
##' participant on the day the contacts describe, \code{contact.date}. Contact
##' reports describe the previous day, which is the day before the report
##' \code{date}. A contact report with no symptom report on that day, or from
##' a participant without symptom data, gets a missing status rather than
##' "no symptoms". The reported illness dates indicate whether symptoms were
##' present on the contact day. If several reports match, a status is positive
##' if any report is positive. Otherwise it is missing if any report is
##' unknown, and negative if all reports are negative.
##'
##' Read the symptom data with a \code{clean} argument in
##' \code{\link{extract_data}} that cannot drop symptom reports. The default
##' cleaning steps (e.g., \code{"remove.first"}) can remove a symptom report,
##' and the status of a contact report from the same day then cannot be
##' determined. Use \code{clean = c("unsuccessful.join",
##' "remove.bad.symptom.dates")} to clear invalid symptom dates without
##' dropping reports when reading a single survey.
##' @param contacts a data table of contact reports with columns
##' \code{global_id} and \code{contact.date}
##' @param symptoms a data table of symptom reports, as returned by
##' \code{\link{extract_data}}, with columns \code{global_id}, \code{date},
##' \code{no.symptoms}, \code{ili}, \code{ili.self}, \code{resp.symptoms},
##' \code{symptoms.start.date}, \code{symptoms.end.date} and
##' \code{symptoms.end.option}
##' @return the contact reports with added columns \code{current_symptoms}
##' ("symptoms" or "no symptoms"), \code{current_ili} for derived ILI,
##' \code{current_ili_self} for self-reported ILI (both "ILI" or "no ILI"),
##' and \code{current_ari} ("ARI" or "no ARI"), each missing where there is no
##' symptom report on that day or the status cannot be determined
##' @examples
##' contacts <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = c("background", "contact")
##' )
##' symptoms <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = "symptom",
##'   clean = c("unsuccessful.join", "remove.bad.symptom.dates")
##' )
##' table(contact_illness(contacts, symptoms)$current_symptoms, useNA = "ifany")
##' @import data.table
##' @export
contact_illness <- function(contacts, symptoms) {
  symptoms <- data.table(symptoms)
  symptoms <- symptoms[!is.na(global_id) & !is.na(date)]
  symptoms[, on_day :=
             no.symptoms == "f" & symptoms.start.date <= date &
             (symptoms.end.date >= date |
              (is.na(symptoms.end.date) & symptoms.end.option == "still_ill"))]
  status <- symptoms[,
    list(
      current_symptoms = fifelse(
        any(on_day), "symptoms", "no symptoms"
      ),
      current_ili = fifelse(
        any(on_day & ili == "t"), "ILI", "no ILI"
      ),
      current_ili_self = fifelse(
        any(on_day & ili.self == "t"), "ILI", "no ILI"
      ),
      current_ari = fifelse(
        any(on_day & resp.symptoms == "t"), "ARI", "no ARI"
      )
    ), by = list(global_id, date)
  ]
  setnames(status, "date", "contact.date")
  merge(
    data.table(contacts), status, by = c("global_id", "contact.date"),
    all.x = TRUE
  )
}
