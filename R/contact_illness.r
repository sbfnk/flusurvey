##' Illness status of contact reports
##'
##' Looks up, for each contact report, the symptom report filed by the same
##' participant on the day the contacts describe, \code{contact.date}. Contact
##' reports describe the previous day, which is the day before the report
##' \code{date}. A contact report with no symptom report on that day, or from
##' a participant without symptom data, gets a missing status rather than
##' "no symptoms".
##'
##' Read the symptom data with a \code{clean} argument in
##' \code{\link{extract_data}} that cannot drop symptom reports. The default
##' cleaning steps (e.g., \code{"remove.first"}) can remove a symptom report,
##' and the status of a contact report from the same day then cannot be
##' determined. Passing only \code{clean = "unsuccessful.join"} skips the
##' other steps and, when a single survey is read, drops no symptom reports.
##' @param contacts a data table of contact reports with columns
##' \code{global_id} and \code{contact.date}
##' @param symptoms a data table of symptom reports, as returned by
##' \code{\link{extract_data}}, with columns \code{global_id}, \code{date},
##' \code{no.symptoms}, \code{ili.self} and \code{resp.symptoms}
##' @return the contact reports with added columns \code{current_symptoms}
##' ("symptoms" or "no symptoms"), \code{current_ili} ("ILI" or "no ILI") and
##' \code{current_ari} ("ARI" or "no ARI"), each missing where there is no
##' symptom report on the day
##' @examples
##' contacts <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = c("background", "contact")
##' )
##' symptoms <- extract_data(
##'   simulated_data, years = 2013:2014, surveys = "symptom",
##'   clean = "unsuccessful.join"
##' )
##' table(contact_illness(contacts, symptoms)$current_symptoms, useNA = "ifany")
##' @import data.table
##' @export
contact_illness <- function(contacts, symptoms) {
  symptoms <- data.table(symptoms)
  last_of_day <- !duplicated(
    symptoms[, list(global_id, date)],
    fromLast = TRUE
  )
  symptoms <- symptoms[last_of_day]
  status <- symptoms[,
    list(
      global_id, date,
      current_symptoms = fifelse(
        no.symptoms == "f", "symptoms", "no symptoms"
      ),
      current_ili = fifelse(
        no.symptoms == "f" & ili.self == "t", "ILI", "no ILI"
      ),
      current_ari = fifelse(
        no.symptoms == "f" & resp.symptoms == "t", "ARI", "no ARI"
      )
    )
  ]
  setnames(status, "date", "contact.date")
  merge(
    data.table(contacts), status, by = c("global_id", "contact.date"),
    all.x = TRUE
  )
}
