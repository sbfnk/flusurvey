##' Illness status of contact reports
##'
##' Looks up, for each contact report, the symptom report filed by the same
##' participant on the same day. A contact report with no symptom report on
##' that day, or from a participant without symptom data, gets a missing
##' status rather than "no symptoms".
##'
##' The symptom data should be read without the cleaning steps that drop
##' reports (e.g., \code{clean = "unsuccessful.join"} in
##' \code{\link{extract_data}}), as otherwise the status of contact reports
##' whose same-day symptom report was removed cannot be determined.
##' @param contacts a data table of contact reports with columns
##' \code{global_id} and \code{date}
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
  symptoms <- symptoms[!duplicated(symptoms[, list(global_id, date)],
    fromLast = TRUE
  )]
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
  merge(
    data.table(contacts), status, by = c("global_id", "date"), all.x = TRUE
  )
}
