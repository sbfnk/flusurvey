##' Flag contact reports made on a day of illness
##'
##' A contact report describes the contacts of one day. Whether the
##' participant was ill on that day comes from their episodes of illness
##' rather than from joining the contact and symptom surveys by report date
##' (see \code{\link{merge_data}}). The function matches the contact day of
##' each contact report against the start and end dates of the episodes of
##' the participant.
##'
##' @param contacts a data table of contact reports, usually from
##' \code{extract_data(..., surveys = "contact")} or with the background
##' survey too. It needs the columns \code{global_id} and \code{contact.date}.
##' @param bouts the episodes of illness, as returned by
##' \code{\link{bouts_of_illness}} for the symptom survey of the same
##' participants (extracted without the contact survey). It needs the columns
##' \code{global_id}, \code{symptoms.start.date} and \code{symptoms.end.date}.
##' Episodes with a missing start or end date are not matched. With the
##' default \code{missing.start = "first.report"} of
##' \code{\link{bouts_of_illness}}, an episode without a reported start date
##' is taken to start at its first report and is matched from that date.
##' @return \code{contacts} as a data table with the logical column
##' \code{ill} added: \code{TRUE} if the contact day falls on or between the
##' start and end dates of any episode of the same participant, \code{FALSE}
##' otherwise (including participants without episodes). The rows are kept in
##' their original order.
##' @author sbfnk
##' @examples
##' symptoms <- extract_data(
##'   simulated_data, years = 2013, surveys = c("background", "symptom")
##' )
##' contacts <- extract_data(
##'   simulated_data, years = 2013, surveys = c("background", "contact")
##' )
##' bouts <- bouts_of_illness(symptoms)
##' table(flag_ill_contacts(contacts, bouts)$ill)
##' @import data.table
##' @export
flag_ill_contacts <- function(contacts, bouts) {
  contact_cols <- c("global_id", "contact.date")
  bout_cols <- c("global_id", "symptoms.start.date", "symptoms.end.date")
  if (!all(contact_cols %in% colnames(contacts))) {
    stop(
      "'contacts' needs the column(s) ",
      toString(paste0("'", setdiff(contact_cols, colnames(contacts)), "'")),
      ".",
      call. = FALSE
    )
  }
  if (!all(bout_cols %in% colnames(bouts))) {
    stop(
      "'bouts' needs the column(s) ",
      toString(paste0("'", setdiff(bout_cols, colnames(bouts)), "'")),
      ".",
      call. = FALSE
    )
  }

  res <- copy(data.table(contacts))
  episodes <- unique(
    data.table(bouts)[
      !is.na(symptoms.start.date) & !is.na(symptoms.end.date),
      bout_cols,
      with = FALSE
    ]
  )
  res[, flag.row := seq_len(.N)]
  ill_rows <- episodes[
    res[!is.na(contact.date), list(global_id, contact.date, flag.row)],
    on = list(
      global_id,
      symptoms.start.date <= contact.date,
      symptoms.end.date >= contact.date
    ),
    nomatch = NULL
  ]$flag.row
  res[, ill := flag.row %in% ill_rows]
  res[, flag.row := NULL]
  res[]
}
