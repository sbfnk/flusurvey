## Build a small table of symptom reports in the form bouts_of_illness()
## takes, for tests that need a few exact rows.
##
## Each argument has one element per report, in the order of the reports
## (shorter ones are recycled). Dates are given as strings ("2013-01-07") or
## `Date`s. `same` is "yes", "no" or "dont_know", and `NA` for a report
## without symptoms. A report has no symptoms where `no_symptoms` is true,
## which by default is where `same` and both dates are `NA`.
make_reports <- function(date, same = NA_character_, start = NA_character_,
                         end = NA_character_, no_symptoms = NULL,
                         participant_id = "p1", season = "2012/13",
                         health_score = NA_integer_) {
  n <- length(date)
  same <- rep_len(same, n)
  start <- rep_len(start, n)
  end <- rep_len(end, n)
  if (is.null(no_symptoms)) {
    no_symptoms <- is.na(same) & is.na(start) & is.na(end)
  }
  no_symptoms <- rep_len(no_symptoms, n)
  data.table::data.table(
    symptom.id = seq_len(n),
    participant_id = rep_len(participant_id, n),
    season = rep_len(season, n),
    date = as.Date(date),
    same = factor(same, levels = c("yes", "no", "dont_know")),
    symptoms.start.date = as.Date(start),
    symptoms.end.date = as.Date(end),
    no.symptoms = factor(ifelse(no_symptoms, "t", "f"), levels = c("f", "t")),
    health.score = rep_len(as.integer(health_score), n)
  )
}
