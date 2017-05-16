##' Extract bouts of illness from a flusurvey data table
##'
##' @param x the data to extract bouts from
##' @param progress whether to display a progress bar (default: TRUE)
##' @return a data table of bouts of illness
##' @author seb
##' @import data.table
##' @importFrom utils setTxtProgressBar txtProgressBar
##' @export
bouts_of_illness <- function(x, progress=TRUE)
{
    dt <- data.table(x)

    if ("health.score" %in% colnames(dt))
    {
        baselines <-
            dt[no.symptoms == "t",
               list(baseline.health.score =
                        as.numeric(median(health.score, na.rm = TRUE))),
               by = participant_id]
        dt <- merge(dt, baselines, by = "participant_id", all.x = TRUE)
    }

    ids <- unique(dt$participant_id)
    bouts <- list()

    symptoms <-
        c("fever", "chills", "blocked.runny.nose", "sneezing",
          "sore.throat", "cough", "shortness.breath", "headache",
          "muscle.and.or.joint.pain ", "chest.pain", "tired", "loss.appetite",
          "phlegm", "watery.eyes", "nausea", "vomiting", "diarrhoea",
          "stomach.ache", "other.symptoms")

    if (progress)
    {
        pb <-
            txtProgressBar(min = 0, max = length(ids), char = ".", style = 1)
    }

    for (this.id in ids)
    {
        participant <- dt[participant_id == this.id]
        ## group into bouts
        participant[, new.bout := (same == "no")]
        no.symptoms <- participant[, no.symptoms == "t"]
        if (sum(no.symptoms) > 0) {
            first.no.symptoms <- min(which(no.symptoms))
            if (first.no.symptoms > 1 &&
                any(participant[seq_len(first.no.symptoms - 1),
                                !is.na(symptoms.start.date)]))
            {
                min.start <-
                    min(which(participant[seq_len(first.no.symptoms - 1),
                                          !is.na(symptoms.start.date)]))
                participant[min.start, new.bout := TRUE]
                if (min.start > 1)
                {
                    participant <- participant[-seq_len(min.start - 1)]
                }
            }
        } else {
            participant[1, new.bout := TRUE]
        }
        participant[is.na(new.bout), new.bout := FALSE]

        if (nrow(participant) > 1)
        {
            participant[2:nrow(participant),
                        previous.no.symptoms :=
                            participant[1:(nrow(participant) - 1),
                                        no.symptoms]]
            participant <-
                participant[!is.na(previous.no.symptoms) &
                            previous.no.symptoms == "t" &
                            no.symptoms == "f", new.bout := TRUE]
            participant[, previous.no.symptoms := NULL]
        }
        participant[no.symptoms == "f", bout := cumsum(new.bout)]
        participant[, new.bout := NULL]
        bouts <- c(bouts, list(participant[no.symptoms == "t"]))
        for (this.bout in unique(participant[!is.na(bout), bout]))
        {
            df_bout <- participant[bout == this.bout]
            if (is.na(df_bout[nrow(df_bout), symptoms.start.date]) &
                any(!is.na(df_bout[, symptoms.start.date])))
            {
                max.sd <- max(which(!is.na(df_bout[, symptoms.start.date])))
                df_bout[nrow(df_bout), symptoms.start.date :=
                                           df_bout[max.sd, symptoms.start.date]]
            } else if (all(is.na(df_bout[, symptoms.start.date])))
            {
                df_bout[nrow(df_bout), symptoms.start.date :=
                                           df_bout[nrow(df_bout), date]]
            }

            if (is.na(df_bout[nrow(df_bout), symptoms.end.date]) &
                any(!is.na(df_bout[, symptoms.end.date])))
            {
                max.sd <- max(which(!is.na(df_bout[, symptoms.end.date])))
                df_bout[nrow(df_bout), symptoms.end.date :=
                                           df_bout[max.sd, symptoms.end.date]]
            } else if (all(is.na(df_bout[, symptoms.end.date])))
            {
                if (df_bout[nrow(df_bout), max.date > date])
                {
                    df_bout[nrow(df_bout), symptoms.end.date :=
                                               df_bout[nrow(df_bout), date]]
                } else
                {
                    df_bout <- NULL
                }
            }

            if (!is.null(df_bout))
            {
                ## copy anything before symptom.id (backround etc)
                ## from first row
                symptoms.id.column <-
                    which(colnames(df_bout) == "symptom.id")
                if (symptoms.id.column > 1)
                {
                    bg_columns <- seq_len(symptoms.id.column - 1)
                }
                df_bout[nrow(df_bout), bg_columns] <-
                    df_bout[1, bg_columns, with=FALSE]

                symptom_columns <- which(colnames(df_bout) %in% symptoms)
                bout.symptoms <-
                    as.list(apply(df_bout[, symptom_columns, with = FALSE], 2,
                                  function(x)
                                  {
                                      ifelse(any(x == "t"), "t", "f")
                                  }))
                df_bout[nrow(df_bout), colnames(df_bout)[symptom_columns] :=
                                           bout.symptoms]

                if ("health.score" %in% colnames(df_bout))
                {
                    df_bout[nrow(df_bout), health.score :=
                                               min(df_bout[, health.score])]
                }

                if ("suddenly" %in% colnames(df_bout))
                {
                    df_bout[nrow(df_bout),
                            suddenly := sum(any(suddenly == 1))]
                }

                if ("ili" %in% colnames(df_bout))
                {
                    df_bout[nrow(df_bout),
                            ili := sum(any(ili == 1))]
                }

                bouts <- c(bouts, list(df_bout[nrow(df_bout)]))
            }
        }
        if (progress) setTxtProgressBar(pb, this.id)
    }
    if (progress) close(pb)
    return(rbindlist(bouts))
}
