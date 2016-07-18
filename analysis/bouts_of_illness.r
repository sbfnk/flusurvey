library('data.table')

bouts <- list()
dt <- list()

for (file in c("flusurvey_201415.csv", "flusurvey_201314.csv", "flusurvey_201213.csv", "flusurvey_201112.csv"))
{
    dt[[file]] <- data.table(read.csv(file))
    dt[[file]][, date := as.Date(as.character(date))]
    dt[[file]][, symptoms.start.date := as.Date(as.character(symptoms.start.date))]
    dt[[file]][, symptoms.end.date := as.Date(as.character(symptoms.end.date))]

    no.symptoms <- dt[[file]][, list(no.symptoms.reports = sum(no.symptoms == 1)), by = id]

    dt[[file]] <- merge(dt[[file]], no.symptoms, by = "id", all.x = TRUE)
    dt[[file]] <- dt[[file]][no.symptoms.reports > 0]
    dt[[file]][, no.symptoms.reports := NULL]

    if ("health.score" %in% colnames(dt[[file]]))
    {
        baselines <- dt[[file]][no.symptoms == 1,
                        list(baseline.health.score = as.numeric(median(health.score, na.rm = TRUE))),
                        by = id]
        dt[[file]] <- merge(dt[[file]], baselines, by = "id", all.x = TRUE)
    }

    setkey(dt[[file]], id, date)

    ids <- unique(dt[[file]][, id])
    ids <- ids[!is.na(ids)]

    bouts[[file]] <- copy(dt[[file]])[0]
    symptoms <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain ","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")

    for (this.id in ids)
    {
        cat(this.id, "\n")
        ## remove initial illness without starting date
        participant <- dt[[file]][id == this.id]
        ## group into bouts[[file]]
        participant[, new.bout := (same == 1)]
        if (any(participant[, no.symptoms] == 1))
        {
            first.no.symptoms <- min(which(participant[, no.symptoms] == 1))
            if (first.no.symptoms > 1 &
                any(participant[seq_len(first.no.symptoms - 1),
                                !is.na(symptoms.start.date)]))
            {
                min.start <- min(which(participant[seq_len(first.no.symptoms - 1),
                                                   !is.na(symptoms.start.date)]))
                participant[min.start, new.bout := TRUE]
                if (min.start > 1)
                {
                    participant <- participant[-seq_len(min.start - 1)]
                }
            }
            participant[is.na(new.bout), new.bout := FALSE]
            if (nrow(participant) > 1)
            {
                participant[2:nrow(participant),
                            previous.no.symptoms := participant[1:(nrow(participant) - 1),
                                                                no.symptoms]]
                participant <- participant[!is.na(previous.no.symptoms) & previous.no.symptoms == 1 & no.symptoms == 0, new.bout := TRUE]
            }
            participant[, bout := cumsum(new.bout)]
            participant <- participant[no.symptoms == 0]
            for (this.bout in unique(participant[, bout]))
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
                    df_bout[nrow(df_bout), symptoms.start.date := df_bout[1, date]]
                }

                if (is.na(df_bout[nrow(df_bout), symptoms.end.date]) &
                    any(!is.na(df_bout[, symptoms.end.date])))
                {
                    max.sd <- max(which(!is.na(df_bout[, symptoms.end.date])))
                    df_bout[nrow(df_bout), symptoms.end.date :=
                                               df_bout[max.sd, symptoms.end.date]]
                } else if (all(is.na(df_bout[, symptoms.end.date])))
                {
                    df_bout[nrow(df_bout), symptoms.end.date := df_bout[1, date]]
                }

                columns <- which(colnames(df_bout) %in% symptoms)
                bout.symptoms <- as.list(apply(df_bout[, columns, with = FALSE], 2,
                                               function(x) (as.integer(sum(x) > 0))))
                df_bout[nrow(df_bout), colnames(df_bout)[columns] := bout.symptoms]

                if ("health.score" %in% colnames(df_bout))
                {
                    df_bout[nrow(df_bout), health.score :=
                                               min(df_bout[, health.score])]
                }
                
                df_bout[, new.bout := NULL]
                df_bout[, previous.no.symptoms := NULL]
                df_bout[, bout := NULL]
                bouts[[file]] <- rbind(bouts[[file]], df_bout[nrow(df_bout)])
            }
        }
    }

    bouts[[file]][, symptoms.start.week := NULL]
    bouts[[file]][, week := NULL]
    bouts[[file]][, postcode := NULL]
    bouts[[file]][, work.postcode := NULL]
    bouts[[file]][, work.postcode.option := NULL]
    bouts[[file]][, date := NULL]
    bouts[[file]][, howhear.who := NULL]
    bouts[[file]][, using.transport := NULL]
    bouts[[file]][, atrisk := 1 - norisk]
    bouts[[file]][, norisk := NULL]

    bouts[[file]][, suddenly := 1]
    bouts[[file]][is.na(symptoms.suddenly) & is.na(fever.suddenly), suddenly := NA]
    bouts[[file]][is.na(symptoms.suddenly) & fever.suddenly > 0, suddenly := 0]
    bouts[[file]][is.na(fever.suddenly) & symptoms.suddenly > 0, suddenly := 0]
    bouts[[file]][fever.suddenly > 0 & symptoms.suddenly > 0, suddenly := 0]

    bouts[[file]][, ili := ((suddenly == 1) &
                   (fever == 1 | tired == 1 |
                        headache == 1 | muscle.and.or.joint.pain == 1) &
                   (sore.throat == 1 | cough ==1 |
                        shortness.breath == 1))]
    bouts[[file]][, ili := as.integer(ili)]

    bouts[[file]][, ili.fever := ((suddenly == 1) &
                          (fever == 1) &
                          (sore.throat == 1 | cough ==1 |
                               shortness.breath == 1))]
    bouts[[file]][, ili.fever := as.integer(ili.fever)]

    bouts[[file]] <- bouts[[file]][symptoms.start.date <= symptoms.end.date]
    bouts[[file]][, bout.id := 1:nrow(bouts[[file]])]

    saveRDS(bouts[[file]], sub("csv", "rds", sub("flusurvey", "bouts[[file]]", file)))
    write.table(bouts[[file]], sub("flusurvey", "bouts[[file]]", file), quote = TRUE, sep = ",", row.names = FALSE)
}
