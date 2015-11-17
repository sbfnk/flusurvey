dt <- extract_data(data = "flusurvey_raw_2012_2015.rds")
dt[, ili.gi := as.integer(gi == 1 & ili == 1)]

vars <- c("ili", "gi", "ili.gi")

gi_incidence <- get_incidence(dt, incidence.columns = vars, by = "agegroup")

wgi <- dcast(gi_incidence, week + season + agegroup + N ~ type, value.var = "new.cases")
for (var in vars)
{
    wgi[is.na(get(var)), paste(var) := 0]
}

write.table(wgi, "flusurvey_gi.csv", quote = FALSE, sep = ",", row.names = FALSE)
