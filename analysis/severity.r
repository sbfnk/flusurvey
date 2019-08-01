library('flusurvey')
library('data.table')

dt <- extract_data(file.path("data", "flusurvey_raw_2010_2018.rds"), years=2012:2018, surveys=c("background", "symptom"))

symptom_bouts <- bouts_of_illness(dt, symptomatic.only=TRUE)
all_bouts <- bouts_of_illness(dt, symptomatic.only=FALSE)

reduced_symptom_bouts <- symptom_bouts[, c(1:3, 5:112, 114:136, 138:177, 221, 225)]
reduced_all_bouts <- all_bouts[, c(1:3, 5:112, 114:136, 138:177, 221, 225)]

setnames(reduced_symptom_bouts, "bout", "episode")
setnames(reduced_all_bouts, "bout", "episode")

saveRDS(reduced_symptom_bouts, "flusurvey_illness_episodes.rds")
saveRDS(reduced_all_bouts, "flusurvey_all_episodes.rds")
