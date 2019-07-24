library('flusurvey')

dt <- extract_data(file.path("data", "flusurvey_raw_2010_2018.rds"), years=2012:2018, surveys=c("background", "symptom"))
bouts <- bouts_of_illness(dt, symptomatic.only=TRUE)

reduced_bouts <- bouts[, c(1:3, 5:112, 114:136, 138:177, 221, 225)]

setnames(reduced_bouts, "bout", "episode")

saveRDS(reduced_bouts, "flusurvey_illness_episodes.rds")
