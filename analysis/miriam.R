library('flusurvey')
library('readr')

rd12 <- read_data(list(background = "~/Research/FluSurvey/Data/background_12.csv", contact = "~/Research/FluSurvey/Data/contact_12.csv"), 2012)
rd13 <- read_data(list(background = "~/Research/FluSurvey/Data/background_13.csv", contact = "~/Research/FluSurvey/Data/contact_13.csv"), 2013)

rd12 <- lapply(rd12, function(x) {x[, c("date", setdiff(colnames(x), c("date", "timestamp"))), with = FALSE]})
rd13 <- lapply(rd13, function(x) {x[, c("date", setdiff(colnames(x), c("date", "timestamp"))), with = FALSE]})

survey12 <- merge_data(rd12, clean = c("remove.postcodes"))
survey13 <- merge_data(rd13, clean = c("remove.postcodes"))

write_csv(survey12, "surveys_201213.csv")
write_csv(survey13, "surveys_201314.csv")
