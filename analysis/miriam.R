library('flusurvey')
library('readr')

rd12 <- read_data(list(contact = "~/Research/FluSurvey/Data/contact_12.csv"), 2012)
rd13 <- read_data(list(contact = "~/Research/FluSurvey/Data/contact_13.csv"), 2013)

contact12 <- rd12$contact[, c("date", setdiff(colnames(rd$contact), c("date", "timestamp"))), with = FALSE]
contact13 <- rd13$contact[, c("date", setdiff(colnames(rd$contact), c("date", "timestamp"))), with = FALSE]

write_csv(contact12, "contacts_201213.csv")
write_csv(contact12, "contacts_201314.csv")
