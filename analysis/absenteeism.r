library('methods')
library('flusurvey')
library('dplyr')

dt <- extract_data("flusurvey_raw_2010_2017.rds", years=2011:2017)

bouts <- bouts_of_illness(dt, symptomatic.only=TRUE) %>%
	filter(!is.na(ili))

saveRDS(bouts, "bouts_absenteeism.rds")

