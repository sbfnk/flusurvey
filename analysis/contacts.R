library('flusurvey')

dt <- extract_data("flusurvey_raw_2012_2015.rds", years=c(2012, 2013))
dt_contacts <- extract_data("flusurvey_raw_2012_2015.rds", years=c(2012, 2013), surveys="contact")
