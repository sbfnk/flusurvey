library('flusurvey')

dt <- extract_data("flusurvey_raw_2010_2015.rds", years=2010:2013)
dt_contacts <- extract_data("flusurvey_raw_2010_2015.rds", years=2010:2013, surveys="contact")
