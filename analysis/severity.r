library('flusurvey')

dt <- extract_data(data = "data/flusurvey_raw_2010_2018.rds",
                   years=2013:2018, surveys=c("background", "symptom"))


