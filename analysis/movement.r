library('flusurvey')
library('tidyverse')

dt <- extract_data("flusurvey_raw_2010_2018.rds", years=2012:2013,
                   surveys=c("background", "symptom", "contact"))

movement <- dt %>%
  tbl_df %>%
  mutate(symptoms=fct_recode(no.symptoms, yes="f", no="t"),
         ili.fever=fct_recode(ili.fever, yes="t", no="f")) %>%
  select(participant_id, season,
         `conversational.0-4`,
         `conversational.5-18`,
         `conversational.19-44`,
         `conversational.45-64`,
         `conversational.65+`,
         `physical.0-4`,
         `physical.5-18`,
         `physical.19-44`,
         `physical.45-64`,
         `physical.65+`,
         furthest.travelled,
         date,
         age,
         gender,
         vaccinated=vaccine.this.year,
         health.score,
         ili.fever,
         symptoms)

saveRDS(movement, "movement.rds")
write_csv(movement, "movement.csv")
