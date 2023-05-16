library('flusurvey')
library('tidyverse')

create <- FALSE

if (create) {
  dt <- extract_data(file.path("data", "flusurvey_raw_2010_2019.rds"), years=2012:2019, surveys=c("background", "symptom"), clean = c("remove.first", "limit.season", "n.reports", "unsuccessful.join", "only.symptoms"))

  bouts <- bouts_of_illness(dt, symptomatic.only=FALSE)
  saveRDS(bouts, "data/bouts_risk_factors.rds")
}

bouts <- readRDS("data/bouts_risk_factors.rds")

df_reduced <- bouts %>%
  as_tibble() %>%
  select(date,
         participant_id,
         season,
         starts_with("ili"),
         symptoms.start.date,
         symptoms.end.date,
         postcode,
         urban.rural,
         age,
         gender,
         vaccine.this.year,
         main.activity,
         occupation,
         highest.education,
         region,
         frequent.contact.children,
         nb.household,
         nb.household.children,
         pets.none,
         pets.dogs,
         pets.cats,
         pets.birds,
         pets.other,
         starts_with("risk."),
         smoke,
         transport,
         min.date,
         max.date,
         health.score) %>%
  mutate(any.ili = (!is.na(ili.symptoms) & ili.symptoms == "t") |
           (!is.na(ili) & ili == "t") |
           (!is.na(ili.fever) & ili.fever == "t") |
           (!is.na(ili.self) & ili.self == "t")) %>%
  mutate(symptoms.start.date =
           if_else(any.ili, symptoms.start.date, as.Date(NA_character_)),
         symptoms.end.date =
           if_else(any.ili, symptoms.end.date, as.Date(NA_character_))) %>%
  select(-any.ili) %>%
  distinct()

 saveRDS(df_reduced, "bouts_reduced.rds")

library('dplyr')
library('readr')

df_reduced <- readRDS("bouts_reduced.rds")

write_csv(df_reduced, "flusurvey_episodes.csv")

df_reduced_jenny <- bouts %>%
  tbl_df() %>%
  filter(season == "2019/20",
         ili.symptoms == "t" | ili == "t" |
         ili.fever == "t" | ili.self == "t") %>%
  mutate(duration = symptoms.end.date - symptoms.start.date,
         duration = if_else(between(as.integer(duration), 0, 14),
                            as.integer(duration), NA_integer_)) %>%
  select(participant_id,
         age,
         gender,
         starts_with("risk."),
         starts_with("ili"),
         duration,
         starts_with("contact."),
         starts_with("visit."))

write_csv(df_reduced_jenny, "flusurvey_hsb.csv")

