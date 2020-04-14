library('flusurvey')
library('tidyverse')

create <- FALSE

if (create) {
  dt <- extract_data(file.path("data", "flusurvey_raw_2010_2019.rds"), years=2012:2019, surveys=c("background", "symptom"), clean = c("remove.first", "limit.season", "n.reports", "unsuccessful.join", "only.symptoms"))

  bouts <- bouts_of_illness(dt, symptomatic.only=TRUE)
  saveRDS(bouts, "bouts_risk_factors.rds")
}

bouts <- readRDS("bouts_risk_factors.rds")

df_reduced <- bouts %>%
  tbl_df() %>%
  filter(ili.symptoms == "t" | ili == "t" |
         ili.fever == "t" | ili.self == "t") %>%
  select(participant_id,
         season,
         starts_with("ili"),
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
         nb.household.children,
         pets.none,
         pets.dogs,
         pets.cats,
         pets.birds,
         pets.other, 
         starts_with("risk."),
         smoke,
         transport)

 saveRDS(df_reduced, "bouts_reduced.rds")

library('dplyr')
library('readr')

df_reduced <- readRDS("bouts_reduced.rds") %>%
  mutate(min.health.score=ifelse(is.finite(min.health.score), min.health.score, NA))

write_csv(df_reduced, "flusurvey_episodes.csv")

df_reduced_jenny <- bouts %>%
  tbl_df() %>%
  filter(season == "2018/19",
         ili.symptoms == "t" | ili == "t" |
         ili.fever == "t" | ili.self == "t") %>%
  select(participant_id,
         season,
         starts_with("ili"),
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
         nb.household.children,
         pets.none,
         pets.dogs,
         pets.cats,
         pets.birds,
         pets.other, 
         starts_with("risk."),
         smoke,
         transport)

