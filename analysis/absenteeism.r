library('methods')
library('flusurvey')
library('tidyverse')

create <- FALSE

if (create) {
  dt <- extract_data(file.path("data", "flusurvey_raw_2010_2018.rds"), years=2012:2018, surveys=c("background", "symptom"))

  bouts <- bouts_of_illness(dt, symptomatic.only=TRUE)
  saveRDS(bouts, "bouts_absenteeism.rds")
}

bouts <- readRDS("bouts_absenteeism.rds")

df_reduced <- df_bouts %>%
  select(participant_id,
         season,
         gender,
         vaccine.this.year,
         starts_with("risk."),
         pregnant,
         pregnant.trimester,
         starts_with("frequent.contact."),
         children.school,
         starts_with("nb.household"),
         age,
         agegroup,
         living.with.children,
         country,
         region,
         urban.rural,
         highest.education,
         symptoms.start.date,
         symptoms.end.date,
         symptoms.suddenly,
         no.symptoms,
         fever,
         watery.eyes,
         blocked.runny.nose,
         sneezing,
         sore.throat,
         cough,
         phlegm,
         headache,
         muscle.and.or.joint.pain,
         chest.pain,
         tired,
         loss.appetite,
         nausea,
         vomiting,
         diarrhoea,
         chills,
         shortness.breath,
         stomach.ache,
         other.symptoms,
         fever.suddenly,
         fever.temperature.value,
         suddenly,
         ili,
         ili.fever,
         ili.self,
         gi,
         what.do.you.think,
         starts_with("visit.medical.service."),
         starts_with("contact.medical.service."),
         starts_with("medication."),
         alter.routine,
         howlong.altered,
         main.activity,
         occupation,
         smoke,
         transport,
         howlong.transport,
         pets.none,
         pets.dogs,
         pets.cats,
         pets.birds,
         pets.other,
         baseline.health.score,
         min.health.score
         )

 saveRDS(df_reduced, "bouts_reduced.rds")

library('dplyr')
library('readr')

df_reduced <- readRDS("bouts_reduced.rds") %>%
  mutate(min.health.score=ifelse(is.finite(min.health.score), min.health.score, NA))

write_csv(df_reduced, "flusurvey_episodes.csv")
