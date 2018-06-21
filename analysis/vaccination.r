library('flusurvey')
library('tidyverse')

dt <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2017, surveys=c("background", "symptom"),
                   clean=c("remove.first", "remove.bad.symptom.dates", "remove.bad.health.score",
                           "guess.start.dates", "limit.season", "create.numeric.id", "n.reports",
                           "unsuccessful.join", "only.symptoms"))

dbg <- dt %>%
  filter(!duplicated(background.id)) %>%
  mutate(date.vaccine=as.character(date.vaccine)) %>%
  mutate(date.vaccine=if_else(date.vaccine == "", NA_character_, date.vaccine)) %>%
  mutate(date.vaccine=as.Date(date.vaccine)) %>%
  mutate(season.id=paste(season, participant_id, sep=".")) %>%
  group_by(participant_id, season) %>%
  mutate(nsurvey.season=n()) %>%
  ungroup %>%
  group_by(participant_id) %>%
  mutate(nseason=length(unique(season))) %>%
  ungroup

## number of seasons per participant
dbg %>%
  filter(!duplicated(participant_id)) %>%
  group_by(nseason) %>%
  summarise(n())

## number of participants per year
dbg %>%
  filter(!duplicated(season.id)) %>%
  group_by(season) %>%
  summarise(n())

## date of vaccine present
dbg %>%
  filter(!duplicated(participant_id)) %>%
  group_by(region) %>%
  summarise(n()) %>%
  remove_rownames

