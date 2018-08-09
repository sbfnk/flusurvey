library('flusurvey')
library('tidyverse')

dt <- extract_data("flusurvey_raw_2010_2018.rds", years=2012:2018,
                   surveys=c("background", "symptom"),
                   clean=c("remove.first", "remove.bad.symptom.dates",
                           "remove.bad.health.score", "guess.start.dates",
                           "limit.season", "n.reports",
                           "unsuccessful.join", "only.symptoms"))

dt %<>% ## transform variables
    mutate(date.vaccine=as.character(date.vaccine)) %>%
    mutate(date.vaccine=if_else(date.vaccine == "", NA_character_, date.vaccine)) %>%
    mutate(date.vaccine=as.Date(date.vaccine)) %>%
    mutate(season.id=paste(season, participant_id, sep="."))

## create data set for participant numbers
dbg <- dt %>%
  filter(!duplicated(background.id)) %>%
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

## create data set for analysis
dta <- dt %>%
    group_by(participant_id, season) %>%
    mutate(first.questionnaire=min(date)) %>%
    filter(row_number()==n()) %>%
    ungroup %>%
    mutate(vaccinated=!is.na(date.vaccine),
           medication=if_else(no.medication=="t", "f", "t")) %>%
    arrange(participant_id, season) %>%
    select(participant_id, season, gender, age, region, main.activity, occupation, highest.education, frequent.contact.children, frequent.contact.elderly, frequent.contact.patients, frequent.contact.none, nb.household, nb.household.children, transport, vaccine.this.year, date.vaccine, starts_with("why.vaccine"), starts_with("why.not.vaccine"), vaccine.last.year, medication, pregnant, smoke, first.questionnaire)

dta_pre_phe <- dta %>%
    filter(season <= 2015)

part_sample <- sample(unique(dta_pre_phe$participant_id), 100)

dta_sample <- dta_pre_phe %>%
    filter(participant_id %in% part_sample)

write_csv(dta_sample, "flusurvey_sample.csv")
