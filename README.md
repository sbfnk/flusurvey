# flusurvey

<!-- badges: start -->
[![R-CMD-check](https://github.com/sbfnk/flusurvey/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sbfnk/flusurvey/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/sbfnk/flusurvey/branch/main/graph/badge.svg)](https://app.codecov.io/gh/sbfnk/flusurvey)
<!-- badges: end -->

An R package for analysing data from FluSurvey, the UK online cohort survey of influenza-like illness. Participants fill in a background questionnaire when they join and a symptom questionnaire every week during the flu season. In some seasons they also reported their social contacts.

The package takes the questionnaire responses as they were collected and returns one cleaned table with a row per weekly report, from which it estimates incidence and identifies episodes of illness. It is intended for anyone who holds FluSurvey responses and wants routine surveillance outputs or a consistent starting point for research.

## Contents

- Linking of the background, symptom and contact questionnaires for each participant, within and across seasons.
- Cleaning rules for the common problems of self-reported cohort data, which can be applied selectively.
- Case definitions computed from the reported symptoms: influenza-like illness (`ili`), ILI with fever (`ili.fever`), self-assessed ILI (`ili.self`) and gastrointestinal illness (`gi`).
- Incidence by day, week, month or year, stratified by any participant characteristic.
- Episodes of illness with start and end dates, built from consecutive symptomatic reports.
- Participant characteristics derived from the background questionnaire: age and age group, household size, highest level of education, and region and urban or rural setting from the postcode district.
- Numbers of conversational and physical contacts by setting and age group.

The scripts in `analysis/` use these tables for individual studies.

## Installation

```r
install.packages(c("remotes", "data.table", "lubridate", "plyr", "here"))
remotes::install_github("sbfnk/flusurvey")
```

## Data

The survey responses are individual-level data and are not part of this repository. To use the package you need the responses as one CSV file per questionnaire and season.

Seasons are labelled by the calendar year in which they end: `2013` is the 2012/13 season.

## Usage

`read_data()` reads the files of one season and returns a list with one `data.table` per questionnaire. It gives the questions readable names, converts dates and turns coded answers into labelled factors.

```r
library("flusurvey")

raw <- read_data(
  files = list(
    background = "background_13.csv",
    symptom = "symptom_13.csv"
  ),
  year = 2013
)
```

`scripts/convert_data.r` does this for a directory of files named `<questionnaire>_<yy>.csv` and saves all seasons as a single file, `flusurvey_raw_<first year>_<last year>.rds`, in the working directory. The script is part of the repository and is not installed with the package: run it from a clone, after setting `dir` at its top to the directory that holds the files. The examples below assume the file has been moved to `data/`. Keep it out of version control.

`extract_data()` takes that file, cleans each season with `merge_data()` and stacks the seasons into one table with a row per weekly report. A questionnaire that is missing from any of the requested seasons is dropped with a warning.

```r
dt <- extract_data(
  "data/flusurvey_raw_2012_2020.rds",
  years = 2012:2013,
  surveys = c("background", "symptom")
)
```

By default `merge_data()` removes the first report of each participant, reports dated before 1 November or after 1 April, postcodes, participants with fewer than three reports, participants who had symptoms in every report and symptom reports with no matching background questionnaire. It also sets inconsistent symptom start and end dates to missing. To apply only some of these steps, pass their option names as `clean` to `extract_data()`. The names are those in the default value of `clean`, shown under Usage in `?merge_data`.

`get_incidence()` computes incidence by day, week, month or year, with either the active participants or the reports received as the denominator. Use `by` to stratify.

```r
incidence <- get_incidence(dt, incidence.columns = "ili", aggregation = "week")
incidence_by_age <- get_incidence(dt, by = "agegroup")
```

`bouts_of_illness()` groups consecutive symptomatic reports of a participant into episodes with start and end dates.

```r
bouts <- bouts_of_illness(dt)
```

`aggregate_symptoms()` derives the case definitions from the individual symptoms; `merge_data()` calls it for you. `ili` requires sudden onset, a respiratory symptom (sore throat, cough or shortness of breath) and either fever or a systemic symptom (tiredness, weakness or headache). `ili.fever` requires the fever as well. `ili.self` is the participant's own assessment, and `gi` is vomiting or diarrhoea.

## Bundled data

| Object | Contents |
|---|---|
| `questions` | Question codes and their readable names, by season and questionnaire |
| `options` | Answer codes and their labels, by question |
| `regions` | Postcode districts and their regions |
| `urban_rural` | Postcode districts with country and settlement type |

`questions` and `options` are defined in `R/define_questions.r`. `regions` and `urban_rural` are built by `scripts/geography.r` from the CSV files in `inst/extdata/`.

## Analysis scripts

The scripts in `analysis/` were written for particular studies over several years. They expect the data in different forms. Some read the combined data file, most of those under an earlier name such as `flusurvey_raw_2010_2018.rds`. The older ones read separate files for each season. The most recently updated are `contacts.R`, `contacts_health_analysis.R`, `contacts_model.r` and `ili_model.r`.

## Licence

GPL-2. See `LICENSE`.
