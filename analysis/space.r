library("flusurvey")

dt <- extract_data(data = "data/flusurvey_raw_2010_2018.rds",
                   years = 2012:2018, surveys = c("background", "symptom"),
                   clean = c("remove.first", "limit.season", "n.reports", "unsuccessful.join", "only.symptoms"))

vars <- c("ili", "ili.self", "ili.fever")

dt[, vaccinated := factor(as.integer(vaccine.this.year == "yes"), levels=0:1, labels=c("f", "t"))]

space_incidence <-
  get_incidence(dt, incidence.columns = vars,
                aggregation = "season",
                by = c("postcode", "region", "agegroup", "vaccinated"))

readr::write_csv(space_incidence %>% select(-year), "spatial_incidence.csv")
