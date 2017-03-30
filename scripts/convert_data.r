## setwd("/path/to/files/)

library('flusurvey')

files <- list.files(pattern = "(background|contact|symptom|vaccine)_[0-9][0-9].csv")

year_files <- list()
for (file in files)
{
    year <- sub("^[a-z]*_([0-9]*).csv", "\\1", file)
    survey <- sub("^([a-z]*)_[0-9]*.csv", "\\1", file)
    if (nchar(year) == 2) year <- paste0("20", year)
    if (!(year %in% names(year_files))) year_files[[year]] <- list()
    year_files[[year]][[survey]] <- file
}

data <- list()
years <- names(year_files)
for (year in years)
{
    options <- list(files=year_files[[year]], year=year)
    if (year == "2010") options[["sep"]] <- ";"
    data[[year]] <- do.call(read_data, options)
}

saveRDS(data, file = paste0("flusurvey_raw_", min(years), "_", max(years), ".rds"))
