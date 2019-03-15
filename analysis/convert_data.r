library('readr')

data_dir <- path.expand(file.path("~", "Research", "Flusurvey", "Data"))
data_files <- list.files(data_dir, "_[0-9][0-9]\\.csv$", full.names=TRUE)

year_files <- list()

for (file in data_files) {
    year <- as.character(parse_number(basename(file)) + 2000)
    if (!(year %in% names(year_files))) year_files[[year]] <- list()
    type <- sub("_.*$", "", basename(file))
    year_files[[year]][[type]] <- file
}

sep <- as.list(rep(",", length(year_files)))
names(sep) <- names(year_files)
sep[["2010"]] <- ";"

dt <- list()
for (year in names(year_files)) {
    dt[[year]] <- read_data(year_files[[year]], year, sep=sep[[year]])
}

min_year <- min(names(year_files))
max_year <- max(names(year_files))

saveRDS(dt, file.path("data", paste0(paste("flusurvey", "raw", min_year, max_year, sep="_"), ".rds")))
