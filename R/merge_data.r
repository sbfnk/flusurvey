##' Clean and merge flusurvey data tables
##'
##' @param data the data to merge and clean, usually the result of \code{read_data}
##' @param clean cleaning options, NULL for no cleaning, otherwise a vector of cleans to perform (by default all):
##' - 'remove.first', whether to remove everyone's first report,
##' - 'remove.bad.symptom.dates', whether to remove bad symptom dates: symptom start before date of first or end after end of last report, end of symptoms before start of symptoms, symptoms reported to have started or ended after date of report
##' - 'remove.bad.health.scores', whether to remove health scores <0 or >100
##' - 'limit.season', whether to limit a flu season to November -> April
##' - 'remove.postcodes', whether to remove postcodes
##' - 'n.reports', whether to exclude those with fewer than \code{min.reports} reports
##' - 'unsuccessful.join', whether to exclude those with unsuccesful joins (e.g. if symptoms are reported without a background survey present; the web site should have prevented this, but doesn't appear to have done so)
##' - 'only.symptoms', whether to exclude those that have no report without symptoms
##' @param min.reports minimum number of reports per user (ignored if 'min.reports' is not given as a cleaning option)
##' @param ranges what to do if a number of contacts is given as a range: sample from the range ("sample"), or apply a function, given as character string (e.g., "mean")
##' @param age.breaks a vector of limits of age groups (first age group starts at 0 years of age)
##' @return a rolling-joined data table
##' @author seb
##' @import data.table
##' @importFrom lubridate month interval years
##' @importFrom plyr revalue
##' @export
merge_data <- function(data, clean = c("remove.first", "remove.bad.symptom.dates", "remove.bad.health.score", "limit.season", "remove.postcodes", "n.reports", "unsuccessful.join", "only.symptoms"), min.reports = 3, ranges="sample", age.breaks=c(18,45,65))
{
    dt_list <- list()
    clean <- match.arg(clean, several.ok = TRUE)
    for (name in names(data))
    {
        ## only keep last report per day
        dt <- copy(data.table::data.table(data[[name]]))
        dt <- dt[!duplicated(dt[, list(global_id, date)], fromLast = TRUE)]

        if ("limit.season" %in% clean)
        {
          ## figure out date with most reports, this defines the season
            reports <- dt[, .N, date]
            max_date <- reports[N == max(N), date]
            ## season begins on 1 November before the maximum date
            if (lubridate::month(max_date) >= 11)
            {
                season_start <- as.Date(paste(year(max_date), 11, 1, sep = "-"))
            } else
            {
                season_start <- as.Date(paste(year(max_date) - 1, 11, 1, sep = "-"))
            }
            ## season ends on 1 April after the maximum date
            if (lubridate::month(max_date) <= 4)
            {
                season_end <- as.Date(paste(year(max_date), 4, 1, sep = "-"))
            } else
            {
                season_end <- as.Date(paste(year(max_date) + 1, 4, 1, sep = "-"))
            }
            dt <- dt[date >= season_start & date <= season_end]
        }

        if (name == "symptom") ## clean symptoms data
        {
            dt <- aggregate_symptoms(dt)
            ## calculate min.reports, max.reports, nReports
            dt[, nReports := .N, by = global_id]
            if ("n.reports" %in% clean)
            {
              dt <- dt[nReports >= min.reports]
            }

            dt[, min.date := min(date), by = global_id]
            dt[, max.date := max(date), by = global_id]

            ## remove first survey completed by everyone
            if ("remove.first" %in% clean)
            {
                dt <- dt[duplicated(global_id)]
            }

            if ("remove.bad.symptom.dates" %in% clean)
            {
                ## remove end date before start date
                if (length(intersect(c("symptoms.start.date", "symptoms.end.date"), colnames(dt))) == 2)
                {
                  dt[!is.na(symptoms.end.date) & !is.na(symptoms.start.date) &
                     symptoms.end.date < symptoms.start.date,
                     c("symptoms.start.date", "symptoms.end.date") :=
                       list(as.Date(NA_character_), as.Date(NA_character_))]
                }
                ## remove start date after reporting date
                if ("symptoms.start.date" %in% colnames(dt))
                {
                  dt[!is.na(symptoms.start.date) & symptoms.start.date > date,
                     symptoms.start.date := as.Date(NA_character_)]
                }
                ## remove end date after reporting date
                if ("symptoms.end.date" %in% colnames(dt))
                {
                  dt[!is.na(symptoms.end.date) & symptoms.end.date > date,
                     symptoms.end.date := as.Date(NA_character_)]
                }
            }

            if ("remove.bad.health.scores" %in% clean)
            {
                dt <- dt[health.score < 0 | health.score > 100,
                         health.score := NA_real_]
            }

            if ("only.symptoms" %in% clean)
            {
                no.symptoms <-
                    dt[, list(no.symptoms.reports = sum(no.symptoms == "t")),
                        by = global_id]
                dt <- merge(dt, no.symptoms, by = "global_id", all.x = TRUE)
                dt <- dt[no.symptoms.reports > 0]
                dt[, no.symptoms.reports := NULL]
            }

        } else if (name == "background")
        {
            ## calculate birthdates, age and agegroup
            if ("birthmonth" %in% colnames(dt))
            {
                dt[, birthdate := as.Date(paste0(birthmonth, "-01"))]
            } else
            {
                dt[, birthyear := gsub("-.*$", "", birthyear)]
                dt[, birthdate := as.Date(paste0(birthyear, "-01-01"))]
            }
            dt[, age := lubridate::interval(birthdate, date) %/% lubridate::years(1)]
            ## remove negative ages
            dt[age < 0, birthdate := NA]
            dt[age < 0, age := NA]

            dt[, agegroup := cut(age, breaks=c(0, age.breaks, max(age, na.rm = TRUE)),
                                 include.lowest = TRUE, right = TRUE)]
            dt[grep("^\\(65,", agegroup), agegroup := "(65,)"]
            dt[, agegroup := factor(agegroup)]

            for (household.column in grep("^nb\\.household", colnames(dt), value = TRUE))
            {
                if (is.factor(dt[, get(household.column)]))
                {
                    dt[, paste(household.column) := as.integer(as.character(get(household.column)))]
                }
            }

            ## calculate auxiliary variables: living with children,
            ## using public transport
            if ("household.0.4" %in% colnames(dt))
            {
                dt[, living.with.children :=
                         as.integer((household.0.4 == "t" | household.5.18 == "t"))]
            } else
            {
                for (nb.name in grep("^nb\\.", colnames(dt), value = TRUE))
                {
                    dt[is.null(get(nb.name)), paste(nb.name) := 0]
                }
                dt[, living.with.children :=
                         as.integer((nb.household.0.4 > 0 | nb.household.5.18 > 0))]
            }

            ## clean postcodes and add regional and urban/rural information
            urban_rural_data <- copy(urban_rural)
            urban_rural_names <- colnames(urban_rural_data)
            regions_data <- copy(regions)
            regions_data[region == "M99999999", region := "Isle of Man"]
            regions_names <- colnames(regions_data)
            for (col in grep("postcode$", colnames(dt), value = TRUE))
            {
                ## clean postcode
                dt[, paste(col) := toupper(sub("[[:blank:]]+$", "", get(col)))]
                dt[get(col) == "", paste(col) := NA]
                ## set settlement type and country information
                col_prefix <- sub("postcode$", "", col)
                setnames(urban_rural_data, seq_along(urban_rural_names),
                         paste0(col_prefix, urban_rural_names))
                dt <- merge(dt, urban_rural_data, by = col, all.x = TRUE)
                dt[, paste0(col_prefix, "country") :=
                         factor(plyr::revalue(x = get(paste0(col_prefix, "country")),
                                              replace =c("W" = "wales",
                                                         "N" = "northern_ireland",
                                                         "S" = "scotland",
                                                         "E" = "england"),
                                              warn_missing = FALSE))]
                ## set regional information
                setnames(regions_data, seq_along(regions_names),
                         paste0(col_prefix, regions_names))
                dt <- merge(dt, regions_data, by = col, all.x = TRUE)
                dt[, paste0(col_prefix, "region") :=
                         factor(tolower(gsub(" ", "_",
                                             get(paste0(col_prefix, "region")))))]

                ## set urban/rural information
                ## England/Wales
                dt[country %in% c("england", "wales") &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 5),
                   paste0(col_prefix, "urban.rural") := 1]
                dt[country %in% c("england", "wales") &
                   get(paste0(col_prefix, "settlement.type")) %in% c(2, 3, 4, 6, 7, 8),
                   paste0(col_prefix, "urban.rural") := 0]
                ## Scotland
                dt[country == "scotland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 2),
                   paste0(col_prefix, "urban.rural") := 1]
                dt[country == "scotland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(3, 4, 5, 6, 7),
                   paste0(col_prefix, "urban.rural") := 0]

                ## Northern Ireland
                dt[country == "northern_ireland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 2, 3, 4),
                   paste0(col_prefix, "urban.rural") := 1]
                dt[country == "northern_ireland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(5, 6, 7),
                   paste0(col_prefix, "urban.rural") := 0]

                dt[, paste0(col_prefix, "urban.rural") :=
                         factor(get(paste0(col_prefix, "urban.rural")),
                                levels=c(0, 1),
                                labels=c("rural", "urban"))]

            }
            ## setnames(urban_rural, seq_along(urban_rural_names), urban_rural_names)
            ## setnames(regions, seq_along(regions), regions_names)

            ## highest education level
            dt[, highest.education := NA_character_]
            edu_columns <-
                grep("^(no\\.)?education(\\.|$)", colnames(dt), value=TRUE)
            for (col in edu_columns) {
                dt[get(col) == "t", highest.education := col]
            }
            dt[, highest.education :=
                     factor(highest.education,
                            levels=c("no.education", "education.gcse",
                                     "education.alevels", "education.bsc",
                                     "education.msc", "education.stillin"))]
            if ("education" %in% colnames(dt))
            {
                dt[is.na(highest.education) & !is.na(education),
                   highest.education := education + 1L]
            }

            ## household members
            hh_columns <- grep("^nb.household\\.", value=TRUE, colnames(dt))
            for (col in hh_columns) {
                indicator_col <- sub("nb\\.", "", col)
                if (indicator_col %in% colnames(dt)) {
                    dt[get(indicator_col)=="f", paste(col) := 0]
                }
            }
            dt[, nb.household := rowSums(.SD),
               .SDcols = hh_columns]
            dt[, nb.household.children := rowSums(.SD),
               .SDcols = c("nb.household.0.4", "nb.household.5.18")]
        } else if (name == "contact")
        {
          ## loop over conversational/physical variables, check for dashes,  remove text
          types <- c("conversational", "physical")
          contact_columns <-
            grep(paste0("^", "(", paste(types, collapse="|"), ")\\."),
                 value=TRUE, colnames(dt))
          settings <-
            unique(sub("^.*\\.([^.]*)\\..*$", "\\1", contact_columns))
          agegroups <-
            unique(sub("^.*\\.[^.]*\\.([^.]*)$", "\\1", contact_columns))

          for (column in contact_columns) {
            dt[, paste(column) := as.character(get(column))]
            dt[get(column) == "NULL", paste(column) := NA_character_]
            if (ranges=="sample") {
              dt[grepl("-", get(column)),
                 paste(column) :=
                   as.character(as.integer(runif(.N,
                                                 as.integer(sub("-.*$", "", get(column))),
                                                 as.integer(sub("^.*-", "", get(column))) + 1)))]
            } else {
              ## apply a function
              dt[grepl("-", get(column)),
                 paste(column) :=
                   as.character(as.integer(do.call(ranges,
                                                   list(c(as.integer(sub("-.*$", "", get(column))),
                                                          as.integer(sub("^.*-", "", get(column))) + 1)))))]
            }
            dt[, paste(column) :=
                   as.integer(gsub("[^0-9]", "", get(column)))]
          }

          columns <- list()
          for (type in types) {
            type_columns <-
              grep(paste0("^", type), contact_columns, value=TRUE)
            columns[[type]] <- type_columns
            for (setting in settings) {
              setting_columns <-
                grep(paste0("^", type, "\\.", setting), contact_columns,
                     value=TRUE)
              columns[[paste(type, setting, sep=".")]] <- setting_columns
            }
            for (agegroup in agegroups) {
              agegroup_columns <-
                grep(paste0("^", type, "\\.[^.]*\\.", agegroup),
                     contact_columns, value=TRUE)
              columns[[paste(type, agegroup, sep=".")]] <- agegroup_columns
            }
          }

          for (column_name in names(columns)) {
            dt[, paste(column_name) := as.integer(rowSums(.SD, na.rm=TRUE)) *
                   ifelse(rowSums(is.na(.SD)) == ncol(.SD), NA, 1L),
               .SDcols = columns[[column_name]]]
          }

        }

        if ("id" %in% colnames(dt))
        {
            setnames(dt, "id", paste(name, "id", sep = "."))
        }
        if ("timestamp" %in% colnames(dt)) dt[, timestamp := NULL]

        setkey(dt, global_id, date)
        dt_list[[name]] <- dt
    }
    ## join
    res <- dt_list[[length(dt_list)]]
    if (length(dt_list) > 1)
    {
        for (i in rev(seq_len(length(dt_list) - 1)))
        {
            res <- dt_list[[i]][res, roll = TRUE]
        }
    }

    res <- res[nchar(as.character(global_id)) > 0]

    res[, day.of.week := (data.table::wday(date) - 2) %% 7 + 1]
    res[, month := data.table::month(date)]

    if ("unsuccessful.join" %in% clean)
    {
        id_cols <- grep("\\.id$", colnames(res), value=TRUE)
        for (col in id_cols) {
            res <- res[!is.na(get(col))]
        }
    }

    setkey(res, date, global_id)

    if ("remove.postcodes" %in% clean)
    {
        res <- res[, setdiff(colnames(res), grep("postcode$", colnames(res), value = TRUE)), with = FALSE]
    }

    char_columns <-
      which(vapply(colnames(res), function(x) class(dt[[x]]), "") == "character", TRUE)

    tf <-
      names(char_columns)[vapply(names(char_columns), function(x) {
        length(setdiff(c("t", "f"), setdiff(unique(dt[[x]]), NA_character_))) ==  0
      }, TRUE)]

    for (column in tf) res[, paste(column) := factor(get(column))]

    return(res)
}

