##' This reads in flusurvey data for a given year
##'
##' This reads different surveys and returns a list of data tables
##' containing the surveys. It does minimal cleaning and conversion
##' into R data formats. The csv files that should be provided can be
##' obtained via SQL dump,  e.g.
##' \\f ','
##' \\a
##' \\t
##' \\o intake_16.csv
##' SELECT * FROM pollster_results_intake;
##' \\o
##' \\q
##'
##' and similarly for weekly surveys
##'
##' @param files a (named!) list of files. The names stand for the surveys (e.g., "symptoms", "background", "contacts", etc.)
##' @param year the year from which the data come. If not given, will try to guess it from the headers in the .csv files
##' @param ... options to be passed to \code{read.csv}
##' @return a list of data tables with the data
##' @author seb
##' @import data.table
##' @export
read_data <- function(files, year, ...)
{
    res <- list()
    for (name in names(files))
    {
        dt <- data.table(read.csv(files[[name]], ...))

        ## remove empty variables
        fields <- copy(colnames(dt))
        for (col in fields)
        {
            if (all(is.na(dt[, get(col)])) || all(is.null(dt[, get(col)])))
            {
                dt[, paste(col) := NULL]
            }
        }

        year <- as.character(year)

        ## convert question name
        setnames(dt, names(flusurvey::questions[[year]][[name]]),
                 flusurvey::questions[[year]][[name]])

        ## convert dates
        dt[, date := as.Date(timestamp)]

        for (col in grep("\\.date$", colnames(dt), value = TRUE))
        {
            dt[get(col) == "", paste(col) := NA_character_]
            dt[, paste(col) := as.Date(get(col))]
        }

        ## convert options
        for (option in intersect(colnames(dt), names(flusurvey::options)))
        {
            dt[, paste(option) := factor(plyr::revalue(as.character(get(option)),
                                                       flusurvey::options[[option]],
                                                       warn_missing = FALSE))]
        }

        setkey(dt, global_id, date)

        res[[name]] <- dt
    }

    return(res)
}

##' Clean and merge flusurvey data tables
##'
##' @param data the data to merge and clean, usually the result of \code{read_data}
##' @param remove.first whether to remove everyone's first report
##' @param remove.bad.symptom.dates whether to remove rows with the symptom end date before the symptom start date
##' @return a rolling-joined data table
##' @author seb
##' @import data.table lubridate
clean_and_merge <- function(data, remove.first = TRUE, remove.bad.symptom.dates = TRUE)
{
    dt_list <- list()
    for (name in names(data))
    {
        ## only keep last report per day
        dt <- copy(data[[name]])
        dt <- dt[!duplicated(dt[, list(global_id, date)], fromLast = TRUE)]

        if (name == "symptoms")
        {
            ## calculate ili
            dt[symptoms.suddenly == "yes", suddenly := 1]
            dt[symptoms.suddenly == "no", suddenly := 0]
            dt[(is.na(suddenly) | suddenly == 0) & fever.suddenly == "yes",
               suddenly := 1]
            dt[is.na(suddenly) & fever.suddenly == "no", suddenly := 0]

            dt[, ili := ((suddenly == 1) &
                         (fever == "yes" | tired == "yes" |
                          headache == "yes" |
                          muscle.and.or.joint.pain == "yes") &
                         (sore.throat == "yes" | cough == "yes" |
                          shortness.breath == "yes"))]
            dt[, ili := as.integer(ili)]

            dt[, ili.notired := ((suddenly == 1) &
                                 (fever == "yes" | headache == "yes" |
                                  muscle.and.or.joint.pain =="yes") &
                                 (sore.throat == "yes" | cough == "yes" |
                                  shortness.breath == "yes"))]
            dt[, ili.notired := as.integer(ili.notired)]

            dt[, ili.fever := ((suddenly == 1) &
                               (fever == "yes") &
                               (sore.throat == "yes" | cough == "yes" |
                                shortness.breath == "yes"))]
            dt[, ili.fever := as.integer(ili.fever)]

            dt[, ili.self := (what.do.you.think == 0)]
            dt[is.na(ili.self), ili.self := FALSE]
            dt[, ili.self := as.integer(ili.self)]

            ## calculate min.reports, max.reports, nReports
            dt[, nReports := .N, by = global_id]
            dt[, min.date := min(date), by = global_id]
            dt[, max.date := max(date), by = global_id]

            ## remove first survey completed by everyone
            if (remove.first)
            {
                dt <- dt[duplicated(global_id)]
            }

            if (remove.bad.symptom.dates)
            {
                dt <- dt[!(!is.na(symptoms.end.date) &
                            !is.na(symptoms.start.date) &
                            symptoms.end.date < symptoms.start.date)]
            }
        } else if (name == "background")
        {
            ## calculate birthdates, age and agegroup
            dt[, birthdate := as.Date(paste(birthmonth, "-01",sep=""))]
            dt[, age := lubridate::new_interval(birthdate, date) %/% years(1)]
            ## remove negative ages
            dt[age < 0, birthdate := NA]
            dt[age < 0, age := NA]

            dt[, agegroup := cut(age, breaks=c(0,18,45,65, max(age, na.rm = TRUE)),
                                 include.lowest = TRUE, right = TRUE)]

            ## calculate auxiliary variables: living with children,
            ## using public transport
            dt[, living.with.children :=
                     as.integer((household.0.4 == "t" | household.5.18 == "t"))]

            ## clean postcodes and add regional and urban/rural information
            urban_rural_names <- copy(colnames(urban_rural))
            regions_names <- copy(colnames(regions))
            for (col in grep("postcode$", colnames(dt), value = TRUE))
            {
                ## clean postcode
                dt[, paste(col) := toupper(sub("[[:blank:]]+$", "", get(col)))]
                dt[get(col) == "", paste(col) := NA]
                ## set settlement type and country information
                col_prefix <- sub("postcode$", "", col)
                setnames(urban_rural, seq_along(urban_rural_names),
                         paste0(col_prefix, urban_rural_names))
                dt <- merge(dt, urban_rural, by = col, all.x = TRUE)
                dt[, paste0(col_prefix, "country") :=
                         factor(plyr::revalue(x = get(paste0(col_prefix, "country")),
                                              replace =c("W" = "wales",
                                                         "N" = "northern_ireland",
                                                         "S" = "scotland",
                                                         "E" = "england"),
                                              warn_missing = FALSE))]
                ## set regional information
                setnames(regions, seq_along(regions_names),
                         paste0(col_prefix, regions_names))
                dt <- merge(dt, regions, by = col, all.x = TRUE)
                dt[, paste0(col_prefix, "region") :=
                         factor(tolower(gsub(" ", "_",
                                             get(paste0(col_prefix, "region")))))]

                ## set urban/rural information
                ## England/Wales
                dt[country %in% c("england", "wales") &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 5),
                   paste0(col_prefix, "urban") := 1]
                dt[country %in% c("england", "wales") &
                   get(paste0(col_prefix, "settlement.type")) %in% c(2, 3, 4, 6, 7, 8),
                   paste0(col_prefix, "urban") := 0]
                ## Scotland
                dt[country == "scotland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 2),
                   paste0(col_prefix, "urban") := 1]
                dt[country == "scotland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(3, 4, 5, 6, 7),
                   paste0(col_prefix, "urban") := 0]

                ## Northern Ireland
                dt[country == "northern_ireland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(1, 2, 3, 4),
                   paste0(col_prefix, "urban") := 1]
                dt[country == "northern_ireland" &
                   get(paste0(col_prefix, "settlement.type")) %in% c(5, 6, 7),
                   paste0(col_prefix, "urban") := 0]

                dt[, paste0(col_prefix, "urban") :=
                         factor(get(paste0(col_prefix, "urban")))]

            }
            setnames(urban_rural, seq_along(urban_rural_names), urban_rural_names)
            setnames(regions, seq_along(regions), regions_names)

        } else if (name == "contacts")
        {
            dt <- dt[, "conversational.home" := get("conversational.home.0-4") +
                          get("conversational.home.5-18") +
                          get("conversational.home.19-44") +
                          get("conversational.home.45-64") +
                          get("conversational.home.65+"), with = FALSE]
            dt <- dt[, "conversational.work" := get("conversational.work.0-4") +
                          get("conversational.work.5-18") +
                          get("conversational.work.19-44") +
                          get("conversational.work.45-64") +
                          get("conversational.work.65+"), with = FALSE]
            dt <- dt[, "conversational.other" := get("conversational.other.0-4") +
                           get("conversational.other.5-18") +
                           get("conversational.other.19-44") +
                           get("conversational.other.45-64") +
                           get("conversational.other.65+"), with = FALSE]
            dt <- dt[, "conversational.0-4" := get("conversational.home.0-4") +
                           get("conversational.work.0-4") +
                           get("conversational.other.0-4"), with = FALSE]
            dt <- dt[, "conversational.5-18" :=
                           get("conversational.home.5-18") +
                           get("conversational.work.5-18") +
                           get("conversational.other.5-18"), with = FALSE]
            dt <- dt[, "conversational.19-44" := get("conversational.home.19-44") +
                           get("conversational.work.19-44") +
                           get("conversational.other.19-44"), with = FALSE]
            dt <- dt[, "conversational.45-64" := get("conversational.home.45-64") +
                           get("conversational.work.45-64") +
                           get("conversational.other.45-64"), with = FALSE]
            dt <- dt[, "conversational.65+" := get("conversational.home.65+") +
                           get("conversational.work.65+") +
                           get("conversational.other.65+"), with = FALSE]
            dt <- dt[, "conversational" := get("conversational.home") +
                           get("conversational.work") +
                           get("conversational.other"), with = FALSE]
            dt <- dt[, "physical.home" := get("physical.home.0-4") +
                           get("physical.home.5-18") +
                           get("physical.home.19-44") +
                           get("physical.home.45-64") +
                           get("physical.home.65+"), with = FALSE]
            dt <- dt[, "physical.work" := get("physical.work.0-4") +
                           get("physical.work.5-18") +
                           get("physical.work.19-44") +
                           get("physical.work.45-64") +
                           get("physical.work.65+"), with = FALSE]
            dt <- dt[, "physical.other" := get("physical.other.0-4") +
                           get("physical.other.5-18") +
                           get("physical.other.19-44") +
                           get("physical.other.45-64") +
                           get("physical.other.65+"), with = FALSE]
            dt <- dt[, "physical.0-4" := get("physical.home.0-4") +
                           get("physical.work.0-4") +
                           get("physical.other.0-4"), with = FALSE]
            dt <- dt[, "physical.5-18" := get("physical.home.5-18") +
                           get("physical.work.5-18") +
                           get("physical.other.5-18"), with = FALSE]
            dt <- dt[, "physical.19-44" := get("physical.home.19-44") +
                           get("physical.work.19-44") +
                           get("physical.other.19-44"), with = FALSE]
            dt <- dt[, "physical.45-64" := get("physical.home.45-64") +
                           get("physical.work.45-64") +
                           get("physical.other.45-64"), with = FALSE]
            dt <- dt[, "physical.65+" := get("physical.home.65+") +
                           get("physical.work.65+") +
                           get("physical.other.65+"), with = FALSE]
            dt <- dt[, "physical" := get("physical.home") +
                           get("physical.work") +
                           get("physical.other"), with = FALSE]

        }

        dt[, id := NULL]
        dt[, user := NULL]
        dt[, timestamp := NULL]

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
    
    ## Monday of the week
    for (col in grep("date$", colnames(res), value = TRUE))
    {
        week.col <- sub("date$", "week.date", col)
        res[, paste(week.col) := get(col) - wday(get(col)) + 2]
    }

    return(res)
}
