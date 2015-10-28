##' Clean and merge flusurvey data tables
##'
##' @param data the data to merge and clean, usually the result of \code{read_data}
##' @param clean cleaning options, NULL for no cleaning, otherwise a vector of cleans to perform (by default all):
##' - 'remove.first', whether to remove everyone's first report,
##' - 'remove.bad.symptom.dates', whether to remove rows with the symptom end date before the symptom start date, or with symptom dates outside the reporting dates
##' - 'guess.start.dates', whether to guess the symptom start dates when the person couldn't remember (as a day of the report)
##' - 'limit.season', whether to limit a flu season to November -> April
##' - 'create.numeric.id', whether to create a numeric id for every user,
##' - 'n.reports', whether to exclude those with fewer than \code{min.reports} reports
##' @param min.reports minimum number of reports per user (ignored if 'min.reports' is not given as a cleaning option)
##' @return a rolling-joined data table
##' @author seb
##' @import data.table
##' @export
merge_data <- function(data, clean = c("remove.first", "remove.bad.symptom.dates", "guess.start.dates", "limit.season", "create.numeric.id", "n.reports"), min.reports = 3)
{
    dt_list <- list()
    clean <- match.arg(clean, several.ok = TRUE)
    for (name in names(data))
    {
        ## only keep last report per day
        dt <- copy(data[[name]])
        dt <- dt[!duplicated(dt[, list(global_id, date)], fromLast = TRUE)]

        if (name == "symptom")
        {
            ## calculate ili
            dt[symptoms.suddenly == "yes", suddenly := 1]
            dt[symptoms.suddenly == "no", suddenly := 0]
            dt[(is.na(suddenly) | suddenly == 0) & fever.suddenly == "yes",
               suddenly := 1]
            dt[is.na(suddenly) & fever.suddenly == "no", suddenly := 0]

            dt[, ili := ((suddenly == 1) &
                         (fever == "t" | tired == "t" |
                          headache == "t" |
                          muscle.and.or.joint.pain == "t") &
                         (sore.throat == "t" | cough == "t" |
                          shortness.breath == "t"))]
            dt[, ili := as.integer(ili)]

            dt[, ili.fever := ((suddenly == 1) &
                               (fever == "t") &
                               (sore.throat == "t" | cough == "t" |
                                shortness.breath == "t"))]
            dt[, ili.fever := as.integer(ili.fever)]

            dt[, ili.self := (what.do.you.think == 0)]
            dt[is.na(ili.self), ili.self := FALSE]
            dt[, ili.self := as.integer(ili.self)]

            dt[, gi := (vomiting == "t" | diarrhoea == "t")]
            dt[, gi := as.integer(gi)]

            ## calculate min.reports, max.reports, nReports
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

            dt[, nReports := .N, by = global_id]
            dt[, min.date := min(date), by = global_id]
            dt[, max.date := max(date), by = global_id]

            ## remove first survey completed by everyone
            if ("remove.first" %in% clean)
            {
                dt <- dt[duplicated(global_id)]
            }

            if ("guess.start.dates" %in% clean)
            {
                dt[no.symptoms == "f" & is.na(symptoms.start.date), 
                   symptoms.start.date := date]
            }

            if ("remove.bad.symptom.dates" %in% clean)
            {
                ## remove end date before start date
                dt <- dt[!(!is.na(symptoms.end.date) &
                            !is.na(symptoms.start.date) &
                            symptoms.end.date < symptoms.start.date)]
                ## remove start date before first report or after reporting date
                dt <- dt[!(!is.na(symptoms.start.date) &
                            (symptoms.start.date < min.date |
                             symptoms.start.date > date))]
                ## remove end date before first report or after reporting date
                dt <- dt[!(!is.na(symptoms.end.date) &
                            (symptoms.end.date < min.date |
                             symptoms.end.date > date))]
            }

        } else if (name == "background")
        {
            ## calculate birthdates, age and agegroup
            dt[, birthdate := as.Date(paste(birthmonth, "-01",sep=""))]
            dt[, age := lubridate::new_interval(birthdate, date) %/% lubridate::years(1)]
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

        } else if (name == "contact")
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
    
    setkey(res, date, global_id)

    if ("create.numeric.id" %in% clean)
    {
        id_table <- data.table(global_id = unique(res[, global_id]),
                               participant_id = seq_along(unique(res[, global_id])))
        res <- merge(res, id_table, all.x = TRUE, by = "global_id")
        res[, global_id := NULL]
    }

    if ("nReports" %in% colnames(res))
    {
        if ("n.reports" %in% clean)
        {
            res <- res[nReports >= min.reports]
        } else
        {
            res <- res[nReports > 0]
        }
    }

    return(res)
}

