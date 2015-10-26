library(data.table)
library(plyr) ## for revalue

## compute the age in years from a birthdate (from) and the current date (to)
age_years <- function(from, to)
{
    if (is.na(from) || is.na(to)) {
        NA
    } else {
        lt <- as.POSIXlt(c(from, to))
        age <- lt$year[2] - lt$year[1]
        mons <- lt$mon + lt$mday/50
        if(mons[2] < mons[1]) age <- age -1
        age
    }
}

uk.ur <- read.csv("urban_rural.csv", header=F, sep=",")

## 2015 data
st15 <- data.table(read.csv('weekly_15.csv', sep=',', header=T))
bt15 <- data.table(read.csv('intake_15.csv', sep=',', header=T))

## ## create translation table so that every participant gets a unique ID number
## ## (called global.id.number)
## translation <- data.table(global_id = unique(bt15[nchar(as.character(global_id)) > 0, global_id]))
## translation[, number := 200000 + seq(1,nrow(translation))]

## ## assign global id numbers
## bt15[, global.id.number := translation[match(global_id, translation[, global_id]), number]]
## st15[, global.id.number := translation$number[match(sf15$global_id,
##                                                   translation$global_id)]

## ## put data in data tables (for the rolling join to be used later)
## bt15$bid <- seq(1:nrow(bt15))

## rm(sf15)
## rm(bf15)

## setnames(bt15, 2, "global_id.bg")

st15$date <- as.Date(st15$timestamp)
bt15$date <- as.Date(bt15$timestamp)

st15 <- st15[date >= "2014-07-01"]
bt15 <- bt15[date >= "2014-07-01"]

## only keep last report per day
st15 <- st15[!duplicated(bt15[, list(global_id, date)], fromLast=T)]
bt15 <- bt15[!duplicated(bt15[, list(global_id, date)], fromLast=T)]
setkey(st15, global_id, date)
setkey(bt15, global_id, date)


## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate

fields <- copy(colnames(st15))
for (col in fields)
{
    if (all(is.na(st15[, get(col)])))
    {
        st15[, paste(col) := NULL]
    }
}

st15[, same := factor(revalue(as.character(same), c("0" = "yes", "1" = "no", "2" = "dont_know", "3" = NA)))]
st15[, symptoms.start.option := factor(revalue(as.character(symptoms.start.option), c("0" = "date", "1" = "dont_know")))]
st15[, symptoms.end.option := factor(revalue(as.character(symptoms.end.option), c("0" = "date", "1" = "dont_know", "2" = "still_ill")))]
st15[, symptoms.suddenly := factor(revalue(as.character(symptoms.suddenly), c("0" = "yes", "1" = "no", "2" = "dont_know")))]
st15[, fever.start.option := factor(revalue(as.character(fever.start.option), c("1" = "date", "2" = "dont_know")))]
st15[, fever.suddenly := factor(revalue(as.character(fever.suddenly), c("0" = "yes", "1" = "no", "2" = "dont_know")))]
st15[, fever.temperature := factor(revalue(as.character(fever.temperature), c("0" = "yes", "1" = "no", "2" = "dont_know")))]
st15[, fever.temperature.value := factor(revalue(as.character(fever.temperature.value), c("0" = "[,37)", "1" = "[37,37.5)", "2" = "[37.5,38)", "3" = "[38,39)", "4" = "[39,40)", "5" = "[40,)", "6" = "dont_know")))]
st15[, visit.medical.service.howsoon.gp.receptionist := factor(revalue(as.character(visit.medical.service.howsoon.gp.receptionist), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, visit.medical.service.howsoon.gp.doctor.nurse := factor(revalue(as.character(visit.medical.service.howsoon.gp.doctor.nurse), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, visit.medical.service.howsoon.nhs := factor(revalue(as.character(visit.medical.service.howsoon.nhs), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, visit.medical.service.howsoon.other := factor(revalue(as.character(visit.medical.service.howsoon.other), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, contact.medical.service.howsoon.gp.receptionist := factor(revalue(as.character(contact.medical.service.howsoon.gp.receptionist), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, contact.medical.service.howsoon.gp.doctor.nurse := factor(revalue(as.character(contact.medical.service.howsoon.gp.doctor.nurse), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, contact.medical.service.howsoon.nhs := factor(revalue(as.character(contact.medical.service.howsoon.nhs), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, contact.medical.service.howsoon.other := factor(revalue(as.character(contact.medical.service.howsoon.other), c("100" = NA, "0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, howsoon.medication := factor(revalue(as.character(howsoon.medication), c("0" = "same.day", "1" = "1.day", "2" = "2.days", "3" = "3.days", "4" = "4.days", "5" = "[5,7].days", "6" = "(7,) days", "7" = "dont_know")))]
st15[, alter.routine := factor(revalue(as.character(alter.routine), c("0" = "no", "1" = "yes_but_no_time_off", "2" = "yes_time_off")))]
st15[, still.altered := factor(revalue(as.character(still.altered), c("0" = "yes", "1" = "no", "3" = "other")))]
st15[, howlong.altered := factor(revalue(as.character(howlong.altered), c("0" = "1.day", "1" = "2.days", "2" = "3.days", "3" = "4.days", "4" = "5.days", "5" = "(5,10].days", "6" = "(10,15].days", "7" = "(15,) days")))]
st15[, what.do.you.think := factor(revalue(as.character(what.do.you.think), c("0" = "ili", "1" = "common_cold", "2" = "allergy_hay_fever", "3" = "gastro", "4" = "other", "5" = "dont_know", "6" = "asthma")))]

st15[symptoms.suddenly == "yes", suddenly := 1]
st15[symptoms.suddenly == "no", suddenly := 0]
st15[(is.na(suddenly) | suddenly == 0) & fever.suddenly == "yes", suddenly := 1]
st15[is.na(suddenly) & fever.suddenly == "no", suddenly := 0]

st15[, ili := ((suddenly == 1) &
             (fever == "yes" | tired == "yes" |
              headache == "yes" |
              muscle.and.or.joint.pain == "yes") &
             (sore.throat == "yes" | cough == "yes" |
              shortness.breath == "yes"))]
st15[, ili := as.numeric(ili)]

st15[, ili.notired := ((suddenly == 1) &
                     (fever == "yes" | headache == "yes" |
                      muscle.and.or.joint.pain =="yes") &
                     (sore.throat == "yes" | cough == "yes" |
                      shortness.breath == "yes"))]
st15[, ili.notired := as.numeric(ili.notired)]

st15[, ili.fever := ((suddenly == 1) &
                   (fever == "yes") &
                   (sore.throat == "yes" | cough == "yes" |
                    shortness.breath == "yes"))]
st15[, ili.fever := as.numeric(ili.fever)]

st15[, nReports := .N, by = global_id]
st15[, min.date := min(date), by = global_id]
st15[, max.date := max(date), by = global_id]

## Monday of the week
st15[, week.date := date - wday(date) + 2]

st15$symptoms.start.date <- as.Date(st15$symptoms.start.date, "%Y-%m-%d")
st15$symptoms.end.date <- as.Date(st15$symptoms.end.date, "%Y-%m-%d")

st15[, symptoms.start.week.date := symptoms.start.date - wday(symptoms.start.date) + 2]
st15[, symptoms.end.week.date := symptoms.end.date - wday(symptoms.end.date) + 2]

## more variables to be used later
fields <- copy(colnames(st15))
for (col in fields)
{
    if (all(is.na(st15[, get(col)])))
    {
        st15[, paste(col) := NULL]
    }
}

bt15 <- bt15[, country := "uk"]



bt15$birthdate <- as.Date(paste(bt15$birthmonth, "-01",sep=""))

bt15$norisk <- factor(bt15$norisk)
bt15$atrisk <- bt15$norisk
levels(bt15$atrisk) <- c(NA, 1,0)
bt15$atrisk <- as.numeric(paste(bt15$atrisk))
bt15$age <-  0
bt15$age <- apply(bt15, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt15$agegroup <- cut(bt15$age, breaks=c(0,18,45,65, max(bt15$age, na.rm=T)),
                     include.lowest=T, right=F)
bt15$vaccine.date <- as.Date(bt15$date.vaccine, "%Y-%m-%d")
## bt15$vaccine <- as.numeric(bt15$vaccine.this.year==0 & (is.na(bt15$vaccine.date) |
##                            bt15$vaccine.date <= bt15$date))
bt15$children <- as.numeric((bt15$household.0.4 == "t" | bt15
    $household.5.18 == "t"))

st15$ili.self <- (st15$what.do.you.think == 0)
st15[is.na(ili.self)]$ili.self <- FALSE

bt15$using.transport <- (bt15$transport > 0)

bt15$postcode <- sub("[[:blank:]]+$", "", bt15$postcode)
bt15$postcode <- toupper(bt15$postcode)

bt15$work.postcode <- sub("[[:blank:]]+$", "", bt15$work.postcode)
bt15$work.postcode <- toupper(bt15$work.postcode)

bt15 <- bt15[country == "uk", "ur" := uk.ur$V3[match(bt15[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "uk.country" := uk.ur$V2[match(bt15[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "urban" := rep(0, length(bt15[country ==
                                                            "uk"]$postcode)), with=F]
bt15 <- bt15[country == "uk", "ur" := uk.ur$V3[match(bt15[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "uk.country" := uk.ur$V2[match(bt15[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "urban" := rep(0, length(bt15[country ==
                                                            "uk"]$postcode)), with=F]

bt15[country == "uk" & is.na(bt15$ur),]$urban <- 2

bt15[bt15$uk.country %in% c("E","W") & !(bt15$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt15[bt15$uk.country %in% c("E","W") & bt15$ur %in% c(1,5),]$urban <- 1

bt15[bt15$uk.country == "S" & bt15$ur %in% c(1,2),]$urban <- 1
bt15[bt15$uk.country == "S" & bt15$ur %in% c(3,4,5,6,7),]$urban <- 0

bt15[bt15$uk.country == "N" & bt15$ur %in% c(1,2,3,4),]$urban <- 1
bt15[bt15$uk.country == "N" & !(bt15$ur %in% c(5,6,7)),]$urban <- 0

bt15$urban <- as.factor(bt15$urban)

bt15 <- bt15[country == "uk", "work.ur" := uk.ur$V3[match(bt15[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "work.uk.country" := uk.ur$V2[match(bt15[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt15 <- bt15[country == "uk", "work.urban" := rep(0, length(bt15[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt15[country == "uk" & is.na(bt15$work.ur),]$work.urban <- 2

bt15[bt15$work.uk.country %in% c("E","W") & !(bt15$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt15[bt15$work.uk.country %in% c("E","W") & bt15$work.ur %in% c(1,5),]$work.urban <- 1

bt15[bt15$work.uk.country == "S" & bt15$work.ur %in% c(1,2),]$work.urban <- 1
bt15[bt15$work.uk.country == "S" & bt15$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt15[bt15$work.uk.country == "N" & bt15$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt15[bt15$work.uk.country == "N" & !(bt15$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt15$work.urban <- as.factor(bt15$work.urban)

data.15 <- list(symptoms = st15, background = bt15)
saveRDS(data.15, "flusurvey_201315_raw.rds")

data.15 <- readRDS("flusurvey_201315_raw.rds")
st15 <- data.15$symptoms
bt15 <- data.15$background

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt15 <- bt15[st15, roll=TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt15 <- dt15[!is.na(global.id.number)]

dt15 <- dt15[date > "2014-07-01"]

saveRDS(dt15, "flusurvey_201415.rds")

dt15 <- readRDS("flusurvey_201415.rds")

## 2014
sf14 <- read.csv('weekly_14.csv', sep=',', header=T)
bf14 <- read.csv('intake_14.csv', sep=',', header=T)

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf14$global_id))
translation$number <- 140000 + seq(1,nrow(translation))

## assign global id numbers
bf14$global.id.number <- translation$number[match(bf14$global_id,
                                                  translation$global_id)]
sf14$global.id.number <- translation$number[match(sf14$global_id,
                                                  translation$global_id)]

## put data in data tables (for the rolling join to be used later)
st14 <- data.table(sf14)
bt14 <- data.table(bf14)
bt14$bid <- seq(1:nrow(bt14))

rm(sf14)
rm(bf14)

setnames(bt14, 2, "global_id.bg")

st14$date <- as.Date(st14$timestamp)
bt14$date <- as.Date(bt14$timestamp)

st14 <- st14[!duplicated(bt14[, list(global.id.number, date)], fromLast=T)]
bt14 <- bt14[!duplicated(bt14[, list(global.id.number, date)], fromLast=T)]
setkey(st14, global.id.number, date)
setkey(bt14, global.id.number, date)


## set convenient names

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
symptoms.14 <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")

for (symptom in symptoms.14) {
    st14 <- st14[get(symptom) == "f", paste("symptom.", symptom, sep = "") := as.integer(0), with = F]
    st14 <- st14[get(symptom) == "t", paste("symptom.", symptom, sep = "") := as.integer(1), with = F]
    st14 <- st14[, !symptom, with = F]
    setnames(st14, paste("symptom.", symptom, sep = ""), symptom)
}

st14[, suddenly := 1]
st14[is.na(symptoms.suddenly) & is.na(fever.suddenly), suddenly := NA]
st14[is.na(symptoms.suddenly) & fever.suddenly > 0, suddenly := 0]
st14[is.na(fever.suddenly) & symptoms.suddenly > 0, suddenly := 0]
st14[fever.suddenly > 0 & symptoms.suddenly > 0, suddenly := 0]

st14[, ili := ((suddenly == 1) &
               (fever == 1 | tired == 1 |
                headache == 1 | muscle.and.or.joint.pain == 1) &
               (sore.throat == 1 | cough ==1 |
                shortness.breath == 1))]
st14[, ili := as.integer(ili)]

st14[, ili.notired := ((suddenly == 1) &
                       (fever == 1 |
                        headache == 1 | muscle.and.or.joint.pain == 1) &
                       (sore.throat == 1 | cough ==1 |
                        shortness.breath == 1))]
st14[, ili.notired := as.integer(ili.notired)]

st14[, ili.fever := ((suddenly == 1) &
                     (fever == 1) &
                     (sore.throat == 1 | cough ==1 |
                      shortness.breath == 1))]
st14[, ili.fever := as.integer(ili.fever)]

freq <-
    data.table(aggregate(st14$global.id.number,
                         by=list(st14$global.id.number),
                         length))
setkey(freq, Group.1)
st14 <- st14[freq]
setnames(st14, "x", "nReports")

mindate <-
    data.table(aggregate(st14$date,
                         by=list(st14$global.id.number),
                         min))
setkey(mindate, Group.1)
st14 <- st14[mindate]
setnames(st14, "x", "mindate")
maxdate <-
    data.table(aggregate(st14$date,
                         by=list(st14$global.id.number),
                         max))
setkey(maxdate, Group.1)
st14 <- st14[maxdate]
setnames(st14, "x", "maxdate")

st14$week <- format(st14$date, format="%G-%W")
st14[st14$week=="2014-00"]$week <- "2014-53"

st14$symptoms.start.date <- as.Date(st14$symptoms.start.date, "%Y-%m-%d")
st14$symptoms.end.date <- as.Date(st14$symptoms.end.date, "%Y-%m-%d")
st14$symptoms.start.week <- format(st14$symptoms.start.date, format="%G-%W")
st14[st14$symptoms.start.week=="2014-00"]$symptoms.start.week <- "2013-53"
st14[st14$symptoms.start.week=="2014-52"]$symptoms.start.week <- "2013-52"

## more variables to be used later
bt14 <- bt14[, country := "uk"]

bt14$birthdate <- as.Date(paste(bt14$birthmonth, "-01",sep=""))


bt14$norisk <- factor(bt14$norisk)
bt14$atrisk <- bt14$norisk
levels(bt14$atrisk) <- c(NA, 1,0)
bt14$atrisk <- as.numeric(paste(bt14$atrisk))
bt14$age <-  0
bt14$age <- apply(bt14, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt14$agegroup <- cut(bt14$age, breaks=c(0,18,45,65, max(bt14$age, na.rm=T)),
                     include.lowest=T, right=F)
bt14$vaccine.date <- as.Date(bt14$date.vaccine, "%Y-%m-%d")
## bt14$vaccine <- as.numeric(bt14$vaccine.this.year==0 & (is.na(bt14$vaccine.date) |
##                            bt14$vaccine.date <= bt14$date))
bt14$children <- as.numeric((bt14$household.0.4 == "t" | bt14
    $household.5.18 == "t"))

st14$ili.self <- (st14$what.do.you.think == 0)
st14[is.na(ili.self)]$ili.self <- FALSE

bt14$using.transport <- (bt14$transport > 0)

bt14$postcode <- sub("[[:blank:]]+$", "", bt14$postcode)
bt14$postcode <- toupper(bt14$postcode)

bt14$work.postcode <- sub("[[:blank:]]+$", "", bt14$work.postcode)
bt14$work.postcode <- toupper(bt14$work.postcode)

bt14 <- bt14[country == "uk", "ur" := uk.ur$V3[match(bt14[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "uk.country" := uk.ur$V2[match(bt14[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "urban" := rep(0, length(bt14[country ==
                                                            "uk"]$postcode)), with=F]
bt14 <- bt14[country == "uk", "ur" := uk.ur$V3[match(bt14[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "uk.country" := uk.ur$V2[match(bt14[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "urban" := rep(0, length(bt14[country ==
                                                            "uk"]$postcode)), with=F]

bt14[country == "uk" & is.na(bt14$ur),]$urban <- 2

bt14[bt14$uk.country %in% c("E","W") & !(bt14$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt14[bt14$uk.country %in% c("E","W") & bt14$ur %in% c(1,5),]$urban <- 1

bt14[bt14$uk.country == "S" & bt14$ur %in% c(1,2),]$urban <- 1
bt14[bt14$uk.country == "S" & bt14$ur %in% c(3,4,5,6,7),]$urban <- 0

bt14[bt14$uk.country == "N" & bt14$ur %in% c(1,2,3,4),]$urban <- 1
bt14[bt14$uk.country == "N" & !(bt14$ur %in% c(5,6,7)),]$urban <- 0

bt14$urban <- as.factor(bt14$urban)

bt14 <- bt14[country == "uk", "work.ur" := uk.ur$V3[match(bt14[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "work.uk.country" := uk.ur$V2[match(bt14[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt14 <- bt14[country == "uk", "work.urban" := rep(0, length(bt14[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt14[country == "uk" & is.na(bt14$work.ur),]$work.urban <- 2

bt14[bt14$work.uk.country %in% c("E","W") & !(bt14$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt14[bt14$work.uk.country %in% c("E","W") & bt14$work.ur %in% c(1,5),]$work.urban <- 1

bt14[bt14$work.uk.country == "S" & bt14$work.ur %in% c(1,2),]$work.urban <- 1
bt14[bt14$work.uk.country == "S" & bt14$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt14[bt14$work.uk.country == "N" & bt14$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt14[bt14$work.uk.country == "N" & !(bt14$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt14$work.urban <- as.factor(bt14$work.urban)

data.14 <- list(symptoms = st14, background = bt14)
saveRDS(data.14, "flusurvey_201314_raw.rds")

data.14 <- readRDS("flusurvey_201314_raw.rds")
st14 <- data.14$symptoms
bt14 <- data.14$background

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt14 <- bt14[st14, roll=TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt14 <- dt14[!is.na(global.id.number)]

saveRDS(dt14, "flusurvey_201314.rds")
dt14 <- readRDS("flusurvey_201314.rds")

## 2013
sf13 <- read.csv('weekly_13.csv', sep=',', header=T)
bf13 <- read.csv('intake_13.csv', sep=',', header=T)
cf13 <- read.csv('contact_13.csv', sep=',', header=T)

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf13$global_id))
translation$number <- 130000 + seq(1,nrow(translation))

## assign global id numbers
bf13$global.id.number <- translation$number[match(bf13$global_id,
                                                  translation$global_id)]
sf13$global.id.number <- translation$number[match(sf13$global_id,
                                                  translation$global_id)]
cf13$global.id.number <- translation$number[match(cf13$global_id,
                                                  translation$global_id)]

## put data in data tables (for the rolling join to be used later)
st13 <- data.table(sf13)
bt13 <- data.table(bf13)
ct13 <- data.table(cf13)
bt13$bid <- seq(1:nrow(bt13))
ct13$cid <- seq(1:nrow(ct13))

rm(sf13)
rm(bf13)
rm(cf13)

setnames(bt13, 2, "global_id.bg")
setnames(ct13, 2, "global_id.contacts")

st13$date <- as.Date(st13$timestamp)
bt13$date <- as.Date(bt13$timestamp)
ct13$date <- as.Date(ct13$timestamp)

st13 <- st13[!duplicated(bt13[, list(global.id.number, date)], fromLast=T)]
bt13 <- bt13[!duplicated(bt13[, list(global.id.number, date)], fromLast=T)]
ct13 <- ct13[!duplicated(ct13[, list(global.id.number, date)], fromLast=T)]
setkey(st13, global.id.number, date)
setkey(bt13, global.id.number, date)
setkey(ct13, global.id.number, date)


## set convenient names
ct13 <- ct13[,"conversational.home" := get("conversational.home.0-4") +
                  get("conversational.home.5-18") + get("conversational.home.19-44") +
                  get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct13 <- ct13[,"conversational.work" := get("conversational.work.0-4") +
                  get("conversational.work.5-18") + get("conversational.work.19-44") +
                  get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct13 <- ct13[,"conversational.other" := get("conversational.other.0-4") +
                  get("conversational.other.5-18") + get("conversational.other.19-44") +
                  get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct13 <- ct13[,"conversational.0-4" := get("conversational.home.0-4") +
                  get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct13 <- ct13[,"conversational.5-18" := get("conversational.home.5-18") +
                  get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct13 <- ct13[,"conversational.19-44" := get("conversational.home.19-44") +
                  get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct13 <- ct13[,"conversational.45-64" := get("conversational.home.45-64") +
                  get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct13 <- ct13[,"conversational.65+" := get("conversational.home.65+") +
                  get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct13 <- ct13[,"conversational" := get("conversational.home") +
                  get("conversational.work") + get("conversational.other"), with=F]
ct13 <- ct13[,"physical.home" := get("physical.home.0-4") +
                  get("physical.home.5-18") + get("physical.home.19-44") +
                  get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct13 <- ct13[,"physical.work" := get("physical.work.0-4") +
                  get("physical.work.5-18") + get("physical.work.19-44") +
                  get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct13 <- ct13[,"physical.other" := get("physical.other.0-4") +
                  get("physical.other.5-18") + get("physical.other.19-44") +
                  get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct13 <- ct13[,"physical.0-4" := get("physical.home.0-4") +
                  get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct13 <- ct13[,"physical.5-18" := get("physical.home.5-18") +
                  get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct13 <- ct13[,"physical.19-44" := get("physical.home.19-44") +
                  get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct13 <- ct13[,"physical.45-64" := get("physical.home.45-64") +
                  get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct13 <- ct13[,"physical.65+" := get("physical.home.65+") +
                  get("physical.work.65+") + get("physical.other.65+"), with=F]
ct13 <- ct13[,"physical" := get("physical.home") +
                  get("physical.work") + get("physical.other"), with=F]

ct13$week <- format(ct13$date, format="%G-%W")
ct13[ct13$week=="2013-53"]$week <- "2012-53"
ct13[ct13$week=="2013-00"]$week <- "2012-53"

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
symptoms.13 <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")

for (symptom in symptoms.13) {
    st13 <- st13[get(symptom) == "f", paste("symptom.", symptom, sep = "") := as.integer(0), with = F]
    st13 <- st13[get(symptom) == "t", paste("symptom.", symptom, sep = "") := as.integer(1), with = F]
    st13 <- st13[, !symptom, with = F]
    setnames(st13, paste("symptom.", symptom, sep = ""), symptom)
}

st13[, suddenly := 1]
st13[is.na(symptoms.suddenly) & is.na(fever.suddenly), suddenly := NA]
st13[is.na(symptoms.suddenly) & fever.suddenly > 0, suddenly := 0]
st13[is.na(fever.suddenly) & symptoms.suddenly > 0, suddenly := 0]
st13[fever.suddenly > 0 & symptoms.suddenly > 0, suddenly := 0]

st13[, ili := ((suddenly == 1) &
               (fever == 1 | tired == 1 |
                headache == 1 | muscle.and.or.joint.pain == 1) &
               (sore.throat == 1 | cough ==1 |
                shortness.breath == 1))]
st13[, ili := as.integer(ili)]

st13[, ili.notired := ((suddenly == 1) &
                       (fever == 1 |
                        headache == 1 | muscle.and.or.joint.pain == 1) &
                       (sore.throat == 1 | cough ==1 |
                        shortness.breath == 1))]
st13[, ili.notired := as.integer(ili.notired)]

st13[, ili.fever := ((suddenly == 1) &
                     (fever == 1) &
                     (sore.throat == 1 | cough ==1 |
                      shortness.breath == 1))]
st13[, ili.fever := as.integer(ili.fever)]

freq <-
    data.table(aggregate(st13$global.id.number,
                         by=list(st13$global.id.number),
                         length))
setkey(freq, Group.1)
st13 <- st13[freq]
setnames(st13, "x", "nReports")

mindate <-
    data.table(aggregate(st13$date,
                         by=list(st13$global.id.number),
                         min))
setkey(mindate, Group.1)
st13 <- st13[mindate]
setnames(st13, "x", "mindate")
maxdate <-
    data.table(aggregate(st13$date,
                         by=list(st13$global.id.number),
                         max))
setkey(maxdate, Group.1)
st13 <- st13[maxdate]
setnames(st13, "x", "maxdate")

st13$week <- format(st13$date, format="%G-%W")
st13[st13$week=="2013-00"]$week <- "2012-53"
st13[st13$week=="2013-53"]$week <- "2012-53"

st13$symptoms.start.date <- as.Date(st13$symptoms.start.date, "%Y-%m-%d")
st13$symptoms.end.date <- as.Date(st13$symptoms.end.date, "%Y-%m-%d")
st13$symptoms.start.week <- format(st13$symptoms.start.date, format="%G-%W")
st13[st13$symptoms.start.week=="2013-00"]$symptoms.start.week <- "2012-53"
st13[st13$symptoms.start.week=="2013-53"]$symptoms.start.week <- "2012-53"

## more variables to be used later
bt13 <- bt13[, country := "uk"]

bt13$birthdate <- as.Date(paste(bt13$birthmonth, "-01",sep=""))

bt13$norisk <- factor(bt13$norisk)
bt13$atrisk <- bt13$norisk
levels(bt13$atrisk) <- c(1,0)
bt13$atrisk <- as.numeric(paste(bt13$atrisk))
bt13$age <-  0
bt13$age <- apply(bt13, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt13$agegroup <- cut(bt13$age, breaks=c(0,18,45,65, max(bt13$age, na.rm=T)),
                     include.lowest=T, right=F)
bt13$vaccine.date <- as.Date(bt13$date.vaccine, "%Y-%m-%d")
## bt13$vaccine <- as.numeric(bt13$vaccine.this.year==0 & (is.na(bt13$vaccine.date) |
##                            bt13$vaccine.date <= bt13$date))
bt13$children <- as.numeric((bt13$household.0.4 == "t" | bt13
    $household.5.18 == "t"))

st13$ili.self <- (st13$what.do.you.think == 0)
st13[is.na(ili.self)]$ili.self <- FALSE

bt13$using.transport <- (bt13$transport > 0)

bt13$postcode <- sub("[[:blank:]]+$", "", bt13$postcode)
bt13$postcode <- toupper(bt13$postcode)

bt13$work.postcode <- sub("[[:blank:]]+$", "", bt13$work.postcode)
bt13$work.postcode <- toupper(bt13$work.postcode)

bt13 <- bt13[country == "uk", "ur" := uk.ur$V3[match(bt13[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "uk.country" := uk.ur$V2[match(bt13[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "urban" := rep(0, length(bt13[country ==
                                                            "uk"]$postcode)), with=F]
bt13 <- bt13[country == "uk", "ur" := uk.ur$V3[match(bt13[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "uk.country" := uk.ur$V2[match(bt13[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "urban" := rep(0, length(bt13[country ==
                                                            "uk"]$postcode)), with=F]

bt13[country == "uk" & is.na(bt13$ur),]$urban <- 2

bt13[bt13$uk.country %in% c("E","W") & !(bt13$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt13[bt13$uk.country %in% c("E","W") & bt13$ur %in% c(1,5),]$urban <- 1

bt13[bt13$uk.country == "S" & bt13$ur %in% c(1,2),]$urban <- 1
bt13[bt13$uk.country == "S" & bt13$ur %in% c(3,4,5,6,7),]$urban <- 0

bt13[bt13$uk.country == "N" & bt13$ur %in% c(1,2,3,4),]$urban <- 1
bt13[bt13$uk.country == "N" & !(bt13$ur %in% c(5,6,7)),]$urban <- 0

bt13$urban <- as.factor(bt13$urban)

bt13 <- bt13[country == "uk", "work.ur" := uk.ur$V3[match(bt13[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "work.uk.country" := uk.ur$V2[match(bt13[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt13 <- bt13[country == "uk", "work.urban" := rep(0, length(bt13[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt13[country == "uk" & is.na(bt13$work.ur),]$work.urban <- 2

bt13[bt13$work.uk.country %in% c("E","W") & !(bt13$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt13[bt13$work.uk.country %in% c("E","W") & bt13$work.ur %in% c(1,5),]$work.urban <- 1

bt13[bt13$work.uk.country == "S" & bt13$work.ur %in% c(1,2),]$work.urban <- 1
bt13[bt13$work.uk.country == "S" & bt13$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt13[bt13$work.uk.country == "N" & bt13$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt13[bt13$work.uk.country == "N" & !(bt13$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt13$work.urban <- as.factor(bt13$work.urban)

data.13 <- list(symptoms = st13, background = bt13, contact = ct13)
saveRDS(data.13, "flusurvey_201213_raw.rds")

data.13 <- readRDS("flusurvey_201213_raw.rds")
st13 <- data.13$symptoms
bt13 <- data.13$background
ct13 <- data.13$contact

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt13 <- bt13[ct13[st13, roll=TRUE], roll = TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt13 <- dt13[!is.na(global.id.number)]

saveRDS(dt13, "flusurvey_201213.rds")
dt13 <- readRDS("flusurvey_201213.rds")

## 2012
sf12 <- read.csv('weekly_12.csv', sep=',', header=T)
bf12 <- read.csv('intake_12.csv', sep=',', header=T)
cf12 <- read.csv('contact_12.csv', sep=',', header=T)

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf12$global_id))
translation$number <- 120000 + seq(1,nrow(translation))

## assign global id numbers
bf12$global.id.number <- translation$number[match(bf12$global_id,
                                                  translation$global_id)]
sf12$global.id.number <- translation$number[match(sf12$global_id,
                                                  translation$global_id)]
cf12$global.id.number <- translation$number[match(cf12$global_id,
                                                  translation$global_id)]

## put data in data tables (for the rolling join to be used later)
st12 <- data.table(sf12)
bt12 <- data.table(bf12)
ct12 <- data.table(cf12)
bt12$bid <- seq(1:nrow(bt12))
ct12$cid <- seq(1:nrow(ct12))

rm(sf12)
rm(bf12)
rm(cf12)

setnames(bt12, 2, "global_id.bg")
setnames(ct12, 2, "global_id.contacts")

st12$date <- as.Date(st12$timestamp)
bt12$date <- as.Date(bt12$timestamp)
ct12$date <- as.Date(ct12$timestamp)

st12 <- st12[!duplicated(bt12[, list(global.id.number, date)], fromLast=T)]
bt12 <- bt12[!duplicated(bt12[, list(global.id.number, date)], fromLast=T)]
ct12 <- ct12[!duplicated(ct12[, list(global.id.number, date)], fromLast=T)]
setkey(st12, global.id.number, date)
setkey(bt12, global.id.number, date)
setkey(ct12, global.id.number, date)



ct12 <- ct12[,"conversational.home" := get("conversational.home.0-4") +
                  get("conversational.home.5-18") + get("conversational.home.19-44") +
                  get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct12 <- ct12[,"conversational.work" := get("conversational.work.0-4") +
                  get("conversational.work.5-18") + get("conversational.work.19-44") +
                  get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct12 <- ct12[,"conversational.other" := get("conversational.other.0-4") +
                  get("conversational.other.5-18") + get("conversational.other.19-44") +
                  get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct12 <- ct12[,"conversational.0-4" := get("conversational.home.0-4") +
                  get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct12 <- ct12[,"conversational.5-18" := get("conversational.home.5-18") +
                  get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct12 <- ct12[,"conversational.19-44" := get("conversational.home.19-44") +
                  get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct12 <- ct12[,"conversational.45-64" := get("conversational.home.45-64") +
                  get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct12 <- ct12[,"conversational.65+" := get("conversational.home.65+") +
                  get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct12 <- ct12[,"conversational" := get("conversational.home") +
                  get("conversational.work") + get("conversational.other"), with=F]
ct12 <- ct12[,"physical.home" := get("physical.home.0-4") +
                  get("physical.home.5-18") + get("physical.home.19-44") +
                  get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct12 <- ct12[,"physical.work" := get("physical.work.0-4") +
                  get("physical.work.5-18") + get("physical.work.19-44") +
                  get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct12 <- ct12[,"physical.other" := get("physical.other.0-4") +
                  get("physical.other.5-18") + get("physical.other.19-44") +
                  get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct12 <- ct12[,"physical.0-4" := get("physical.home.0-4") +
                  get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct12 <- ct12[,"physical.5-18" := get("physical.home.5-18") +
                  get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct12 <- ct12[,"physical.19-44" := get("physical.home.19-44") +
                  get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct12 <- ct12[,"physical.45-64" := get("physical.home.45-64") +
                  get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct12 <- ct12[,"physical.65+" := get("physical.home.65+") +
                  get("physical.work.65+") + get("physical.other.65+"), with=F]
ct12 <- ct12[,"physical" := get("physical.home") +
                  get("physical.work") + get("physical.other"), with=F]

ct12$week <- format(ct12$date, format="%G-%W")
ct12[ct12$week=="2011-00"]$week <- "2011-53"

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
symptoms.12 <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")

for (symptom in symptoms.12) {
    st12 <- st12[get(symptom) == "f", paste("symptom.", symptom, sep = "") := as.integer(0), with = F]
    st12 <- st12[get(symptom) == "t", paste("symptom.", symptom, sep = "") := as.integer(1), with = F]
    st12 <- st12[, !symptom, with = F]
    setnames(st12, paste("symptom.", symptom, sep = ""), symptom)
}

st12[, suddenly := 1]
st12[is.na(symptoms.suddenly) & is.na(fever.suddenly), suddenly := NA]
st12[is.na(symptoms.suddenly) & fever.suddenly > 0, suddenly := 0]
st12[is.na(fever.suddenly) & symptoms.suddenly > 0, suddenly := 0]
st12[fever.suddenly > 0 & symptoms.suddenly > 0, suddenly := 0]

st12[, ili := ((suddenly == 1) &
               (fever == 1 | tired == 1 |
                headache == 1 | muscle.and.or.joint.pain == 1) &
               (sore.throat == 1 | cough ==1 |
                shortness.breath == 1))]
st12[, ili := as.integer(ili)]

st12[, ili.notired := ((suddenly == 1) &
                       (fever == 1 |
                        headache == 1 | muscle.and.or.joint.pain == 1) &
                       (sore.throat == 1 | cough ==1 |
                        shortness.breath == 1))]
st12[, ili.notired := as.integer(ili.notired)]

st12[, ili.fever := ((suddenly == 1) &
                     (fever == 1) &
                     (sore.throat == 1 | cough ==1 |
                      shortness.breath == 1))]
st12[, ili.fever := as.integer(ili.fever)]

freq <-
    data.table(aggregate(st12$global.id.number,
                         by=list(st12$global.id.number),
                         length))
setkey(freq, Group.1)
st12 <- st12[freq]
setnames(st12, "x", "nReports")

mindate <-
    data.table(aggregate(st12$date,
                         by=list(st12$global.id.number),
                         min))
setkey(mindate, Group.1)
st12 <- st12[mindate]
setnames(st12, "x", "mindate")
maxdate <-
    data.table(aggregate(st12$date,
                         by=list(st12$global.id.number),
                         max))
setkey(maxdate, Group.1)
st12 <- st12[maxdate]
setnames(st12, "x", "maxdate")

st12$week <- format(st12$date, format="%G-%W")
st12[st12$week=="2011-00"]$week <- "2011-53"
st12$weekweight <- 1/table(st12$week)[st12$week]

st12$symptoms.start.date <- as.Date(st12$symptoms.start.date, "%Y-%m-%d")
st12$symptoms.end.date <- as.Date(st12$symptoms.end.date, "%Y-%m-%d")
st12$symptoms.start.week <- format(st12$symptoms.start.date, format="%G-%W")
st12[st12$symptoms.start.week=="2011-00"]$symptoms.start.week <- "2011-53"

st12$ili.self <- (st12$what.do.you.think == 0)
st12[is.na(ili.self)]$ili.self <- FALSE

## more variables to be used later
bt12 <- bt12[, country := "uk"]

bt12$birthdate <- as.Date(paste(bt12$birthmonth, "-01",sep=""))

bt12$norisk <- factor(bt12$norisk)
bt12$atrisk <- bt12$norisk
levels(bt12$atrisk) <- c(1,0)
bt12$atrisk <- as.numeric(paste(bt12$atrisk))
bt12$age <-  0
bt12$age <- apply(bt12, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt12$agegroup <- cut(bt12$age, breaks=c(0,18,45,65, max(bt12$age, na.rm=T)),
                     include.lowest=T, right=F)
bt12$vaccine.date <- as.Date(bt12$date.vaccine, "%Y/%m/%d")
## bt12$vaccine <- as.numeric(bt12$vaccine.this.year==0 & (is.na(bt12$vaccine.date) |
##                            bt12$vaccine.date <= bt12$date))
bt12$children <- as.numeric((bt12$household.0.4 == "t" | bt12
    $household.5.18 == "t"))

bt12$using.transport <- (bt12$transport > 0)

bt12$postcode <- sub("[[:blank:]]+$", "", bt12$postcode)
bt12$postcode <- toupper(bt12$postcode)

bt12$work.postcode <- sub("[[:blank:]]+$", "", bt12$work.postcode)
bt12$work.postcode <- toupper(bt12$work.postcode)

bt12 <- bt12[country == "uk", "ur" := uk.ur$V3[match(bt12[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "uk.country" := uk.ur$V2[match(bt12[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "urban" := rep(0, length(bt12[country ==
                                                            "uk"]$postcode)), with=F]
bt12 <- bt12[country == "uk", "ur" := uk.ur$V3[match(bt12[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "uk.country" := uk.ur$V2[match(bt12[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "urban" := rep(0, length(bt12[country ==
                                                            "uk"]$postcode)), with=F]

bt12[country == "uk" & is.na(bt12$ur),]$urban <- 2

bt12[bt12$uk.country %in% c("E","W") & !(bt12$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt12[bt12$uk.country %in% c("E","W") & bt12$ur %in% c(1,5),]$urban <- 1

bt12[bt12$uk.country == "S" & bt12$ur %in% c(1,2),]$urban <- 1
bt12[bt12$uk.country == "S" & bt12$ur %in% c(3,4,5,6,7),]$urban <- 0

bt12[bt12$uk.country == "N" & bt12$ur %in% c(1,2,3,4),]$urban <- 1
bt12[bt12$uk.country == "N" & !(bt12$ur %in% c(5,6,7)),]$urban <- 0

bt12$urban <- as.factor(bt12$urban)

bt12 <- bt12[country == "uk", "work.ur" := uk.ur$V3[match(bt12[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "work.uk.country" := uk.ur$V2[match(bt12[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt12 <- bt12[country == "uk", "work.urban" := rep(0, length(bt12[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt12[country == "uk" & is.na(bt12$work.ur),]$work.urban <- 2

bt12[bt12$work.uk.country %in% c("E","W") & !(bt12$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt12[bt12$work.uk.country %in% c("E","W") & bt12$work.ur %in% c(1,5),]$work.urban <- 1

bt12[bt12$work.uk.country == "S" & bt12$work.ur %in% c(1,2),]$work.urban <- 1
bt12[bt12$work.uk.country == "S" & bt12$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt12[bt12$work.uk.country == "N" & bt12$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt12[bt12$work.uk.country == "N" & !(bt12$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt12$work.urban <- as.factor(bt12$work.urban)

data.12 <- list(symptoms = st12, background = bt12, contact = ct12)
saveRDS(data.12, "flusurvey_201112_raw.rds")

data.12 <- readRDS("flusurvey_201112_raw.rds")
st12 <- data.12$symptoms
bt12 <- data.12$background
ct12 <- data.12$contact

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt12 <- bt12[ct12[st12, roll=TRUE], roll = TRUE]

dt12 <- dt12[!is.na(global.id.number)]

saveRDS(dt12, "flusurvey_201112.rds")

dt12 <- readRDS("flusurvey_201112.rds")
## 2011

sf11 <- read.csv('201011/weekly_201011.csv', header=T, sep=',');
bf11 <- read.csv('201011/intake_201011.csv', header=T, sep=',');
cf11 <- read.csv('201011/contactdata_2011-04-06.csv', header=T, sep=',');

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf11$user_id))
translation$number <- 110000 + seq(1,nrow(translation))

## assign global id numbers
bf11$global.id.number <- translation$number[match(bf11$user_id,
                                                  translation$global_id)]
sf11$global.id.number <- translation$number[match(sf11$user_id,
                                                  translation$global_id)]
cf11$global.id.number <- translation$number[match(cf11$user_id,
                                                  translation$global_id)]

## put data in data tables (for the rolling join to be used later)
st11 <- data.table(sf11)
bt11 <- data.table(bf11)
ct11 <- data.table(cf11)

bt11$bid <- seq(1:nrow(bt11))
ct11$cid <- seq(1:nrow(ct11))

rm(sf11)
rm(bf11)
rm(cf11)

setnames(bt11, "user_id", "user_id.bg")
setnames(ct11, "user_id", "user_id.contacts")

st11$date <- as.Date(st11$date, format = "%d/%m/%Y")
bt11$date <- as.Date(bt11$created, format = "%d/%m/%Y")
ct11$date <- as.Date(sub(" .*", "", ct11$date), format = "%Y-%m-%d")

st11 <- st11[!duplicated(bt11[, list(global.id.number, date)], fromLast=T)]
bt11 <- bt11[!duplicated(bt11[, list(global.id.number, date)], fromLast=T)]
ct11 <- ct11[!duplicated(ct11[, list(global.id.number, date)], fromLast=T)]
setkey(st11, global.id.number, date)
setkey(bt11, global.id.number, date)
setkey(ct11, global.id.number, date)


conversation.categories.11 <- c("conversational.home.0-4", "conversational.home.5-18", "conversational.home.19-44", "conversational.home.45-64", "conversational.home.65+", "conversational.work.0-4", "conversational.work.5-18", "conversational.work.19-44", "conversational.work.45-64", "conversational.work.65+", "conversational.other.0-4", "conversational.other.5-18", "conversational.other.19-44", "conversational.other.45-64", "conversational.other.65+", "physical.home.0-4", "physical.home.5-18", "physical.home.19-44", "physical.home.45-64", "physical.home.65+", "physical.work.0-4", "physical.work.5-18", "physical.work.19-44", "physical.work.45-64", "physical.work.65+", "physical.other.0-4", "physical.other.5-18", "physical.other.19-44", "physical.other.45-64", "physical.other.65+")

for (category in conversation.categories.11) {
    ct11 <- ct11[get(category) == "None", paste(category) := "0"]
    ct11 <- ct11[, paste(category) := as.numeric(as.character(get(category)))]
    ct11 <- ct11[get(category) == 17, paste(category) := 25]
    ct11 <- ct11[get(category) == 18, paste(category) := 50]
    ct11 <- ct11[get(category) == 19, paste(category) := 100]
}

ct11 <- ct11[,"conversational.home" := get("conversational.home.0-4") +
                  get("conversational.home.5-18") + get("conversational.home.19-44") +
                  get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct11 <- ct11[,"conversational.work" := get("conversational.work.0-4") +
                  get("conversational.work.5-18") + get("conversational.work.19-44") +
                  get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct11 <- ct11[,"conversational.other" := get("conversational.other.0-4") +
                  get("conversational.other.5-18") + get("conversational.other.19-44") +
                  get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct11 <- ct11[,"conversational.0-4" := get("conversational.home.0-4") +
                  get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct11 <- ct11[,"conversational.5-18" := get("conversational.home.5-18") +
                  get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct11 <- ct11[,"conversational.19-44" := get("conversational.home.19-44") +
                  get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct11 <- ct11[,"conversational.45-64" := get("conversational.home.45-64") +
                  get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct11 <- ct11[,"conversational.65+" := get("conversational.home.65+") +
                  get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct11 <- ct11[,"conversational" := get("conversational.home") +
                  get("conversational.work") + get("conversational.other"), with=F]
ct11 <- ct11[,"physical.home" := get("physical.home.0-4") +
                  get("physical.home.5-18") + get("physical.home.19-44") +
                  get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct11 <- ct11[,"physical.work" := get("physical.work.0-4") +
                  get("physical.work.5-18") + get("physical.work.19-44") +
                  get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct11 <- ct11[,"physical.other" := get("physical.other.0-4") +
                  get("physical.other.5-18") + get("physical.other.19-44") +
                  get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct11 <- ct11[,"physical.0-4" := get("physical.home.0-4") +
                  get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct11 <- ct11[,"physical.5-18" := get("physical.home.5-18") +
                  get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct11 <- ct11[,"physical.19-44" := get("physical.home.19-44") +
                  get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct11 <- ct11[,"physical.45-64" := get("physical.home.45-64") +
                  get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct11 <- ct11[,"physical.65+" := get("physical.home.65+") +
                  get("physical.work.65+") + get("physical.other.65+"), with=F]
ct11 <- ct11[,"physical" := get("physical.home") +
                  get("physical.work") + get("physical.other"), with=F]

ct11$week <- format(ct11$date, format="%G-%W")
ct11[ct11$week=="2010-00"]$week <- "2010-53"

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
st11$ili <- ((st11$symptoms.suddenly == 0) &
             (st11$fever == 1 | st11$tired == 1 | st11$headache == 1 |
              st11$muscle.and.or.joint.pain == 1) &
             (st11$sore.throat == 1 | st11$cough == 1 | st11$shortness.breath == 1))
st11$ili <- as.numeric(st11$ili)

st11$ili.notired <- ((st11$symptoms.suddenly == 0) &
                     (st11$fever == 1 | st11$headache == 1 |
                      st11$muscle.and.or.joint.pain ==1) &
                     (st11$sore.throat == 1 | st11$cough ==1 |
                      st11$shortness.breath == 1))
st11$ili.notired <- as.numeric(st11$ili.notired)

st11$ili.fever <- ((st11$symptoms.suddenly == 0) &
                   (st11$fever == 1) &
                   (st11$sore.throat == 1 | st11$cough ==1 |
                    st11$shortness.breath == 1))
st11$ili.fever <- as.numeric(st11$ili.fever)


freq <-
    data.table(aggregate(st11$global.id.number,
                         by=list(st11$global.id.number),
                         length))
setkey(freq, Group.1)
st11 <- st11[freq]
setnames(st11, "x", "nReports")

mindate <-
    data.table(aggregate(st11$date,
                         by=list(st11$global.id.number),
                         min))
setkey(mindate, Group.1)
st11 <- st11[mindate]
setnames(st11, "x", "mindate")
maxdate <-
    data.table(aggregate(st11$date,
                         by=list(st11$global.id.number),
                         max))
setkey(maxdate, Group.1)
st11 <- st11[maxdate]
setnames(st11, "x", "maxdate")

st11$week <- format(st11$date, format="%G-%W")
st11[st11$week=="2010-00"]$week <- "2010-53"
st11$weekweight <- 1/table(st11$week)[st11$week]

st11$symptoms.start.date <- as.Date(st11$symptoms.start.date, "%Y-%m-%d")
st11$symptoms.end.date <- as.Date(st11$symptoms.end.date, "%Y-%m-%d")
st11$symptoms.start.week <- format(st11$symptoms.start.date, format="%G-%W")
st11[st11$symptoms.start.week=="2010-00"]$symptoms.start.week <- "2010-53"

st11$ili.self <- (st11$what.do.you.think == 0)
st11[is.na(ili.self)]$ili.self <- FALSE

## more variables to be used later
bt11 <- bt11[, country := "uk"]

bt11$birthdate <- as.Date(paste(bt11$birthmonth, "-01",sep=""))

bt11$atrisk <- 1 - bt11$norisk
bt11$age <-  0
bt11$age <- apply(bt11, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt11$agegroup <- cut(bt11$age, breaks=c(0,18,45,65, max(bt11$age, na.rm=T)),
                     include.lowest=T, right=F)
bt11 <- bt11[, vaccine.date := as.Date(bt11$date.vaccine, "%Y-%m-%d")]
bt11 <- bt11[, children := as.numeric((nb.household.0.4 > 0 | nb.household.5.18 > 0))]

bt11$using.transport <- (bt11$transport > 0)

bt11$postcode <- sub("[[:blank:]]+$", "", bt11$postcode)
bt11$postcode <- toupper(bt11$postcode)

bt11$work.postcode <- sub("[[:blank:]]+$", "", bt11$work.postcode)
bt11$work.postcode <- toupper(bt11$work.postcode)

bt11 <- bt11[country == "uk", "ur" := uk.ur$V3[match(bt11[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "uk.country" := uk.ur$V2[match(bt11[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "urban" := rep(0, length(bt11[country ==
                                                            "uk"]$postcode)), with=F]
bt11 <- bt11[country == "uk", "ur" := uk.ur$V3[match(bt11[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "uk.country" := uk.ur$V2[match(bt11[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "urban" := rep(0, length(bt11[country ==
                                                            "uk"]$postcode)), with=F]

bt11[country == "uk" & is.na(bt11$ur),]$urban <- 2

bt11[bt11$uk.country %in% c("E","W") & !(bt11$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt11[bt11$uk.country %in% c("E","W") & bt11$ur %in% c(1,5),]$urban <- 1

bt11[bt11$uk.country == "S" & bt11$ur %in% c(1,2),]$urban <- 1
bt11[bt11$uk.country == "S" & bt11$ur %in% c(3,4,5,6,7),]$urban <- 0

bt11[bt11$uk.country == "N" & bt11$ur %in% c(1,2,3,4),]$urban <- 1
bt11[bt11$uk.country == "N" & !(bt11$ur %in% c(5,6,7)),]$urban <- 0

bt11$urban <- as.factor(bt11$urban)

bt11 <- bt11[country == "uk", "work.ur" := uk.ur$V3[match(bt11[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "work.uk.country" := uk.ur$V2[match(bt11[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt11 <- bt11[country == "uk", "work.urban" := rep(0, length(bt11[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt11[country == "uk" & is.na(bt11$work.ur),]$work.urban <- 2

bt11[bt11$work.uk.country %in% c("E","W") & !(bt11$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt11[bt11$work.uk.country %in% c("E","W") & bt11$work.ur %in% c(1,5),]$work.urban <- 1

bt11[bt11$work.uk.country == "S" & bt11$work.ur %in% c(1,2),]$work.urban <- 1
bt11[bt11$work.uk.country == "S" & bt11$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt11[bt11$work.uk.country == "N" & bt11$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt11[bt11$work.uk.country == "N" & !(bt11$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt11$work.urban <- as.factor(bt11$work.urban)

data.11 <- list(symptoms = st11, background = bt11, contact = ct11)
saveRDS(data.11, "flusurvey_201011_raw.rds")

data.11 <- readRDS("flusurvey_201011_raw.rds")
st11 <- data.11$symptoms
bt11 <- data.11$background
ct11 <- data.11$contact


## rolling join of symptoms and background, by id number (first) and date
## (second)
dt11 <- bt11[ct11[st11, roll=TRUE], roll = TRUE]

dt11 <- dt11[!is.na(global.id.number)]

saveRDS(dt11, "flusurvey_201011.rds")

dt11 <- readRDS("flusurvey_201011.rds")

## 2010

## read tables
bf10 <- read.csv('200910/background_200910.csv', header=T, sep=';');
vf10 <- read.csv('200910/vaccine_200910.csv', header=T, sep=';');
cf10 <- read.csv('200910/contacts_200910.csv', header=T, sep=';');
sf10 <- read.csv('200910/symptoms_200910.csv', header=T, sep=';');

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf10$uid))
translation$number <- 100000 + seq(1,nrow(translation))

## assign global id numbers
bf10$global.id.number <- translation$number[match(bf10$uid, translation$global_id)]
vf10$global.id.number <- translation$number[match(vf10$uid, translation$global_id)]
sf10$global.id.number <- translation$number[match(sf10$uid, translation$global_id)]
cf10$global.id.number <- translation$number[match(cf10$uid, translation$global_id)]

## put data in data tables (for the rolling join to be used later)
bt10 <- data.table(bf10)
vt10 <- data.table(vf10)
ct10 <- data.table(cf10)
st10 <- data.table(sf10)

rm(sf10)
rm(bf10)
rm(cf10)
rm(vf10)

setnames(bt10, "uid", "uid.bg")
setnames(vt10, "uid", "uid.vaccine")
setnames(ct10, "uid", "uid.contacts")

st10$date <- as.Date(st10$date)
bt10$date <- as.Date(bt10$date)
ct10$date <- as.Date(ct10$date)
vt10$date <- as.Date(vt10$date)

st10 <- st10[!duplicated(bt10[, list(global.id.number, date)], fromLast=T)]
bt10 <- bt10[!duplicated(bt10[, list(global.id.number, date)], fromLast=T)]
vt10 <- vt10[!duplicated(vt10[, list(global.id.number, date)], fromLast=T)]
ct10 <- ct10[!duplicated(ct10[, list(global.id.number, date)], fromLast=T)]
setkey(st10, global.id.number, date)
setkey(bt10, global.id.number, date)
setkey(vt10, global.id.number, date)
setkey(ct10, global.id.number, date)

st10 <- st10[!is.na(uid)]
st10 <- st10[!is.na(date)]

setnames(bt10, 1, "bid")
setnames(ct10, 1, "cid")
setnames(vt10, 1, "vid")

st10 <- st10[-(1:6),]


conversation.categories.10 <- c("conversational.home.0-4", "conversational.home.5-18", "conversational.home.19-64", "conversational.home.65+", "conversational.work.0-4", "conversational.work.5-18", "conversational.work.19-64", "conversational.work.65+", "conversational.other.0-4", "conversational.other.5-18", "conversational.other.19-64", "conversational.other.65+", "physical.home.0-4", "physical.home.5-18", "physical.home.19-64", "physical.home.65+", "physical.work.0-4", "physical.work.5-18", "physical.work.19-64", "physical.work.65+", "physical.other.0-4", "physical.other.5-18", "physical.other.19-64", "physical.other.65+")

for (category in conversation.categories.10) {
    ct10 <- ct10[get(category) == "NULL", paste(category) := "0"]
    ct10 <- ct10[get(category) == "16-24", paste(category) := "16"]
    ct10 <- ct10[get(category) == "25-49", paste(category) := "25"]
    ct10 <- ct10[get(category) == "50-99", paste(category) := "50"]
    ct10 <- ct10[get(category) == "100 or more", paste(category) := "100"]
    ct10 <- ct10[, paste(category) := as.numeric(as.character(get(category)))]
}

ct10 <- ct10[,"conversational.home" := get("conversational.home.0-4") +
                  get("conversational.home.5-18") + get("conversational.home.19-64") +
                  get("conversational.home.65+"), with=F]
ct10 <- ct10[,"conversational.work" := get("conversational.work.0-4") +
                  get("conversational.work.5-18") + get("conversational.work.19-64") +
                  get("conversational.work.65+"), with=F]
ct10 <- ct10[,"conversational.other" := get("conversational.other.0-4") +
                  get("conversational.other.5-18") + get("conversational.other.19-64") +
                  get("conversational.other.65+"), with=F]
ct10 <- ct10[,"conversational.0-4" := get("conversational.home.0-4") +
                  get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct10 <- ct10[,"conversational.5-18" := get("conversational.home.5-18") +
                  get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct10 <- ct10[,"conversational.19-64" := get("conversational.home.19-64") +
                  get("conversational.work.19-64") + get("conversational.other.19-64"), with=F]
ct10 <- ct10[,"conversational.65+" := get("conversational.home.65+") +
                  get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct10 <- ct10[,"conversational" := get("conversational.home") +
                  get("conversational.work") + get("conversational.other"), with=F]
ct10 <- ct10[,"physical.home" := get("physical.home.0-4") +
                  get("physical.home.5-18") + get("physical.home.19-64") +
                  get("physical.home.65+"), with=F]
ct10 <- ct10[,"physical.work" := get("physical.work.0-4") +
                  get("physical.work.5-18") + get("physical.work.19-64") +
                  get("physical.work.65+"), with=F]
ct10 <- ct10[,"physical.other" := get("physical.other.0-4") +
                  get("physical.other.5-18") + get("physical.other.19-64") +
                  get("physical.other.65+"), with=F]
ct10 <- ct10[,"physical.0-4" := get("physical.home.0-4") +
                  get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct10 <- ct10[,"physical.5-18" := get("physical.home.5-18") +
                  get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct10 <- ct10[,"physical.19-64" := get("physical.home.19-64") +
                  get("physical.work.19-64") + get("physical.other.19-64"), with=F]
ct10 <- ct10[,"physical.65+" := get("physical.home.65+") +
                  get("physical.work.65+") + get("physical.other.65+"), with=F]
ct10 <- ct10[,"physical" := get("physical.home") +
                  get("physical.work") + get("physical.other"), with=F]


ct10$week <- format(ct10$date, format="%G-%W")
ct10[ct10$week=="2009-00"]$week <- "2009-53"

vt10 <- vt10[, vaccine.date := as.Date(vt10$date.vaccine, "%Y-%m-%d")]

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
st10$ili.fever <- ((st10$fever.suddenly == 1) &
                   (st10$sore.throat == 1 | st10$cough ==1))
st10$ili.fever <- as.numeric(st10$ili.fever)


freq <-
    data.table(aggregate(st10$global.id.number,
                         by=list(st10$global.id.number),
                         length))
setkey(freq, Group.1)
st10 <- st10[freq]
setnames(st10, "x", "nReports")

mindate <-
    data.table(aggregate(st10$date,
                         by=list(st10$global.id.number),
                         min))
setkey(mindate, Group.1)
st10 <- st10[mindate]
setnames(st10, "x", "mindate")
maxdate <-
    data.table(aggregate(st10$date,
                         by=list(st10$global.id.number),
                         max))
setkey(maxdate, Group.1)
st10 <- st10[maxdate]
setnames(st10, "x", "maxdate")

st10$week <- format(st10$date, format="%G-%W")
st10[st10$week=="2009-00"]$week <- "2009-53"
st10$weekweight <- 1/table(st10$week)[st10$week]

st10$symptoms.start.date <- as.Date(st10$symptoms.start, "%Y-%m-%d")
st10$symptoms.start.week <- format(st10$symptoms.start.date, format="%G-%W")
st10[st10$symptoms.start.week=="2009-00"]$symptoms.start.week <- "2009-53"

bt10 <- bt10[, country := "uk"]

bt10$birthdate <- as.Date(bt10$birthyear, "%Y")

## more variables to be used later
bt10$norisk <- factor(as.numeric(bt10$riskgroup == 0))
bt10$atrisk <- bt10$norisk
levels(bt10$atrisk) <- c(1,0)
bt10$atrisk <- as.numeric(paste(bt10$atrisk))

bt10$age <-  0
bt10$age <- apply(bt10, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt10$agegroup <- cut(bt10$age, breaks=c(0,18,45,65, max(bt10$age, na.rm=T)),
                     include.lowest=T, right=F)
bt10$nb.household.0.4 <- as.character(bt10$nb.household.0.4)
bt10$nb.household.5.18 <- as.character(bt10$nb.household.5.18)
bt10$nb.household.19.64 <- as.character(bt10$nb.household.19.64)
bt10 <- bt10[, "nb.household.65+" := as.character(get("nb.household.65+"))]
invalid <- bt10$nb.household.0.4 == "NULL" & bt10$nb.household.5.18 == "NULL" &
    bt10$nb.household.19.64 == "NULL" & bt10$nb.household.65. == "NULL"
bt10[invalid]$nb.household.0.4 <- "NA"
bt10[invalid]$nb.household.5.18 <- "NA"
bt10[invalid]$nb.household.19.64 <- "NA"
bt10[invalid]$nb.household.65. <- "NA"
bt10$nb.household.0.4 <- as.numeric(bt10$nb.household.0.4)
bt10$nb.household.5.18 <- as.numeric(bt10$nb.household.5.18)
bt10$nb.household.19.64 <- as.numeric(bt10$nb.household.19.64)
bt10 <- bt10[, "nb.household.65+" := as.numeric(get("nb.household.65+"))]

bt10$children <- as.numeric((bt10$nb.household.0.4 > 0 | bt10$nb.household.5.18 > 0))

bt10$postcode <- sub("[[:blank:]]+$", "", bt10$postcode)
bt10$postcode <- toupper(bt10$postcode)

bt10$work.postcode <- sub("[[:blank:]]+$", "", bt10$work.postcode)
bt10$work.postcode <- toupper(bt10$work.postcode)

bt10 <- bt10[country == "uk", "ur" := uk.ur$V3[match(bt10[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "uk.country" := uk.ur$V2[match(bt10[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "urban" := rep(0, length(bt10[country ==
                                                            "uk"]$postcode)), with=F]
bt10 <- bt10[country == "uk", "ur" := uk.ur$V3[match(bt10[country == "uk",]$postcode,
                                                     uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "uk.country" := uk.ur$V2[match(bt10[country ==
                                                                  "uk"]$postcode, uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "urban" := rep(0, length(bt10[country ==
                                                            "uk"]$postcode)), with=F]

bt10[country == "uk" & is.na(bt10$ur),]$urban <- 2

bt10[bt10$uk.country %in% c("E","W") & !(bt10$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
bt10[bt10$uk.country %in% c("E","W") & bt10$ur %in% c(1,5),]$urban <- 1

bt10[bt10$uk.country == "S" & bt10$ur %in% c(1,2),]$urban <- 1
bt10[bt10$uk.country == "S" & bt10$ur %in% c(3,4,5,6,7),]$urban <- 0

bt10[bt10$uk.country == "N" & bt10$ur %in% c(1,2,3,4),]$urban <- 1
bt10[bt10$uk.country == "N" & !(bt10$ur %in% c(5,6,7)),]$urban <- 0

bt10$urban <- as.factor(bt10$urban)

bt10 <- bt10[country == "uk", "work.ur" := uk.ur$V3[match(bt10[country ==
                                                               "uk",]$work.postcode, uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "work.uk.country" := uk.ur$V2[match(bt10[country ==
                                                                       "uk"]$work.postcode, uk.ur$V1)], with=F]
bt10 <- bt10[country == "uk", "work.urban" := rep(0, length(bt10[country ==
                                                                 "uk"]$work.postcode)), with=F]

bt10[country == "uk" & is.na(bt10$work.ur),]$work.urban <- 2

bt10[bt10$work.uk.country %in% c("E","W") & !(bt10$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
bt10[bt10$work.uk.country %in% c("E","W") & bt10$work.ur %in% c(1,5),]$work.urban <- 1

bt10[bt10$work.uk.country == "S" & bt10$work.ur %in% c(1,2),]$work.urban <- 1
bt10[bt10$work.uk.country == "S" & bt10$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

bt10[bt10$work.uk.country == "N" & bt10$work.ur %in% c(1,2,3,4),]$work.urban <- 1
bt10[bt10$work.uk.country == "N" & !(bt10$work.ur %in% c(5,6,7)),]$work.urban <- 0

bt10$work.urban <- as.factor(bt10$work.urban)

data.10 <- list(symptoms = st10, background = bt10, contact = ct10, vaccination = vt10)
saveRDS(data.10, "flusurvey_200910_raw.rds")

data.10 <- readRDS("flusurvey_200910_raw.rds")
st10 <- data.10$symptoms
bt10 <- data.10$background
ct10 <- data.10$contact
vt10 <- data.10$vaccination

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt10 <- bt10[vt10[ct10[st10, roll=TRUE], roll = TRUE], roll = TRUE]
dt10 <- dt10[!is.na(global.id.number)]

saveRDS(dt10, "flusurvey_200910.rds")

dt10 <- readRDS("flusurvey_200910.rds")

## merge what we can merge
join.vertical <- function (...) {
    x <- list(...)
    for (i in 1:(length(x) - 1)) {
        for (j in 2:(i + 1)) {
            names.diff <- setdiff(names(x[[j - 1]]), names(x[[i + 1]]))
            if (length(names.diff) > 0) {
                x[[j - 1]] <- x[[j - 1]][, !names.diff, with = F]
            }
        }
        names.diff.reverse <- setdiff(names(x[[i + 1]]), names(x[[i]]))
        if (length(names.diff.reverse) > 0) {
            x[[i + 1]] <- x[[i + 1]][, !names.diff.reverse, with = F]
        }
    }
    res <- do.call("rbind", c(x, list(use.names = T)))
}

st <- join.vertical(st15, st14, st13, st12, st11, st10)
bt <- join.vertical(bt15, bt14, bt13, bt12, bt11, bt10)
ct <- join.vertical(ct13, ct12, ct11, ct10)
dt <- join.vertical(dt15, dt14, dt13, dt12, dt11, dt10)

data <- list(symptoms = st, background = bt, contact = ct)
saveRDS(data, "flusurvey_200915_raw.rds")
saveRDS(dt, "flusurvey_200915.rds")
