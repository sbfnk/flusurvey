library(data.table)
library(ggplot2)
library(reshape)

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

## 2015
sf15 <- read.csv('weekly_15.csv', sep=',', header=T)
bf15 <- read.csv('intake_15.csv', sep=',', header=T)

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf15$global_id))
translation$number <- 200000 + seq(1,nrow(translation))

## assign global id numbers
bf15$global.id.number <- translation$number[match(bf15$global_id,
                                                  translation$global_id)]
sf15$global.id.number <- translation$number[match(sf15$global_id,
                                                  translation$global_id)]

## put data in data tables (for the rolling join to be used later)
st15 <- data.table(sf15)
bt15 <- data.table(bf15)
bt15$bid <- seq(1:nrow(bt15))

rm(sf15)
rm(bf15)

setnames(bt15, 2, "global_id.bg")

st15$date <- as.Date(st15$timestamp)
bt15$date <- as.Date(bt15$timestamp)

st15 <- st15[!duplicated(bt15[, list(global.id.number, date)], fromLast=T)]
bt15 <- bt15[!duplicated(bt15[, list(global.id.number, date)], fromLast=T)]
setkey(st15, global.id.number, date)
setkey(bt15, global.id.number, date)


## set convenient names
setnames(bt15, "Q0", "self")
setnames(bt15, "Q1", "gender")
setnames(bt15, "Q2", "birthmonth")
setnames(bt15, "Q3", "postcode")
setnames(bt15, "Q4", "main.activity")
setnames(bt15, "Q4b", "work.postcode.option")
setnames(bt15, "Q4b_0_open", "work.postcode")
setnames(bt15, "Q4c", "occupation")
setnames(bt15, "Q4d", "education")
setnames(bt15, "Q5_0", "frequent.contact.children")
setnames(bt15, "Q5_1", "frequent.contact.elderly")
setnames(bt15, "Q5_2", "frequent.contact.patients")
setnames(bt15, "Q5_3", "frequent.contact.people")
setnames(bt15, "Q5_4", "frequent.contact.none")
setnames(bt15, "Q6_0", "household.0.4")
setnames(bt15, "Q6_0_open", "nb.household.0.4")
setnames(bt15, "Q6_1", "household.5.18")
setnames(bt15, "Q6_1_open", "nb.household.5.18")
setnames(bt15, "Q6_2", "household.19.44")
setnames(bt15, "Q6_2_open", "nb.household.19.44")
setnames(bt15, "Q6_3", "household.45.64")
setnames(bt15, "Q6_3_open", "nb.household.45.64")
setnames(bt15, "Q6_4", "household.65+")
setnames(bt15, "Q6_4_open", "nb.household.65+")
setnames(bt15, "Q6b", "children.school")
setnames(bt15, "Q7", "transport")
setnames(bt15, "Q7b", "howlong.transport")
setnames(bt15, "Q8", "howoften.flulike")
setnames(bt15, "Q9", "vaccine.last.year")
setnames(bt15, "Q10", "vaccine.this.year")
setnames(bt15, "Q10b", "date.vaccine.option")
setnames(bt15, "Q10b_1_open", "date.vaccine")
setnames(bt15, "Q10c_0", "why.vaccine.riskgroup")
setnames(bt15, "Q10c_1", "why.vaccine.protected")
setnames(bt15, "Q10c_2", "why.vaccine.protect.others")
setnames(bt15, "Q10c_3", "why.vaccine.given.at.school")
setnames(bt15, "Q10c_4", "why.vaccine.doctor")
setnames(bt15, "Q10c_5", "why.vaccine.work.recommended")
setnames(bt15, "Q10c_6", "why.vaccine.convenient")
setnames(bt15, "Q10c_7", "why.vaccine.free")
setnames(bt15, "Q10c_8", "why.vaccine.nomiss.work")
setnames(bt15, "Q10c_9", "why.vaccine.always")
setnames(bt15, "Q10c_10", "why.vaccine.other")
setnames(bt15, "Q10d_0", "why.not.vaccine.notyet")
setnames(bt15, "Q10d_1", "why.not.vaccine.notoffered")
setnames(bt15, "Q10d_2", "why.not.vaccine.norisk")
setnames(bt15, "Q10d_3", "why.not.vaccine.natural")
setnames(bt15, "Q10d_4", "why.not.vaccine.noteffective")
setnames(bt15, "Q10d_5", "why.not.vaccine.minor")
setnames(bt15, "Q10d_6", "why.not.vaccine.unlikely")
setnames(bt15, "Q10d_7", "why.not.vaccine.cause")
setnames(bt15, "Q10d_8", "why.not.vaccine.side.effects")
setnames(bt15, "Q10d_9", "why.not.vaccine.dont.like")
setnames(bt15, "Q10d_10", "why.not.vaccine.unavailable")
setnames(bt15, "Q10d_11", "why.not.vaccine.not.free")
setnames(bt15, "Q10d_12", "why.not.vaccine.no.reason")
setnames(bt15, "Q10d_13", "why.not.vaccine.doctor")
setnames(bt15, "Q10d_15", "why.not.vaccine.other")
setnames(bt15, "Q11_0", "norisk")
setnames(bt15, "Q11_1", "risk.asthma")
setnames(bt15, "Q11_2", "risk.diabetes")
setnames(bt15, "Q11_3", "risk.lung")
setnames(bt15, "Q11_4", "risk.heart")
setnames(bt15, "Q11_5", "risk.kidney")
setnames(bt15, "Q11_6", "risk.immune")
setnames(bt15, "Q12", "pregnant")
setnames(bt15, "Q12b", "pregnant.trimester")
setnames(bt15, "Q13", "smoke")
setnames(bt15, "Q15_1", "allergy.hayfever")
setnames(bt15, "Q15_2", "allergy.dust")
setnames(bt15, "Q15_3", "allergy.animals")
setnames(bt15, "Q15_4", "allergy.other")
setnames(bt15, "Q15_5", "allergy.none")
setnames(bt15, "Q15_0", "diet.none")
setnames(bt15, "Q15_1", "diet.vegetarian")
setnames(bt15, "Q15_2", "diet.vegan")
setnames(bt15, "Q15_3", "diet.other")
setnames(bt15, "Q16_0", "pets.none")
setnames(bt15, "Q16_1", "pets.dogs")
setnames(bt15, "Q16_2", "pets.cats")
setnames(bt15, "Q16_3", "pets.birds")
setnames(bt15, "Q16_4", "pets.other")
setnames(bt15, "Q18_0", "howhear.radio.tv")
setnames(bt15, "Q18_1", "howhear.paper.magazine")
setnames(bt15, "Q18_2", "howhear.internet")
setnames(bt15, "Q18_3", "howhear.poster")
setnames(bt15, "Q18_4", "howhear.school.work")
setnames(bt15, "Q18_5", "howhear.bsa")
setnames(bt15, "Q18_6", "howhear.family.friends")
setnames(bt15, "Q18", "howhear.who")
setnames(bt15, "Q19a", "activity.vigorous")
setnames(bt15, "Q19b", "activity.moderate")
setnames(bt15, "Q19c", "activity.winter")

setnames(st15, "Q1_0", "no.symptoms")
setnames(st15, "Q1_1", "fever")
setnames(st15, "Q1_2", "chills")
setnames(st15, "Q1_3", "blocked.runny.nose")
setnames(st15, "Q1_4", "sneezing")
setnames(st15, "Q1_5", "sore.throat")
setnames(st15, "Q1_6", "cough")
setnames(st15, "Q1_7", "shortness.breath")
setnames(st15, "Q1_8", "headache")
setnames(st15, "Q1_9", "muscle.and.or.joint.pain")
setnames(st15, "Q1_10", "chest.pain")
setnames(st15, "Q1_11", "tired")
setnames(st15, "Q1_12", "loss.appetite")
setnames(st15, "Q1_13", "phlegm")
setnames(st15, "Q1_15", "watery.eyes")
setnames(st15, "Q1_15", "nausea")
setnames(st15, "Q1_16", "vomiting")
setnames(st15, "Q1_17", "diarrhoea")
setnames(st15, "Q1_18", "stomach.ache")
setnames(st15, "Q1_19", "other.symptoms")
setnames(st15, "Q2", "same")
setnames(st15, "Q3", "symptoms.start.option")
setnames(st15, "Q3_0_open", "symptoms.start.date")
setnames(st15, "Q4", "symptoms.end.option")
setnames(st15, "Q4_0_open", "symptoms.end.date")
setnames(st15, "Q5", "symptoms.suddenly")
setnames(st15, "Q6", "fever.start.option")
setnames(st15, "Q6_1_open", "fever.start")
setnames(st15, "Q6b", "fever.suddenly")
setnames(st15, "Q6c", "fever.temperature")
setnames(st15, "Q6d", "fever.temperature.value")
setnames(st15, "Q7_0", "visit.medical.service.no")
setnames(st15, "Q7_1", "visit.medical.service.gp")
setnames(st15, "Q7_2", "visit.medical.service.ae")
setnames(st15, "Q7_3", "visit.medical.service.hospital")
setnames(st15, "Q7_4", "visit.medical.service.other")
setnames(st15, "Q7_5", "visit.medical.service.appointment")
setnames(st15, "Q7b_multi_row1_col1", "visit.medical.service.howsoon.gp.receptionist")
setnames(st15, "Q7b_multi_row2_col1", "visit.medical.service.howsoon.gp.doctor.nurse")
setnames(st15, "Q7b_multi_row3_col1", "visit.medical.service.howsoon.nhs")
setnames(st15, "Q7b_multi_row4_col1", "visit.medical.service.howsoon.other")
setnames(st15, "Q8_0", "contact.medical.service.no")
setnames(st15, "Q8_1", "contact.medical.service.gp.receptionist")
setnames(st15, "Q8_2", "contact.medical.service.gp.doctor")
setnames(st15, "Q8_3", "contact.medical.service.nhs")
setnames(st15, "Q8_4", "contact.medical.service.npfs")
setnames(st15, "Q8_5", "contact.medical.service.other")
setnames(st15, "Q8b_multi_row1_col1", "contact.medical.service.howsoon.gp.receptionist")
setnames(st15, "Q8b_multi_row2_col1", "contact.medical.service.howsoon.gp.doctor.nurse")
setnames(st15, "Q8b_multi_row3_col1", "contact.medical.service.howsoon.nhs")
setnames(st15, "Q8b_multi_row4_col1", "contact.medical.service.howsoon.other")
setnames(st15, "Q9_0", "no.medication")
setnames(st15, "Q9_1", "medication.painkillers")
setnames(st15, "Q9_2", "medication.cough")
setnames(st15, "Q9_3", "medication.antiviral")
setnames(st15, "Q9_4", "medication.antibiotic")
setnames(st15, "Q9_5", "medication.other")
setnames(st15, "Q9_6", "medication.dontknow")
setnames(st15, "Q9b", "medication.howsoon")
setnames(st15, "Q10", "alter.routine")
setnames(st15, "Q10b", "still.altered")
setnames(st15, "Q10c", "howlong.altered")
setnames(st15, "Q11", "what.do.you.think")
setnames(st15, "Q12", "health.score")

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
symptoms.15 <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other.symptoms")

for (symptom in symptoms.15) {
    st15 <- st15[get(symptom) == "f", paste("symptom.", symptom, sep = "") := as.integer(0), with = F]
    st15 <- st15[get(symptom) == "t", paste("symptom.", symptom, sep = "") := as.integer(1), with = F]
    st15 <- st15[, !symptom, with = F]
    setnames(st15, paste("symptom.", symptom, sep = ""), symptom)
}

st15$ili <- ((st15$symptoms.suddenly == 0) &
             (st15$fever == 1 | st15$tired == 1 |
              st15$headache == 1 |
              st15$muscle.and.or.joint.pain ==1) &
             (st15$sore.throat == 1 | st15$cough ==1 |
              st15$shortness.breath == 1))
st15$ili <- as.numeric(st15$ili)

st15$ili.notired <- ((st15$symptoms.suddenly == 0) &
                     (st15$fever == 1 | st15$headache == 1 |
                      st15$muscle.and.or.joint.pain ==1) &
                     (st15$sore.throat == 1 | st15$cough ==1 |
                      st15$shortness.breath == 1))
st15$ili.notired <- as.numeric(st15$ili.notired)

st15$ili.fever <- ((st15$symptoms.suddenly == 0) &
                   (st15$fever == 1) &
                   (st15$sore.throat == 1 | st15$cough ==1 |
                    st15$shortness.breath == 1))
st15$ili.fever <- as.numeric(st15$ili.fever)


freq <-
    data.table(aggregate(st15$global.id.number,
                         by=list(st15$global.id.number),
                         length))
setkey(freq, Group.1)
st15 <- st15[freq]
setnames(st15, "x", "nReports")

mindate <-
    data.table(aggregate(st15$date,
                         by=list(st15$global.id.number),
                         min))
setkey(mindate, Group.1)
st15 <- st15[mindate]
setnames(st15, "x", "mindate")
maxdate <-
    data.table(aggregate(st15$date,
                         by=list(st15$global.id.number),
                         max))
setkey(maxdate, Group.1)
st15 <- st15[maxdate]
setnames(st15, "x", "maxdate")

st15$week <- format(st15$date, format="%G-%W")
st15[st15$week=="2015-00"]$week <- "2015-53"

st15$symptoms.start.date <- as.Date(st15$symptoms.start.date, "%Y-%m-%d")
st15$symptoms.end.date <- as.Date(st15$symptoms.end.date, "%Y-%m-%d")
st15$symptoms.start.week <- format(st15$symptoms.start.date, format="%G-%W")
st15[st15$symptoms.start.week=="2015-00"]$symptoms.start.week <- "2013-53"
st15[st15$symptoms.start.week=="2015-52"]$symptoms.start.week <- "2013-52"

## more variables to be used later
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

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt15 <- bt15[st15, roll=TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt15 <- dt15[!is.na(global.id.number)]

saveRDS(dt15, "flusurvey_201315.rds")

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
setnames(bt14, "Q0", "self")
setnames(bt14, "Q1", "gender")
setnames(bt14, "Q2", "birthmonth")
setnames(bt14, "Q3", "postcode")
setnames(bt14, "Q4", "main.activity")
setnames(bt14, "Q4b", "work.postcode.option")
setnames(bt14, "Q4b_0_open", "work.postcode")
setnames(bt14, "Q4c", "occupation")
setnames(bt14, "Q4d", "education")
setnames(bt14, "Q5_0", "frequent.contact.children")
setnames(bt14, "Q5_1", "frequent.contact.elderly")
setnames(bt14, "Q5_2", "frequent.contact.patients")
setnames(bt14, "Q5_3", "frequent.contact.people")
setnames(bt14, "Q5_4", "frequent.contact.none")
setnames(bt14, "Q6_0", "household.0.4")
setnames(bt14, "Q6_0_open", "nb.household.0.4")
setnames(bt14, "Q6_1", "household.5.18")
setnames(bt14, "Q6_1_open", "nb.household.5.18")
setnames(bt14, "Q6_2", "household.19.44")
setnames(bt14, "Q6_2_open", "nb.household.19.44")
setnames(bt14, "Q6_3", "household.45.64")
setnames(bt14, "Q6_3_open", "nb.household.45.64")
setnames(bt14, "Q6_4", "household.65+")
setnames(bt14, "Q6_4_open", "nb.household.65+")
setnames(bt14, "Q6b", "children.school")
setnames(bt14, "Q7", "transport")
setnames(bt14, "Q7b", "howlong.transport")
setnames(bt14, "Q8", "howoften.flulike")
setnames(bt14, "Q9", "vaccine.last.year")
setnames(bt14, "Q10", "vaccine.this.year")
setnames(bt14, "Q10b", "date.vaccine.option")
setnames(bt14, "Q10b_1_open", "date.vaccine")
setnames(bt14, "Q10c_0", "why.vaccine.riskgroup")
setnames(bt14, "Q10c_1", "why.vaccine.protected")
setnames(bt14, "Q10c_2", "why.vaccine.protect.others")
setnames(bt14, "Q10c_3", "why.vaccine.given.at.school")
setnames(bt14, "Q10c_4", "why.vaccine.doctor")
setnames(bt14, "Q10c_5", "why.vaccine.work.recommended")
setnames(bt14, "Q10c_6", "why.vaccine.convenient")
setnames(bt14, "Q10c_7", "why.vaccine.free")
setnames(bt14, "Q10c_8", "why.vaccine.nomiss.work")
setnames(bt14, "Q10c_9", "why.vaccine.always")
setnames(bt14, "Q10c_10", "why.vaccine.other")
setnames(bt14, "Q10d_0", "why.not.vaccine.notyet")
setnames(bt14, "Q10d_1", "why.not.vaccine.notoffered")
setnames(bt14, "Q10d_2", "why.not.vaccine.norisk")
setnames(bt14, "Q10d_3", "why.not.vaccine.natural")
setnames(bt14, "Q10d_4", "why.not.vaccine.noteffective")
setnames(bt14, "Q10d_5", "why.not.vaccine.minor")
setnames(bt14, "Q10d_6", "why.not.vaccine.unlikely")
setnames(bt14, "Q10d_7", "why.not.vaccine.cause")
setnames(bt14, "Q10d_8", "why.not.vaccine.side.effects")
setnames(bt14, "Q10d_9", "why.not.vaccine.dont.like")
setnames(bt14, "Q10d_10", "why.not.vaccine.unavailable")
setnames(bt14, "Q10d_11", "why.not.vaccine.not.free")
setnames(bt14, "Q10d_12", "why.not.vaccine.no.reason")
setnames(bt14, "Q10d_13", "why.not.vaccine.doctor")
setnames(bt14, "Q10d_14", "why.not.vaccine.other")
setnames(bt14, "Q11_0", "norisk")
setnames(bt14, "Q11_1", "risk.asthma")
setnames(bt14, "Q11_2", "risk.diabetes")
setnames(bt14, "Q11_3", "risk.lung")
setnames(bt14, "Q11_4", "risk.heart")
setnames(bt14, "Q11_5", "risk.kidney")
setnames(bt14, "Q11_6", "risk.immune")
setnames(bt14, "Q12", "pregnant")
setnames(bt14, "Q12b", "pregnant.trimester")
setnames(bt14, "Q13", "smoke")
setnames(bt14, "Q14_1", "allergy.hayfever")
setnames(bt14, "Q14_2", "allergy.dust")
setnames(bt14, "Q14_3", "allergy.animals")
setnames(bt14, "Q14_4", "allergy.other")
setnames(bt14, "Q14_5", "allergy.none")
setnames(bt14, "Q15_0", "diet.none")
setnames(bt14, "Q15_1", "diet.vegetarian")
setnames(bt14, "Q15_2", "diet.vegan")
setnames(bt14, "Q15_3", "diet.other")
setnames(bt14, "Q16_0", "pets.none")
setnames(bt14, "Q16_1", "pets.dogs")
setnames(bt14, "Q16_2", "pets.cats")
setnames(bt14, "Q16_3", "pets.birds")
setnames(bt14, "Q16_4", "pets.other")
setnames(bt14, "Q18_0", "howhear.radio.tv")
setnames(bt14, "Q18_1", "howhear.paper.magazine")
setnames(bt14, "Q18_2", "howhear.internet")
setnames(bt14, "Q18_3", "howhear.poster")
setnames(bt14, "Q18_4", "howhear.school.work")
setnames(bt14, "Q18_5", "howhear.bsa")
setnames(bt14, "Q18_6", "howhear.family.friends")
setnames(bt14, "Q18", "howhear.who")
setnames(bt14, "Q19a", "activity.vigorous")
setnames(bt14, "Q19b", "activity.moderate")
setnames(bt14, "Q19c", "activity.winter")

setnames(st14, "Q1_0", "no.symptoms")
setnames(st14, "Q1_1", "fever")
setnames(st14, "Q1_2", "chills")
setnames(st14, "Q1_3", "blocked.runny.nose")
setnames(st14, "Q1_4", "sneezing")
setnames(st14, "Q1_5", "sore.throat")
setnames(st14, "Q1_6", "cough")
setnames(st14, "Q1_7", "shortness.breath")
setnames(st14, "Q1_8", "headache")
setnames(st14, "Q1_9", "muscle.and.or.joint.pain")
setnames(st14, "Q1_10", "chest.pain")
setnames(st14, "Q1_11", "tired")
setnames(st14, "Q1_12", "loss.appetite")
setnames(st14, "Q1_13", "phlegm")
setnames(st14, "Q1_14", "watery.eyes")
setnames(st14, "Q1_15", "nausea")
setnames(st14, "Q1_16", "vomiting")
setnames(st14, "Q1_17", "diarrhoea")
setnames(st14, "Q1_18", "stomach.ache")
setnames(st14, "Q1_19", "other.symptoms")
setnames(st14, "Q2", "same")
setnames(st14, "Q3", "symptoms.start.option")
setnames(st14, "Q3_0_open", "symptoms.start.date")
setnames(st14, "Q4", "symptoms.end.option")
setnames(st14, "Q4_0_open", "symptoms.end.date")
setnames(st14, "Q5", "symptoms.suddenly")
setnames(st14, "Q6", "fever.start.option")
setnames(st14, "Q6_1_open", "fever.start")
setnames(st14, "Q6b", "fever.suddenly")
setnames(st14, "Q6c", "fever.temperature")
setnames(st14, "Q6d", "fever.temperature.value")
setnames(st14, "Q7_0", "visit.medical.service.no")
setnames(st14, "Q7_1", "visit.medical.service.gp")
setnames(st14, "Q7_2", "visit.medical.service.ae")
setnames(st14, "Q7_3", "visit.medical.service.hospital")
setnames(st14, "Q7_4", "visit.medical.service.other")
setnames(st14, "Q7_5", "visit.medical.service.appointment")
setnames(st14, "Q7b_multi_row1_col1", "visit.medical.service.howsoon.gp.receptionist")
setnames(st14, "Q7b_multi_row2_col1", "visit.medical.service.howsoon.gp.doctor.nurse")
setnames(st14, "Q7b_multi_row3_col1", "visit.medical.service.howsoon.nhs")
setnames(st14, "Q7b_multi_row4_col1", "visit.medical.service.howsoon.other")
setnames(st14, "Q8_0", "contact.medical.service.no")
setnames(st14, "Q8_1", "contact.medical.service.gp.receptionist")
setnames(st14, "Q8_2", "contact.medical.service.gp.doctor")
setnames(st14, "Q8_3", "contact.medical.service.nhs")
setnames(st14, "Q8_4", "contact.medical.service.npfs")
setnames(st14, "Q8_5", "contact.medical.service.other")
setnames(st14, "Q8b_multi_row1_col1", "contact.medical.service.howsoon.gp.receptionist")
setnames(st14, "Q8b_multi_row2_col1", "contact.medical.service.howsoon.gp.doctor.nurse")
setnames(st14, "Q8b_multi_row3_col1", "contact.medical.service.howsoon.nhs")
setnames(st14, "Q8b_multi_row4_col1", "contact.medical.service.howsoon.other")
setnames(st14, "Q9_0", "no.medication")
setnames(st14, "Q9_1", "medication.painkillers")
setnames(st14, "Q9_2", "medication.cough")
setnames(st14, "Q9_3", "medication.antiviral")
setnames(st14, "Q9_4", "medication.antibiotic")
setnames(st14, "Q9_5", "medication.other")
setnames(st14, "Q9_6", "medication.dontknow")
setnames(st14, "Q9b", "medication.howsoon")
setnames(st14, "Q10", "alter.routine")
setnames(st14, "Q10b", "still.altered")
setnames(st14, "Q10c", "howlong.altered")
setnames(st14, "Q11", "what.do.you.think")
setnames(st14, "Q12", "health.score")

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

st14$ili <- ((st14$symptoms.suddenly == 0) &
             (st14$fever == 1 | st14$tired == 1 |
              st14$headache == 1 |
              st14$muscle.and.or.joint.pain ==1) &
             (st14$sore.throat == 1 | st14$cough ==1 |
              st14$shortness.breath == 1))
st14$ili <- as.numeric(st14$ili)

st14$ili.notired <- ((st14$symptoms.suddenly == 0) &
                     (st14$fever == 1 | st14$headache == 1 |
                      st14$muscle.and.or.joint.pain ==1) &
                     (st14$sore.throat == 1 | st14$cough ==1 |
                      st14$shortness.breath == 1))
st14$ili.notired <- as.numeric(st14$ili.notired)

st14$ili.fever <- ((st14$symptoms.suddenly == 0) &
                   (st14$fever == 1) &
                   (st14$sore.throat == 1 | st14$cough ==1 |
                    st14$shortness.breath == 1))
st14$ili.fever <- as.numeric(st14$ili.fever)


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

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt14 <- bt14[st14, roll=TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt14 <- dt14[!is.na(global.id.number)]

saveRDS(dt14, "flusurvey_201314.rds")

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
setnames(bt13, "Q0", "self")
setnames(bt13, "Q1", "gender")
setnames(bt13, "Q2", "birthmonth")
setnames(bt13, "Q3", "postcode")
setnames(bt13, "Q4", "main.activity")
setnames(bt13, "Q4b", "work.postcode.option")
setnames(bt13, "Q4b_0_open", "work.postcode")
setnames(bt13, "Q4c", "occupation")
setnames(bt13, "Q4d_0", "no.education")
setnames(bt13, "Q4d_1", "education.gcse")
setnames(bt13, "Q4d_2", "education.alevels")
setnames(bt13, "Q4d_3", "education.bsc")
setnames(bt13, "Q4d_4", "education.msc")
setnames(bt13, "Q4d_5", "education.stillin")
setnames(bt13, "Q5_0", "frequent.contact.children")
setnames(bt13, "Q5_1", "frequent.contact.elderly")
setnames(bt13, "Q5_2", "frequent.contact.patients")
setnames(bt13, "Q5_3", "frequent.contact.people")
setnames(bt13, "Q5_4", "frequent.contact.none")
setnames(bt13, "Q6_0", "household.0.4")
setnames(bt13, "Q6_0_open", "nb.household.0.4")
setnames(bt13, "Q6_1", "household.5.18")
setnames(bt13, "Q6_1_open", "nb.household.5.18")
setnames(bt13, "Q6_2", "household.19.44")
setnames(bt13, "Q6_2_open", "nb.household.19.44")
setnames(bt13, "Q6_3", "household.45.64")
setnames(bt13, "Q6_3_open", "nb.household.45.64")
setnames(bt13, "Q6_4", "household.65+")
setnames(bt13, "Q6_4_open", "nb.household.65+")
setnames(bt13, "Q6b", "children.school")
setnames(bt13, "Q7", "transport")
setnames(bt13, "Q7b", "howlong.transport")
setnames(bt13, "Q8", "howoften.flulike")
setnames(bt13, "Q9", "vaccine.last.year")
setnames(bt13, "Q10", "vaccine.this.year")
setnames(bt13, "Q10b", "date.vaccine.option")
setnames(bt13, "Q10b_1_open", "date.vaccine")
setnames(bt13, "Q10c_0", "why.vaccine.riskgroup")
setnames(bt13, "Q10c_1", "why.vaccine.protected")
setnames(bt13, "Q10c_2", "why.vaccine.protect.others")
setnames(bt13, "Q10c_3", "why.vaccine.doctor")
setnames(bt13, "Q10c_4", "why.vaccine.work.recommended")
setnames(bt13, "Q10c_5", "why.vaccine.convenient")
setnames(bt13, "Q10c_6", "why.vaccine.free")
setnames(bt13, "Q10c_7", "why.vaccine.nomiss.work")
setnames(bt13, "Q10c_8", "why.vaccine.always")
setnames(bt13, "Q10c_9", "why.vaccine.other")
setnames(bt13, "Q10d_0", "why.not.vaccine.notyet")
setnames(bt13, "Q10d_1", "why.not.vaccine.notoffered")
setnames(bt13, "Q10d_2", "why.not.vaccine.norisk")
setnames(bt13, "Q10d_3", "why.not.vaccine.natural")
setnames(bt13, "Q10d_4", "why.not.vaccine.noteffective")
setnames(bt13, "Q10d_5", "why.not.vaccine.minor")
setnames(bt13, "Q10d_6", "why.not.vaccine.unlikely")
setnames(bt13, "Q10d_7", "why.not.vaccine.cause")
setnames(bt13, "Q10d_8", "why.not.vaccine.side.effects")
setnames(bt13, "Q10d_9", "why.not.vaccine.dont.like")
setnames(bt13, "Q10d_10", "why.not.vaccine.unavailable")
setnames(bt13, "Q10d_11", "why.not.vaccine.not.free")
setnames(bt13, "Q10d_12", "why.not.vaccine.no.reason")
setnames(bt13, "Q10d_13", "why.not.vaccine.doctor")
setnames(bt13, "Q10d_14", "why.not.vaccine.other")
setnames(bt13, "Q11_0", "norisk")
setnames(bt13, "Q11_1", "risk.asthma")
setnames(bt13, "Q11_2", "risk.diabetes")
setnames(bt13, "Q11_3", "risk.lung")
setnames(bt13, "Q11_4", "risk.heart")
setnames(bt13, "Q11_5", "risk.kidney")
setnames(bt13, "Q11_6", "risk.immune")
setnames(bt13, "Q12", "pregnant")
setnames(bt13, "Q12b", "pregnant.trimester")
setnames(bt13, "Q13", "smoke")
setnames(bt13, "Q14_1", "allergy.hayfever")
setnames(bt13, "Q14_2", "allergy.dust")
setnames(bt13, "Q14_3", "allergy.animals")
setnames(bt13, "Q14_4", "allergy.other")
setnames(bt13, "Q14_5", "allergy.none")
setnames(bt13, "Q15_0", "diet.none")
setnames(bt13, "Q15_1", "diet.vegetarian")
setnames(bt13, "Q15_2", "diet.vegan")
setnames(bt13, "Q15_3", "diet.low-calorie")
setnames(bt13, "Q15_4", "diet.other")
setnames(bt13, "Q16_0", "pets.none")
setnames(bt13, "Q16_1", "pets.dogs")
setnames(bt13, "Q16_2", "pets.cats")
setnames(bt13, "Q16_3", "pets.birds")
setnames(bt13, "Q16_4", "pets.other")
setnames(bt13, "Q17_0", "howhear.radio.tv")
setnames(bt13, "Q17_1", "howhear.paper.magazine")
setnames(bt13, "Q17_2", "howhear.internet")
setnames(bt13, "Q17_3", "howhear.poster")
setnames(bt13, "Q17_4", "howhear.family.friends")
setnames(bt13, "Q17_5", "howhear.other")

setnames(st13, "Q1_0", "no.symptoms")
setnames(st13, "Q1_1", "fever")
setnames(st13, "Q1_2", "chills")
setnames(st13, "Q1_3", "blocked.runny.nose")
setnames(st13, "Q1_4", "sneezing")
setnames(st13, "Q1_5", "sore.throat")
setnames(st13, "Q1_6", "cough")
setnames(st13, "Q1_7", "shortness.breath")
setnames(st13, "Q1_8", "headache")
setnames(st13, "Q1_9", "muscle.and.or.joint.pain")
setnames(st13, "Q1_10", "chest.pain")
setnames(st13, "Q1_11", "tired")
setnames(st13, "Q1_12", "loss.appetite")
setnames(st13, "Q1_13", "phlegm")
setnames(st13, "Q1_14", "watery.eyes")
setnames(st13, "Q1_15", "nausea")
setnames(st13, "Q1_16", "vomiting")
setnames(st13, "Q1_17", "diarrhoea")
setnames(st13, "Q1_18", "stomach.ache")
setnames(st13, "Q1_19", "other.symptoms")
setnames(st13, "Q2", "same")
setnames(st13, "Q3", "symptoms.start.option")
setnames(st13, "Q3_0_open", "symptoms.start.date")
setnames(st13, "Q4", "symptoms.end.option")
setnames(st13, "Q4_0_open", "symptoms.end.date")
setnames(st13, "Q5", "symptoms.suddenly")
setnames(st13, "Q6", "fever.start.option")
setnames(st13, "Q6_1_open", "fever.start")
setnames(st13, "Q6b", "fever.suddenly")
setnames(st13, "Q6c", "fever.temperature")
setnames(st13, "Q6d", "fever.temperature.value")
setnames(st13, "Q7_0", "visit.medical.service.no")
setnames(st13, "Q7_1", "visit.medical.service.gp")
setnames(st13, "Q7_2", "visit.medical.service.ae")
setnames(st13, "Q7_3", "visit.medical.service.hospital")
setnames(st13, "Q7_4", "visit.medical.service.other")
setnames(st13, "Q7_5", "visit.medical.service.appointment")
setnames(st13, "Q7b_multi_row1_col1", "visit.medical.service.howsoon.gp.receptionist")
setnames(st13, "Q7b_multi_row2_col1", "visit.medical.service.howsoon.gp.doctor.nurse")
setnames(st13, "Q7b_multi_row3_col1", "visit.medical.service.howsoon.nhs")
setnames(st13, "Q7b_multi_row4_col1", "visit.medical.service.howsoon.other")
setnames(st13, "Q8_0", "contact.medical.service.no")
setnames(st13, "Q8_1", "contact.medical.service.gp.receptionist")
setnames(st13, "Q8_2", "contact.medical.service.gp.doctor")
setnames(st13, "Q8_3", "contact.medical.service.nhs")
setnames(st13, "Q8_4", "contact.medical.service.npfs")
setnames(st13, "Q8_5", "contact.medical.service.other")
setnames(st13, "Q8b_multi_row1_col1", "contact.medical.service.howsoon.gp.receptionist")
setnames(st13, "Q8b_multi_row2_col1", "contact.medical.service.howsoon.gp.doctor.nurse")
setnames(st13, "Q8b_multi_row3_col1", "contact.medical.service.howsoon.nhs")
setnames(st13, "Q8b_multi_row4_col1", "contact.medical.service.howsoon.other")
setnames(st13, "Q9_0", "no.medication")
setnames(st13, "Q9_1", "medication.painkillers")
setnames(st13, "Q9_2", "medication.cough")
setnames(st13, "Q9_3", "medication.antiviral")
setnames(st13, "Q9_4", "medication.antibiotic")
setnames(st13, "Q9_5", "medication.other")
setnames(st13, "Q9_6", "medication.dontknow")
setnames(st13, "Q9b", "medication.howsoon")
setnames(st13, "Q10", "alter.routine")
setnames(st13, "Q10b", "still.altered")
setnames(st13, "Q10c", "howlong.altered")
setnames(st13, "Q11", "what.do.you.think")
setnames(st13, "Q12", "health.score")
## setnames(st13, "Q12_multi_row1_col1", "howmany.household.ili")
## setnames(st13, "Q13_multi_row1_col1", "howmany.other.ili")

setnames(ct13, "Q1_multi_row1_col1", "conversational.home.0-4")
setnames(ct13, "Q1_multi_row1_col2", "conversational.home.5-18")
setnames(ct13, "Q1_multi_row1_col3", "conversational.home.19-44")
setnames(ct13, "Q1_multi_row1_col4", "conversational.home.45-64")
setnames(ct13, "Q1_multi_row1_col5", "conversational.home.65+")
setnames(ct13, "Q1_multi_row2_col1", "conversational.work.0-4")
setnames(ct13, "Q1_multi_row2_col2", "conversational.work.5-18")
setnames(ct13, "Q1_multi_row2_col3", "conversational.work.19-44")
setnames(ct13, "Q1_multi_row2_col4", "conversational.work.45-64")
setnames(ct13, "Q1_multi_row2_col5", "conversational.work.65+")
setnames(ct13, "Q1_multi_row3_col1", "conversational.other.0-4")
setnames(ct13, "Q1_multi_row3_col2", "conversational.other.5-18")
setnames(ct13, "Q1_multi_row3_col3", "conversational.other.19-44")
setnames(ct13, "Q1_multi_row3_col4", "conversational.other.45-64")
setnames(ct13, "Q1_multi_row3_col5", "conversational.other.65+")
setnames(ct13, "Q2_multi_row1_col1", "physical.home.0-4")
setnames(ct13, "Q2_multi_row1_col2", "physical.home.5-18")
setnames(ct13, "Q2_multi_row1_col3", "physical.home.19-44")
setnames(ct13, "Q2_multi_row1_col4", "physical.home.45-64")
setnames(ct13, "Q2_multi_row1_col5", "physical.home.65+")
setnames(ct13, "Q2_multi_row2_col1", "physical.work.0-4")
setnames(ct13, "Q2_multi_row2_col2", "physical.work.5-18")
setnames(ct13, "Q2_multi_row2_col3", "physical.work.19-44")
setnames(ct13, "Q2_multi_row2_col4", "physical.work.45-64")
setnames(ct13, "Q2_multi_row2_col5", "physical.work.65+")
setnames(ct13, "Q2_multi_row3_col1", "physical.other.0-4")
setnames(ct13, "Q2_multi_row3_col2", "physical.other.5-18")
setnames(ct13, "Q2_multi_row3_col3", "physical.other.19-44")
setnames(ct13, "Q2_multi_row3_col4", "physical.other.45-64")
setnames(ct13, "Q2_multi_row3_col5", "physical.other.65+")
setnames(ct13, "Q3", "public.transport")
setnames(ct13, "Q4", "enclosed.indoor.space")
setnames(ct13, "Q5", "furthest.travelled")

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

st13$ili <- ((st13$symptoms.suddenly == 0) &
             (st13$fever == 1 | st13$tired == 1 | st13$headache == 1 |
              st13$muscle.and.or.joint.pain ==1) &
             (st13$sore.throat == 1 | st13$cough ==1 |
              st13$shortness.breath == 1))
st13$ili <- as.numeric(st13$ili)

st13$ili.notired <- ((st13$symptoms.suddenly == 0) &
                     (st13$fever == 1 | st13$headache == 1 |
                      st13$muscle.and.or.joint.pain ==1) &
                     (st13$sore.throat == 1 | st13$cough ==1 |
                      st13$shortness.breath == 1))
st13$ili.notired <- as.numeric(st13$ili.notired)

st13$ili.fever <- ((st13$symptoms.suddenly == 0) &
                   (st13$fever == 1) &
                   (st13$sore.throat == 1 | st13$cough ==1 |
                    st13$shortness.breath == 1))
st13$ili.fever <- as.numeric(st13$ili.fever)


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


## rolling join of symptoms and background, by id number (first) and date
## (second)
dt13 <- bt13[ct13[st13, roll=TRUE], roll = TRUE]

##cleanup (some participants have only a weekly survey, no background one)
dt13 <- dt13[!is.na(global.id.number)]

saveRDS(dt13, "flusurvey_201213.rds")

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


## set convenient names
setnames(bt12, "Q0", "self")
setnames(bt12, "Q1", "gender")
setnames(bt12, "Q2", "birthmonth")
setnames(bt12, "Q3", "postcode")
setnames(bt12, "Q4", "main.activity")
setnames(bt12, "Q4b", "work.postcode.option")
setnames(bt12, "Q4b_0_open", "work.postcode")
setnames(bt12, "Q4c", "occupation")
setnames(bt12, "Q4d_0", "no.education")
setnames(bt12, "Q4d_1", "education.gcse")
setnames(bt12, "Q4d_2", "education.alevels")
setnames(bt12, "Q4d_3", "education.bsc")
setnames(bt12, "Q4d_4", "education.msc")
setnames(bt12, "Q4d_5", "education.stillin")
setnames(bt12, "Q5_0", "frequent.contact.children")
setnames(bt12, "Q5_1", "frequent.contact.elderly")
setnames(bt12, "Q5_2", "frequent.contact.patients")
setnames(bt12, "Q5_3", "frequent.contact.people")
setnames(bt12, "Q5_4", "frequent.contact.none")
setnames(bt12, "Q6_0", "household.0.4")
setnames(bt12, "Q6_0_open", "nb.household.0.4")
setnames(bt12, "Q6_1", "household.5.18")
setnames(bt12, "Q6_1_open", "nb.household.5.18")
setnames(bt12, "Q6_2", "household.19.44")
setnames(bt12, "Q6_2_open", "nb.household.19.44")
setnames(bt12, "Q6_3", "household.45.64")
setnames(bt12, "Q6_3_open", "nb.household.45.64")
setnames(bt12, "Q6_4", "household.65+")
setnames(bt12, "Q6_4_open", "nb.household.65+")
setnames(bt12, "Q6b", "children.school")
setnames(bt12, "Q7", "transport")
setnames(bt12, "Q7b", "howlong.transport")
setnames(bt12, "Q8", "howoften.flulike")
setnames(bt12, "Q9", "vaccine.last.year")
setnames(bt12, "Q10", "vaccine.this.year")
setnames(bt12, "Q10b", "date.vaccine.option")
setnames(bt12, "Q10b_1_open", "date.vaccine")
setnames(bt12, "Q10c_0", "why.vaccine.riskgroup")
setnames(bt12, "Q10c_1", "why.vaccine.protected")
setnames(bt12, "Q10c_2", "why.vaccine.protect.others")
setnames(bt12, "Q10c_3", "why.vaccine.doctor")
setnames(bt12, "Q10c_4", "why.vaccine.work.recommended")
setnames(bt12, "Q10c_5", "why.vaccine.convenient")
setnames(bt12, "Q10c_6", "why.vaccine.free")
setnames(bt12, "Q10c_7", "why.vaccine.nomiss.work")
setnames(bt12, "Q10c_8", "why.vaccine.always")
setnames(bt12, "Q10c_9", "why.vaccine.other")
setnames(bt12, "Q10d_0", "why.not.vaccine.notyet")
setnames(bt12, "Q10d_1", "why.not.vaccine.notoffered")
setnames(bt12, "Q10d_2", "why.not.vaccine.norisk")
setnames(bt12, "Q10d_3", "why.not.vaccine.natural")
setnames(bt12, "Q10d_4", "why.not.vaccine.noteffective")
setnames(bt12, "Q10d_5", "why.not.vaccine.minor")
setnames(bt12, "Q10d_6", "why.not.vaccine.unlikely")
setnames(bt12, "Q10d_7", "why.not.vaccine.cause")
setnames(bt12, "Q10d_8", "why.not.vaccine.side.effects")
setnames(bt12, "Q10d_9", "why.not.vaccine.dont.like")
setnames(bt12, "Q10d_10", "why.not.vaccine.unavailable")
setnames(bt12, "Q10d_11", "why.not.vaccine.not.free")
setnames(bt12, "Q10d_12", "why.not.vaccine.no.reason")
setnames(bt12, "Q10d_13", "why.not.vaccine.doctor")
setnames(bt12, "Q10d_14", "why.not.vaccine.other")
setnames(bt12, "Q11_0", "norisk")
setnames(bt12, "Q11_1", "risk.asthma")
setnames(bt12, "Q11_2", "risk.diabetes")
setnames(bt12, "Q11_3", "risk.lung")
setnames(bt12, "Q11_4", "risk.heart")
setnames(bt12, "Q11_5", "risk.kidney")
setnames(bt12, "Q11_6", "risk.immune")
setnames(bt12, "Q12", "pregnant")
setnames(bt12, "Q12b", "pregnant.trimester")
setnames(bt12, "Q13", "smoke")
setnames(bt12, "Q14_1", "allergy.hayfever")
setnames(bt12, "Q14_2", "allergy.dust")
setnames(bt12, "Q14_3", "allergy.animals")
setnames(bt12, "Q14_4", "allergy.other")
setnames(bt12, "Q14_5", "allergy.none")
setnames(bt12, "Q15_0", "diet.none")
setnames(bt12, "Q15_1", "diet.vegetarian")
setnames(bt12, "Q15_2", "diet.vegan")
setnames(bt12, "Q15_3", "diet.low-calorie")
setnames(bt12, "Q15_4", "diet.other")
setnames(bt12, "Q16_0", "pets.none")
setnames(bt12, "Q16_1", "pets.dogs")
setnames(bt12, "Q16_2", "pets.cats")
setnames(bt12, "Q16_3", "pets.birds")
setnames(bt12, "Q16_4", "pets.other")
setnames(bt12, "Q17_0", "howhear.radio.tv")
setnames(bt12, "Q17_1", "howhear.paper.magazine")
setnames(bt12, "Q17_2", "howhear.internet")
setnames(bt12, "Q17_3", "howhear.poster")
setnames(bt12, "Q17_4", "howhear.family.friends")
setnames(bt12, "Q17_5", "howhear.other")

setnames(st12, "Q1_0", "no.symptoms")
setnames(st12, "Q1_1", "fever")
setnames(st12, "Q1_2", "chills")
setnames(st12, "Q1_3", "blocked.runny.nose")
setnames(st12, "Q1_4", "sneezing")
setnames(st12, "Q1_5", "sore.throat")
setnames(st12, "Q1_6", "cough")
setnames(st12, "Q1_7", "shortness.breath")
setnames(st12, "Q1_8", "headache")
setnames(st12, "Q1_9", "muscle.and.or.joint.pain")
setnames(st12, "Q1_10", "chest.pain")
setnames(st12, "Q1_11", "tired")
setnames(st12, "Q1_12", "loss.appetite")
setnames(st12, "Q1_13", "phlegm")
setnames(st12, "Q1_14", "watery.eyes")
setnames(st12, "Q1_15", "nausea")
setnames(st12, "Q1_16", "vomiting")
setnames(st12, "Q1_17", "diarrhoea")
setnames(st12, "Q1_18", "stomach.ache")
setnames(st12, "Q1_19", "other.symptoms")
setnames(st12, "Q2", "same")
setnames(st12, "Q3", "symptoms.start.option")
setnames(st12, "Q3_0_open", "symptoms.start.date")
setnames(st12, "Q4", "symptoms.end.option")
setnames(st12, "Q4_0_open", "symptoms.end.date")
setnames(st12, "Q5", "symptoms.suddenly")
setnames(st12, "Q6", "fever.start.option")
setnames(st12, "Q6_1_open", "fever.start")
setnames(st12, "Q6b", "fever.suddenly")
setnames(st12, "Q6c", "fever.temperature")
setnames(st12, "Q6d", "fever.temperature.value")
setnames(st12, "Q7_0", "visit.medical.service.no")
setnames(st12, "Q7_1", "visit.medical.service.gp")
setnames(st12, "Q7_2", "visit.medical.service.ae")
setnames(st12, "Q7_3", "visit.medical.service.hospital")
setnames(st12, "Q7_4", "visit.medical.service.other")
setnames(st12, "Q7_5", "visit.medical.service.appointment")
setnames(st12, "Q7b", "visit.medical.service.howsoon")
setnames(st12, "Q8_0", "contact.medical.service.no")
setnames(st12, "Q8_1", "contact.medical.service.gp.receptionist")
setnames(st12, "Q8_2", "contact.medical.service.gp.doctor")
setnames(st12, "Q8_3", "contact.medical.service.nhs")
setnames(st12, "Q8_4", "contact.medical.service.npfs")
setnames(st12, "Q8_5", "contact.medical.service.other")
setnames(st12, "Q8b", "contact.medical.service.howsoon")
setnames(st12, "Q9_0", "no.medication")
setnames(st12, "Q9_1", "medication.painkillers")
setnames(st12, "Q9_2", "medication.cough")
setnames(st12, "Q9_3", "medication.antiviral")
setnames(st12, "Q9_4", "medication.antibiotic")
setnames(st12, "Q9_5", "medication.other")
setnames(st12, "Q9_6", "medication.dontknow")
setnames(st12, "Q9b", "medication.howsoon")
setnames(st12, "Q10", "alter.routine")
setnames(st12, "Q10b", "still.altered")
setnames(st12, "Q10c", "howlong.altered")
setnames(st12, "Q11", "what.do.you.think")
setnames(st12, "Q12_multi_row1_col1", "howmany.household.ili")
setnames(st12, "Q13_multi_row1_col1", "howmany.other.ili")

setnames(ct12, "Q1_multi_row1_col1", "conversational.home.0-4")
setnames(ct12, "Q1_multi_row1_col2", "conversational.home.5-18")
setnames(ct12, "Q1_multi_row1_col3", "conversational.home.19-44")
setnames(ct12, "Q1_multi_row1_col4", "conversational.home.45-64")
setnames(ct12, "Q1_multi_row1_col5", "conversational.home.65+")
setnames(ct12, "Q1_multi_row2_col1", "conversational.work.0-4")
setnames(ct12, "Q1_multi_row2_col2", "conversational.work.5-18")
setnames(ct12, "Q1_multi_row2_col3", "conversational.work.19-44")
setnames(ct12, "Q1_multi_row2_col4", "conversational.work.45-64")
setnames(ct12, "Q1_multi_row2_col5", "conversational.work.65+")
setnames(ct12, "Q1_multi_row3_col1", "conversational.other.0-4")
setnames(ct12, "Q1_multi_row3_col2", "conversational.other.5-18")
setnames(ct12, "Q1_multi_row3_col3", "conversational.other.19-44")
setnames(ct12, "Q1_multi_row3_col4", "conversational.other.45-64")
setnames(ct12, "Q1_multi_row3_col5", "conversational.other.65+")
setnames(ct12, "Q2_multi_row1_col1", "physical.home.0-4")
setnames(ct12, "Q2_multi_row1_col2", "physical.home.5-18")
setnames(ct12, "Q2_multi_row1_col3", "physical.home.19-44")
setnames(ct12, "Q2_multi_row1_col4", "physical.home.45-64")
setnames(ct12, "Q2_multi_row1_col5", "physical.home.65+")
setnames(ct12, "Q2_multi_row2_col1", "physical.work.0-4")
setnames(ct12, "Q2_multi_row2_col2", "physical.work.5-18")
setnames(ct12, "Q2_multi_row2_col3", "physical.work.19-44")
setnames(ct12, "Q2_multi_row2_col4", "physical.work.45-64")
setnames(ct12, "Q2_multi_row2_col5", "physical.work.65+")
setnames(ct12, "Q2_multi_row3_col1", "physical.other.0-4")
setnames(ct12, "Q2_multi_row3_col2", "physical.other.5-18")
setnames(ct12, "Q2_multi_row3_col3", "physical.other.19-44")
setnames(ct12, "Q2_multi_row3_col4", "physical.other.45-64")
setnames(ct12, "Q2_multi_row3_col5", "physical.other.65+")
setnames(ct12, "Q3", "public.transport")
setnames(ct12, "Q4", "enclosed.indoor.space")
setnames(ct12, "Q5", "furthest.travelled")

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

st12$ili <- ((st12$symptoms.suddenly == 0) &
             (st12$fever == 1 | st12$tired == 1 | st12$headache == 1 |
              st12$muscle.and.or.joint.pain == 1) &
             (st12$sore.throat == 1 | st12$cough == 1 |
              st12$shortness.breath ==1))
st12$ili <- as.numeric(st12$ili)

st12$ili.notired <- ((st12$symptoms.suddenly == 0) &
                     (st12$fever == 1 | st12$headache == 1 |
                      st12$muscle.and.or.joint.pain == 1) &
                     (st12$sore.throat == 1 | st12$cough == 1 |
                      st12$shortness.breath ==1))
st12$ili.notired <- as.numeric(st12$ili.notired)

st12$ili.fever <- ((st12$symptoms.suddenly == 0) &
                   (st12$fever == 1) &
                   (st12$sore.throat == 1 | st12$cough == 1 |
                    st12$shortness.breath == 1))
st12$ili.fever <- as.numeric(st12$ili.fever)


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

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt12 <- bt12[ct12[st12, roll=TRUE], roll = TRUE]

dt12 <- dt12[!is.na(global.id.number)]

saveRDS(dt12, "flusurvey_201112.rds")

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

setnames(bt11, "IntakeQ1", "gender")
setnames(bt11, "IntakeQ2", "birthmonth")
setnames(bt11, "IntakeQ3", "postcode")
setnames(bt11, "IntakeQ4", "work.postcode")
setnames(bt11, "IntakeQ5", "frequent.contact")
setnames(bt11, "IntakeQ5.0", "frequent.contact.children")
setnames(bt11, "IntakeQ5.1", "frequent.contact.elderly")
setnames(bt11, "IntakeQ5.2", "frequent.contact.patients")
setnames(bt11, "IntakeQ5.3", "frequent.contact.people")
setnames(bt11, "IntakeQ6", "household")
setnames(bt11, "IntakeQ6.0", "nb.household.0.4")
setnames(bt11, "IntakeQ6.1", "nb.household.5.18")
setnames(bt11, "IntakeQ6.2", "nb.household.19.44")
setnames(bt11, "IntakeQ6.3", "nb.household.45.64")
setnames(bt11, "IntakeQ6.4", "nb.household.65+")
setnames(bt11, "IntakeQ6b", "children.school")
setnames(bt11, "IntakeQ7", "transport")
setnames(bt11, "IntakeQ7b", "howlong.transport")
setnames(bt11, "IntakeQ8", "vaccine.swineflu")
setnames(bt11, "IntakeQ8b", "date.vaccine.swineflu")
setnames(bt11, "IntakeQ9", "vaccine.last.year")
setnames(bt11, "IntakeQ10", "vaccine.this.year")
setnames(bt11, "IntakeQ10b", "date.vaccine")
setnames(bt11, "IntakeQ10c", "why.vaccine")
setnames(bt11, "IntakeQ10c.0", "why.vaccine.riskgroup")
setnames(bt11, "IntakeQ10c.1", "why.vaccine.protected")
setnames(bt11, "IntakeQ10c.2", "why.vaccine.protect.others")
setnames(bt11, "IntakeQ10c.3", "why.vaccine.doctor")
setnames(bt11, "IntakeQ10c.4", "why.vaccine.work.recommended")
setnames(bt11, "IntakeQ10c.5", "why.vaccine.convenient")
setnames(bt11, "IntakeQ10c.6", "why.vaccine.free")
setnames(bt11, "IntakeQ10c.7", "why.vaccine.nomiss.work")
setnames(bt11, "IntakeQ10c.8", "why.vaccine.always")
setnames(bt11, "IntakeQ10c.9", "why.vaccine.other")
setnames(bt11, "IntakeQ10d", "why.not.vaccine")
setnames(bt11, "IntakeQ10d.0", "why.not.vaccine.notyet")
setnames(bt11, "IntakeQ10d.1", "why.not.vaccine.norisk")
setnames(bt11, "IntakeQ10d.2", "why.not.vaccine.natural")
setnames(bt11, "IntakeQ10d.3", "why.not.vaccine.noteffective")
setnames(bt11, "IntakeQ10d.4", "why.not.vaccine.minor")
setnames(bt11, "IntakeQ10d.5", "why.not.vaccine.cause")
setnames(bt11, "IntakeQ10d.6", "why.not.vaccine.side.effects")
setnames(bt11, "IntakeQ10d.7", "why.not.vaccine.unavailable")
setnames(bt11, "IntakeQ10d.8", "why.not.vaccine.not.free")
setnames(bt11, "IntakeQ10d.9", "why.not.vaccine.dislike.injections")
setnames(bt11, "IntakeQ10d.10", "why.not.vaccine.no.reason")
setnames(bt11, "IntakeQ10d.11", "why.not.vaccine.doctor")
setnames(bt11, "IntakeQ10d.12", "why.not.vaccine.notoffered")
setnames(bt11, "IntakeQ12", "risk")
setnames(bt11, "IntakeQ12.0", "risk.asthma")
setnames(bt11, "IntakeQ12.1", "risk.diabetes")
setnames(bt11, "IntakeQ12.2", "risk.lung")
setnames(bt11, "IntakeQ12.3", "risk.heart")
setnames(bt11, "IntakeQ12.4", "risk.kidney")
setnames(bt11, "IntakeQ12.5", "risk.immune")
setnames(bt11, "IntakeQ12.6", "norisk")
setnames(bt11, "IntakeQ13", "pregnant")
setnames(bt11, "IntakeQ13b", "trimester")
setnames(bt11, "IntakeQ14", "smoke")
setnames(bt11, "IntakeQ15", "allergy")
setnames(bt11, "IntakeQ15.0", "allergy.hayfever")
setnames(bt11, "IntakeQ15.1", "allergy.dust")
setnames(bt11, "IntakeQ15.2", "allergy.animals")
setnames(bt11, "IntakeQ15.3", "allergy.other")
setnames(st11, "WeeklyQ1.0", "no.symptoms")
setnames(st11, "WeeklyQ1.1", "fever")
setnames(st11, "WeeklyQ1.2", "watery.eyes")
setnames(st11, "WeeklyQ1.3", "blocked.runny.nose")
setnames(st11, "WeeklyQ1.4", "sneezing")
setnames(st11, "WeeklyQ1.5", "sore.throat")
setnames(st11, "WeeklyQ1.6", "cough")
setnames(st11, "WeeklyQ1.7", "phlegm")
setnames(st11, "WeeklyQ1.8", "headache")
setnames(st11, "WeeklyQ1.9", "muscle.and.or.joint.pain")
setnames(st11, "WeeklyQ1.10", "chest.pain")
setnames(st11, "WeeklyQ1.11", "tired")
setnames(st11, "WeeklyQ1.12", "loss.appetite")
setnames(st11, "WeeklyQ1.13", "nausea")
setnames(st11, "WeeklyQ1.14", "vomiting")
setnames(st11, "WeeklyQ1.15", "diarrhoea")
setnames(st11, "WeeklyQ1.16", "other.symptoms")
setnames(st11, "WeeklyQ1.17", "chills")
setnames(st11, "WeeklyQ1.18", "shortness.breath")
setnames(st11, "WeeklyQ1.19", "stomach.ache")
setnames(st11, "WeeklyQ1b", "symptoms.suddenly")
setnames(st11, "WeeklyQ2", "fever.temperature")
setnames(st11, "WeeklyQ2b", "fever.temperature.value")
setnames(st11, "WeeklyQ2c", "fever.start")
setnames(st11, "WeeklyQ3", "same")
setnames(st11, "WeeklyQ4", "symptoms.start.date")
setnames(st11, "WeeklyQ5", "symptoms.end.date")
setnames(st11, "WeeklyQ6.0", "visit.medical.service.gp")
setnames(st11, "WeeklyQ6.1", "visit.medical.service.hospital")
setnames(st11, "WeeklyQ6.2", "visit.medical.service.ae")
setnames(st11, "WeeklyQ6.3", "visit.medical.service.other")
setnames(st11, "WeeklyQ6.4", "visit.medical.service.no")
setnames(st11, "WeeklyQ6.5", "visit.medical.service.appointment")
setnames(st11, "WeeklyQ6b", "visit.medical.service.howsoon")
setnames(st11, "WeeklyQ6c.0", "contact.medical.service.gp")
setnames(st11, "WeeklyQ6c.1", "contact.medical.service.hospital")
setnames(st11, "WeeklyQ6c.2", "contact.medical.service.ae")
setnames(st11, "WeeklyQ6c.3", "contact.medical.service.other")
setnames(st11, "WeeklyQ6c.4", "contact.medical.service.no")
setnames(st11, "WeeklyQ6c.5", "contact.medical.service.appointment")
setnames(st11, "WeeklyQ6d", "contact.medical.service.howsoon")
setnames(st11, "WeeklyQ7.0", "medication.none")
setnames(st11, "WeeklyQ7.1", "medication.painkillers")
setnames(st11, "WeeklyQ7.2", "medication.cough")
setnames(st11, "WeeklyQ7.3", "medication.antivirals")
setnames(st11, "WeeklyQ7.4", "medication.antibiotics")
setnames(st11, "WeeklyQ7.5", "medication.other")
setnames(st11, "WeeklyQ7b", "medication.howlong")
setnames(st11, "WeeklyQ8", "alter.routine")
setnames(st11, "WeeklyQ8b", "still.altered")
setnames(st11, "WeeklyQ8c", "howlong.altered")
setnames(st11, "WeeklyQ9a", "howmany.household.ili")
setnames(st11, "WeeklyQ9b", "howmany.other.ili")
setnames(st11, "WeeklyQ10", "vaccine.this.year.since.registration")
setnames(st11, "WeeklyQ11", "what.do.you.think")
setnames(st11, "ContactQ1", "symptom.conversational")
setnames(st11, "ContactQ2", "symptom.physical")
setnames(st11, "ContactQ3", "symptom.public.transport")
setnames(st11, "ContactQ4", "symptom.indoor.space")

setnames(ct11, "ContactQ1.0", "conversational.home.0-4")
setnames(ct11, "ContactQ1.1", "conversational.home.5-18")
setnames(ct11, "ContactQ1.2", "conversational.home.19-44")
setnames(ct11, "ContactQ1.3", "conversational.home.45-64")
setnames(ct11, "ContactQ1.4", "conversational.home.65+")
setnames(ct11, "ContactQ1.5", "conversational.work.0-4")
setnames(ct11, "ContactQ1.6", "conversational.work.5-18")
setnames(ct11, "ContactQ1.7", "conversational.work.19-44")
setnames(ct11, "ContactQ1.8", "conversational.work.45-64")
setnames(ct11, "ContactQ1.9", "conversational.work.65+")
setnames(ct11, "ContactQ1.10", "conversational.other.0-4")
setnames(ct11, "ContactQ1.11", "conversational.other.5-18")
setnames(ct11, "ContactQ1.12", "conversational.other.19-44")
setnames(ct11, "ContactQ1.13", "conversational.other.45-64")
setnames(ct11, "ContactQ1.14", "conversational.other.65+")
setnames(ct11, "ContactQ2.0", "physical.home.0-4")
setnames(ct11, "ContactQ2.1", "physical.home.5-18")
setnames(ct11, "ContactQ2.2", "physical.home.19-44")
setnames(ct11, "ContactQ2.3", "physical.home.45-64")
setnames(ct11, "ContactQ2.4", "physical.home.65+")
setnames(ct11, "ContactQ2.5", "physical.work.0-4")
setnames(ct11, "ContactQ2.6", "physical.work.5-18")
setnames(ct11, "ContactQ2.7", "physical.work.19-44")
setnames(ct11, "ContactQ2.8", "physical.work.45-64")
setnames(ct11, "ContactQ2.9", "physical.work.65+")
setnames(ct11, "ContactQ2.10", "physical.other.0-4")
setnames(ct11, "ContactQ2.11", "physical.other.5-18")
setnames(ct11, "ContactQ2.12", "physical.other.19-44")
setnames(ct11, "ContactQ2.13", "physical.other.45-64")
setnames(ct11, "ContactQ2.14", "physical.other.65+")

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

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt11 <- bt11[ct11[st11, roll=TRUE], roll = TRUE]

dt11 <- dt11[!is.na(global.id.number)]

saveRDS(dt11, "flusurvey_201011.rds")

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

setnames(bt10, "q1000", "postcode")
setnames(bt10, "q1001", "gender")
setnames(bt10, "q1002", "birthyear")
setnames(bt10, "q1005", "work.postcode")
setnames(bt10, "q2000", "where.spend.time")
setnames(bt10, "q2000_1", "where.spend.time.school")
setnames(bt10, "q2000_2", "where.spend.time.work")
setnames(bt10, "q2000_3", "where.spend.time.home")
setnames(bt10, "q2000_4", "where.spend.time.other")
setnames(bt10, "q2001", "transport")
setnames(bt10, "q2001_1", "transport.bike.motor")
setnames(bt10, "q2001_2", "transport.car")
setnames(bt10, "q2001_3", "transport.walk")
setnames(bt10, "q2001_4", "transport.public")
setnames(bt10, "q2002", "howmany.colds")
setnames(bt10, "q2040", "vaccine.last.year")
setnames(bt10, "q2004", "riskgroup")
setnames(bt10, "q2005", "smoke")
setnames(bt10, "q2008", "exercise")
setnames(bt10, "q2009001", "nb.household")
setnames(bt10, "q2009002", "nb.household.0.4")
setnames(bt10, "q2009003", "nb.household.5.18")
setnames(bt10, "q2009004", "nb.household.19.64")
setnames(bt10, "q2009005", "nb.household.65+")
setnames(bt10, "q2010", "school.nursery")
setnames(bt10, "q2011", "frequent.contact")
setnames(bt10, "q2004_1", "chronic.heart.disease")
setnames(bt10, "q2004_2", "diabetes")
setnames(bt10, "q2004_3", "asthma")
setnames(bt10, "q2004_4", "other.chronic.lung.disease")
setnames(bt10, "q2004_5", "pregnant")
setnames(bt10, "q2004_6", "immunocompromised")
setnames(bt10, "q2004_7", "other.chronic")
setnames(bt10, "q2011_1", "frequent.contact.children")
setnames(bt10, "q2011_2", "frequent.contact.patients")
setnames(bt10, "q2011_3", "frequent.contact.elderly")
setnames(bt10, "q2011_4", "frequent.contact.people")
setnames(bt10, "q2041", "why.vaccine")
setnames(bt10, "q2041_1", "why.vaccine.doctor")
setnames(bt10, "q2041_2", "why.vaccine.protected")
setnames(bt10, "q2041_3", "why.vaccine.protect.others")
setnames(bt10, "q2041_4", "why.vaccine.other")
setnames(bt10, "q2042", "why.not.vaccine")
setnames(bt10, "q2042_1", "why.not.vaccine.doctor")
setnames(bt10, "q2042_2", "why.not.vaccine.noteffective")
setnames(bt10, "q2042_3", "why.not.vaccine.gives.flu")
setnames(bt10, "q2042_4", "why.not.vaccine.side.effects")
setnames(bt10, "q2042_5", "why.not.vaccine.other")
setnames(bt10, "q2042_6", "why.not.vaccine.norisk")
setnames(bt10, "q2060", "how.find.out")
setnames(bt10, "q2060_1", "how.find.out.TV")
setnames(bt10, "q2060_2", "how.find.out.radio")
setnames(bt10, "q2060_3", "how.find.out.newspaper.magazine")
setnames(bt10, "q2060_4", "how.find.out.internet")
setnames(bt10, "q2060_5", "how.find.out.meeting")
setnames(bt10, "q2060_6", "how.find.out.friend")
setnames(bt10, "q2060_7", "how.find.out.survey.team")
setnames(bt10, "q2060_8", "how.find.out.other")
setnames(vt10, "q9001", "offered.swineflu.vaccine")
setnames(vt10, "q9002", "why.offered.swineflu.vaccine")
setnames(vt10, "q9003", "swineflu.vaccine.this.year")
setnames(vt10, "q9004", "intend.swineflue.vaccine")
setnames(vt10, "q9005", "date.swineflu.vaccine")
setnames(vt10, "q9006", "why.not.swineflu.vaccine")
setnames(vt10, "q9007", "why.swineflu.vaccine")
setnames(vt10, "q9008", "offered.seasonal.vaccine")
setnames(vt10, "q9009", "why.offered.seasonal.vaccine")
setnames(vt10, "q9010", "vaccine.this.year")
setnames(vt10, "q9011", "intend.seasonal.vaccine")
setnames(vt10, "q9012", "date.vaccine")
setnames(vt10, "q9013", "why.not.seasonal.vaccine")
setnames(vt10, "q9014", "why.seasonal.vaccine")
setnames(st10, "q3000", "symptoms")
setnames(st10, "q3001", "symptoms.start")
setnames(st10, "q3002", "fever")
setnames(st10, "q3003", "fever.start")
setnames(st10, "q3004", "fever.suddenly")
setnames(st10, "q3005", "medical.service.phone")
setnames(st10, "q30051", "medical.service.visit")
setnames(st10, "q30052", "nights.hospital")
setnames(st10, "q3006", "diagnosis")
setnames(st10, "q3007", "alter.routine")
setnames(st10, "q3008", "howlong.altered")
setnames(st10, "q3009", "medication")
setnames(st10, "q3010", "medication.when")
setnames(st10, "q3011", "encountered.flu")
setnames(st10, "q3000_1", "blocked.runny.nose")
setnames(st10, "q3000_2", "cough")
setnames(st10, "q3000_3", "sore.throat")
setnames(st10, "q3000_4", "headache")
setnames(st10, "q3000_5", "muscle.and.or.joint.pain")
setnames(st10, "q3000_6", "chest.pain")
setnames(st10, "q3000_7", "stomach.ache")
setnames(st10, "q3000_8", "diarrhoea")
setnames(st10, "q3000_9", "nausea")
setnames(st10, "q3000_10", "chills")
setnames(st10, "q3000_11", "weakness")
setnames(st10, "q3000_12", "eye.irritation")
setnames(st10, "q3000_13", "fever.symptom")
setnames(st10, "q3000_14", "no.symptoms")
setnames(st10, "q3005_1", "medical.service.phone.gp")
setnames(st10, "q3005_2", "medical.service.phone.hospital")
setnames(st10, "q3005_3", "medical.service.phone.other")
setnames(st10, "q3005_4", "medical.service.phone.no")
setnames(st10, "q3005_5", "medical.service.phone.ae")
setnames(st10, "q30051_1", "medical.service.visit.gp")
setnames(st10, "q30051_2", "medical.service.visit.hospital")
setnames(st10, "q30051_3", "medical.service.visit.other")
setnames(st10, "q30051_4", "medical.service.visit.no")
setnames(st10, "q30051_5", "medical.service.visit.ae")
setnames(st10, "q300501", "medical.service.phone.howlong")
setnames(st10, "q300502", "medical.service.visit.howlong")
setnames(st10, "q30081", "still.altered")
setnames(st10, "q3009_2", "medication.painkillers")
setnames(st10, "q3009_3", "medication.cough")
setnames(st10, "q3009_4", "medication.tamiflu")
setnames(st10, "q3009_5", "medication.relenza")
setnames(st10, "q3009_6", "medication.none")
setnames(st10, "q3011_1", "encountered.flu.yes")
setnames(st10, "q3011_2", "encountered.flu.no")
setnames(st10, "q3011_3", "encountered.flu.dontknow")
setnames(ct10, "q4000", "conversational.home.0-4")
setnames(ct10, "q4001", "conversational.home.5-18")
setnames(ct10, "q4002", "conversational.home.19-64")
setnames(ct10, "q4003", "conversational.home.65+")
setnames(ct10, "q4004", "conversational.work.0-4")
setnames(ct10, "q4005", "conversational.work.5-18")
setnames(ct10, "q4006", "conversational.work.19-64")
setnames(ct10, "q4007", "conversational.work.65+")
setnames(ct10, "q4008", "conversational.other.0-4")
setnames(ct10, "q4009", "conversational.other.5-18")
setnames(ct10, "q4010", "conversational.other.19-64")
setnames(ct10, "q4011", "conversational.other.65+")
setnames(ct10, "q4012", "physical.home.0-4")
setnames(ct10, "q4013", "physical.home.5-18")
setnames(ct10, "q4014", "physical.home.19-64")
setnames(ct10, "q4015", "physical.home.65+")
setnames(ct10, "q4016", "physical.work.0-4")
setnames(ct10, "q4017", "physical.work.5-18")
setnames(ct10, "q4018", "physical.work.19-64")
setnames(ct10, "q4019", "physical.work.65+")
setnames(ct10, "q4020", "physical.other.0-4")
setnames(ct10, "q4021", "physical.other.5-18")
setnames(ct10, "q4022", "physical.other.19-64")
setnames(ct10, "q4023", "physical.other.65+")
setnames(ct10, "q4024", "public.transport")
setnames(ct10, "q4025", "enclosed.indoor.space")

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

## rolling join of symptoms and background, by id number (first) and date
## (second)
dt10 <- bt10[vt10[ct10[st10, roll=TRUE], roll = TRUE], roll = TRUE]
dt10 <- dt10[!is.na(global.id.number)]

saveRDS(dt10, "flusurvey_200910.rds")

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
