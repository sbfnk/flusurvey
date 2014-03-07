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

## 2013

sf13 <- read.csv('weekly_13.csv', sep=',', header=T)
bf13 <- read.csv('intake_13.csv', sep=',', header=T)
cf13 <- read.csv('contact_13.csv', sep=',', header=T)

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf13$global_id))
translation$number <- seq(1,nrow(translation))

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
setnames(st13, "Q1_19", "other")
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
setnames(st12, "Q10b", "still.altered")
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

ct13[,"conversational.home" := get("conversational.home.0-4") +
     get("conversational.home.5-18") + get("conversational.home.19-44") +
     get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct13[,"conversational.work" := get("conversational.work.0-4") +
     get("conversational.work.5-18") + get("conversational.work.19-44") +
     get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct13[,"conversational.other" := get("conversational.other.0-4") +
     get("conversational.other.5-18") + get("conversational.other.19-44") +
     get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct13[,"conversational.0-4" := get("conversational.home.0-4") +
     get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct13[,"conversational.5-18" := get("conversational.home.5-18") +
     get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct13[,"conversational.19-44" := get("conversational.home.19-44") +
     get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct13[,"conversational.45-64" := get("conversational.home.45-64") +
     get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct13[,"conversational.65+" := get("conversational.home.65+") +
     get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct13[,"conversational" := get("conversational.home") +
     get("conversational.work") + get("conversational.other"), with=F]
ct13[,"physical.home" := get("physical.home.0-4") +
     get("physical.home.5-18") + get("physical.home.19-44") +
     get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct13[,"physical.work" := get("physical.work.0-4") +
     get("physical.work.5-18") + get("physical.work.19-44") +
     get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct13[,"physical.other" := get("physical.other.0-4") +
     get("physical.other.5-18") + get("physical.other.19-44") +
     get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct13[,"physical.0-4" := get("physical.home.0-4") +
     get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct13[,"physical.5-18" := get("physical.home.5-18") +
     get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct13[,"physical.19-44" := get("physical.home.19-44") +
     get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct13[,"physical.45-64" := get("physical.home.45-64") +
     get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct13[,"physical.65+" := get("physical.home.65+") +
     get("physical.work.65+") + get("physical.other.65+"), with=F]
ct13[,"physical" := get("physical.home") +
     get("physical.work") + get("physical.other"), with=F]

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
st13$ili <- ((st13$symptoms.suddenly == 0) &
             (st13$fever == "t" | st13$tired == "t" | st13$headache == "t" |
              st13$muscle.and.or.joint.pain =="t") &
             (st13$sore.throat == "t" | st13$cough =="t" | st13$shortness.breath
              =="t"))
st13$ili <- as.numeric(st13$ili)

st13$ili.notired <- ((st13$symptoms.suddenly == 0) &
                     (st13$fever == "t" | st13$headache == "t" |
                      st13$muscle.and.or.joint.pain =="t") &
                     (st13$sore.throat == "t" | st13$cough =="t" | st13$shortness.breath
                      =="t"))
st13$ili.notired <- as.numeric(st13$ili.notired)

st13$ili.fever <- ((st13$symptoms.suddenly == 0) &
                   (st13$fever == "t") &
                   (st13$sore.throat == "t" | st13$cough =="t" | st13$shortness.breath
                    =="t"))
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
st13$weekweight <- 1/table(st13$week)[st13$week]

st13$symptoms.start.date <- as.Date(st13$symptoms.start.date, "%Y-%m-%d")
st13$symptoms.end.date <- as.Date(st13$symptoms.end.date, "%Y-%m-%d")
st13$symptoms.start.week <- format(st13$symptoms.start.date, format="%G-%W")
st13[st13$symptoms.start.week=="2013-00"]$symptoms.start.week <- "2012-53"
st13[st13$symptoms.start.week=="2013-53"]$symptoms.start.week <- "2012-53"

## more variables to be used later
bt13[, country := "uk"]

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
bt13$vaccine.date <- as.Date(bt13$date.vaccine, "%Y/%m/%d")
## bt13$vaccine <- as.numeric(bt13$vaccine.this.year==0 & (is.na(bt13$vaccine.date) |
##                            bt13$vaccine.date <= bt13$date))
bt13$children <- as.numeric((bt13$household.0.4 == "t" | bt13
                             $household.5.18 == "t"))

st13$ili.self <- (st13$what.do.you.think == 0)
st13[is.na(ili.self)]$ili.self <- FALSE

bt13$using.transport <- (bt13$transport > 0)

uk.ur <- read.csv("urban_rural.csv", header=F, sep=",")

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

rm(bt13)
rm(st13)
rm(ct13)

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
translation$number <- seq(1,nrow(translation))

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
setnames(st12, "Q1_19", "other")
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

ct12[,"conversational.home" := get("conversational.home.0-4") +
     get("conversational.home.5-18") + get("conversational.home.19-44") +
     get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct12[,"conversational.work" := get("conversational.work.0-4") +
     get("conversational.work.5-18") + get("conversational.work.19-44") +
     get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct12[,"conversational.other" := get("conversational.other.0-4") +
     get("conversational.other.5-18") + get("conversational.other.19-44") +
     get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct12[,"conversational.0-4" := get("conversational.home.0-4") +
     get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct12[,"conversational.5-18" := get("conversational.home.5-18") +
     get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct12[,"conversational.19-44" := get("conversational.home.19-44") +
     get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct12[,"conversational.45-64" := get("conversational.home.45-64") +
     get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct12[,"conversational.65+" := get("conversational.home.65+") +
     get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct12[,"conversational" := get("conversational.home") +
     get("conversational.work") + get("conversational.other"), with=F]
ct12[,"physical.home" := get("physical.home.0-4") +
     get("physical.home.5-18") + get("physical.home.19-44") +
     get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct12[,"physical.work" := get("physical.work.0-4") +
     get("physical.work.5-18") + get("physical.work.19-44") +
     get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct12[,"physical.other" := get("physical.other.0-4") +
     get("physical.other.5-18") + get("physical.other.19-44") +
     get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct12[,"physical.0-4" := get("physical.home.0-4") +
     get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct12[,"physical.5-18" := get("physical.home.5-18") +
     get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct12[,"physical.19-44" := get("physical.home.19-44") +
     get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct12[,"physical.45-64" := get("physical.home.45-64") +
     get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct12[,"physical.65+" := get("physical.home.65+") +
     get("physical.work.65+") + get("physical.other.65+"), with=F]
ct12[,"physical" := get("physical.home") +
     get("physical.work") + get("physical.other"), with=F]

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
st12$ili <- ((st12$symptoms.suddenly == 0) &
             (st12$fever == "t" | st12$tired == "t" | st12$headache == "t" |
              st12$muscle.and.or.joint.pain =="t") &
             (st12$sore.throat == "t" | st12$cough =="t" | st12$shortness.breath
              =="t"))
st12$ili <- as.numeric(st12$ili)

st12$ili.notired <- ((st12$symptoms.suddenly == 0) &
                     (st12$fever == "t" | st12$headache == "t" |
                      st12$muscle.and.or.joint.pain =="t") &
                     (st12$sore.throat == "t" | st12$cough =="t" | st12$shortness.breath
                      =="t"))
st12$ili.notired <- as.numeric(st12$ili.notired)

st12$ili.fever <- ((st12$symptoms.suddenly == 0) &
                   (st12$fever == "t") &
                   (st12$sore.throat == "t" | st12$cough =="t" | st12$shortness.breath
                    =="t"))
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
bt12[, country := "uk"]

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

uk.ur <- read.csv("urban_rural.csv", header=F, sep=",")

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

rm(bt12)
rm(st12)
rm(ct12)

dt12 <- dt12[!is.na(global.id.number)]

saveRDS(dt12, "flusurvey_201112.rds")

## 2011

sf11 <- read.csv('201011/weekly_201011.csv', header=T, sep=',');
bf11 <- read.csv('201011/intake_201011.csv', header=T, sep=',');
cf11 <- read.csv('201011/contactdata_2011-04-06.csv', header=T, sep=',');

## create translation table so that every participant gets a unique ID number
## (called global.id.number)
translation <- data.frame(global_id = unique(bf11$user_id))
translation$number <- seq(1,nrow(translation))

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
ct11$date <- as.Date(ct11$date, format = "%d/%m/%Y")

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
setnames(st11, "WeeklyQ1.16", "other")
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

ct11[,"conversational.home" := get("conversational.home.0-4") +
     get("conversational.home.5-18") + get("conversational.home.19-44") +
     get("conversational.home.45-64") + get("conversational.home.65+"), with=F]
ct11[,"conversational.work" := get("conversational.work.0-4") +
     get("conversational.work.5-18") + get("conversational.work.19-44") +
     get("conversational.work.45-64") + get("conversational.work.65+"), with=F]
ct11[,"conversational.other" := get("conversational.other.0-4") +
     get("conversational.other.5-18") + get("conversational.other.19-44") +
     get("conversational.other.45-64") + get("conversational.other.65+"), with=F]
ct11[,"conversational.0-4" := get("conversational.home.0-4") +
     get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
ct11[,"conversational.5-18" := get("conversational.home.5-18") +
     get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
ct11[,"conversational.19-44" := get("conversational.home.19-44") +
     get("conversational.work.19-44") + get("conversational.other.19-44"), with=F]
ct11[,"conversational.45-64" := get("conversational.home.45-64") +
     get("conversational.work.45-64") + get("conversational.other.45-64"), with=F]
ct11[,"conversational.65+" := get("conversational.home.65+") +
     get("conversational.work.65+") + get("conversational.other.65+"), with=F]
ct11[,"conversational" := get("conversational.home") +
     get("conversational.work") + get("conversational.other"), with=F]
ct11[,"physical.home" := get("physical.home.0-4") +
     get("physical.home.5-18") + get("physical.home.19-44") +
     get("physical.home.45-64") + get("physical.home.65+"), with=F]
ct11[,"physical.work" := get("physical.work.0-4") +
     get("physical.work.5-18") + get("physical.work.19-44") +
     get("physical.work.45-64") + get("physical.work.65+"), with=F]
ct11[,"physical.other" := get("physical.other.0-4") +
     get("physical.other.5-18") + get("physical.other.19-44") +
     get("physical.other.45-64") + get("physical.other.65+"), with=F]
ct11[,"physical.0-4" := get("physical.home.0-4") +
     get("physical.work.0-4") + get("physical.other.0-4"), with=F]
ct11[,"physical.5-18" := get("physical.home.5-18") +
     get("physical.work.5-18") + get("physical.other.5-18"), with=F]
ct11[,"physical.19-44" := get("physical.home.19-44") +
     get("physical.work.19-44") + get("physical.other.19-44"), with=F]
ct11[,"physical.45-64" := get("physical.home.45-64") +
     get("physical.work.45-64") + get("physical.other.45-64"), with=F]
ct11[,"physical.65+" := get("physical.home.65+") +
     get("physical.work.65+") + get("physical.other.65+"), with=F]
ct11[,"physical" := get("physical.home") +
     get("physical.work") + get("physical.other"), with=F]

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
bt11[, country := "uk"]

bt11$birthdate <- as.Date(paste(bt11$birthmonth, "-01",sep=""))

bt11$atrisk <- 1 - bt11$norisk
bt11$age <-  0
bt11$age <- apply(bt11, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
bt11$agegroup <- cut(bt11$age, breaks=c(0,18,45,65, max(bt11$age, na.rm=T)),
                     include.lowest=T, right=F)
bt11$children <- as.numeric((bt11$household.0.4 == "t" | bt11
                             $household.5.18 == "t"))

bt11$using.transport <- (bt11$transport > 0)

uk.ur <- read.csv("urban_rural.csv", header=F, sep=",")

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

rm(bt11)
rm(st11)
rm(ct11)

dt11 <- dt11[!is.na(global.id.number)]

saveRDS(dt11, "flusurvey_201011.rds")

## 2010

## read tables
sf <- read.csv('200910/symptoms_200910.csv', header=T, sep=';');
bf <- read.csv('200910/background_200910.csv', header=T, sep=';');
cf <- read.csv('200910/contacts_200910.csv', header=T, sep=';');
vf <- read.csv('200910/vaccine_200910.csv', header=T, sep=';');

## assign global id numbers
bf$global.id.number <- bf$uid
sf$global.id.number <- sf$uid
cf$global.id.number <- cf$uid
vf$global.id.number <- vf$uid

## put data in data tables (for the rolling join to be used later)
st <- data.table(sf)
bt <- data.table(bf)
ct <- data.table(cf)
vt <- data.table(vf)

rm(sf)
rm(bf)
rm(cf)
rm(vf)

st <- st[!is.na(uid)]

st$date <- as.Date(st$date)
bt$date <- as.Date(bt$date)
ct$date <- as.Date(ct$date)
vt$date <- as.Date(vt$date)

st <- st[!is.na(date)]

setnames(bt, 1, "bid")
setnames(ct, 1, "cid")
setnames(vt, 1, "vid")

st <- st[-(1:6),]

## rolling join of symptoms and background, by id number (first) and date
## (second)
setkey(st, global.id.number, date)
setkey(bt, global.id.number, date)
setkey(ct, global.id.number, date)
setkey(vt, global.id.number, date)
### focus on symptoms
##dt <- bt[vt[ct[st, roll=TRUE], roll=TRUE], roll=TRUE]
### focus on contact
dt09 <- bt[vt[st[ct, roll=TRUE], roll=TRUE], roll=TRUE]

rm(bt)
rm(vt)
rm(ct)
rm(st)

setnames(dt09, "q1000", "postcode")
setnames(dt09, "q1001", "gender")
setnames(dt09, "q1002", "birthyear")
setnames(dt09, "q2001", "transport")
setnames(dt09, "q2040", "vaccine.last.year")
setnames(dt09, "q2004", "riskgroup")
setnames(dt09, "q2005", "smoke")
setnames(dt09, "q2008", "exercise")
setnames(dt09, "q2009001", "nb.household")
setnames(dt09, "q2009002", "nb.household.0.4")
setnames(dt09, "q2009003", "nb.household.5.18")
setnames(dt09, "q2009004", "nb.household.19.64")
setnames(dt09, "q2009005", "nb.household.65+")
setnames(dt09, "q2010", "school.nursery")
setnames(dt09, "q2011", "frequent.contact")
setnames(dt09, "q2004_1", "chronic.heart.disease")
setnames(dt09, "q2004_2", "diabetes")
setnames(dt09, "q2004_3", "asthma")
setnames(dt09, "q2004_4", "other.chronic.lung.disease")
setnames(dt09, "q2004_5", "pregnant")
setnames(dt09, "q2004_6", "immunocompromised")
setnames(dt09, "q2004_7", "other.chronic")
setnames(dt09, "q2011_1", "frequent.contact.children")
setnames(dt09, "q2011_2", "frequent.contact.patients")
setnames(dt09, "q2011_3", "frequent.contact.elderly")
setnames(dt09, "q2011_4", "frequent.contact.people")
setnames(dt09, "q9001", "offered.swineflu.vaccine")
setnames(dt09, "q9002", "why.offered.swineflu.vaccine")
setnames(dt09, "q9003", "had.swineflu.vaccine")
setnames(dt09, "q9004", "intend.swineflue.vaccine")
setnames(dt09, "q9005", "date.swineflu.vaccine")
setnames(dt09, "q9006", "why.not.swineflu.vaccine")
setnames(dt09, "q9007", "why.swineflu.vaccine")
setnames(dt09, "q9008", "offered.seasonal.vaccine")
setnames(dt09, "q9009", "why.offered.seasonal.vaccine")
setnames(dt09, "q9010", "had.seasonal.vaccine")
setnames(dt09, "q9011", "intend.seasonal.vaccine")
setnames(dt09, "q9012", "date.seasonal.vaccine")
setnames(dt09, "q9013", "why.not.seasonal.vaccine")
setnames(dt09, "q9014", "why.seasonal.vaccine")
setnames(dt09, "q3000", "symptoms")
setnames(dt09, "q3001", "symptoms.start")
setnames(dt09, "q3002", "fever")
setnames(dt09, "q3003", "fever.start")
setnames(dt09, "q30051", "medical.service.visit")
setnames(dt09, "q3007", "alter.routine")
setnames(dt09, "q3008", "howlong.altered")
setnames(dt09, "q3000_1", "blocked.runny.nose")
setnames(dt09, "q3000_2", "cough")
setnames(dt09, "q3000_3", "sore.throat")
setnames(dt09, "q3000_4", "headache")
setnames(dt09, "q3000_5", "muscle.and.or.joint.pain")
setnames(dt09, "q3000_6", "chest.pain")
setnames(dt09, "q3000_7", "stomach.ache")
setnames(dt09, "q3000_8", "diarrhoea")
setnames(dt09, "q3000_9", "nausea")
setnames(dt09, "q3000_10", "chills")
setnames(dt09, "q3000_11", "weakness")
setnames(dt09, "q3000_12", "eye.irritation")
setnames(dt09, "q3000_13", "fever.symptom")
setnames(dt09, "q3000_14", "no.symptoms")
setnames(dt09, "q30051_1", "medical.service.visit.gp")
setnames(dt09, "q30051_2", "medical.service.visit.hospital")
setnames(dt09, "q30051_3", "medical.service.visit.other")
setnames(dt09, "q30051_4", "medical.service.visit.no")
setnames(dt09, "q30051_5", "medical.service.visit.ae")
setnames(dt09, "q4000.1", "conversational.home.0-4")
setnames(dt09, "q4001", "conversational.home.5-18")
setnames(dt09, "q4002", "conversational.home.19-64")
setnames(dt09, "q4003", "conversational.home.65+")
setnames(dt09, "q4004", "conversational.work.0-4")
setnames(dt09, "q4005", "conversational.work.5-18")
setnames(dt09, "q4006", "conversational.work.19-64")
setnames(dt09, "q4007", "conversational.work.65+")
setnames(dt09, "q4008", "conversational.other.0-4")
setnames(dt09, "q4009", "conversational.other.5-18")
setnames(dt09, "q4010", "conversational.other.19-64")
setnames(dt09, "q4011", "conversational.other.65+")
setnames(dt09, "q4012", "physical.home.0-4")
setnames(dt09, "q4013", "physical.home.5-18")
setnames(dt09, "q4014", "physical.home.19-64")
setnames(dt09, "q4015", "physical.home.65+")
setnames(dt09, "q4016", "physical.work.0-4")
setnames(dt09, "q4017", "physical.work.5-18")
setnames(dt09, "q4018", "physical.work.19-64")
setnames(dt09, "q4019", "physical.work.65+")
setnames(dt09, "q4020", "physical.other.0-4")
setnames(dt09, "q4021", "physical.other.5-18")
setnames(dt09, "q4022", "physical.other.19-64")
setnames(dt09, "q4023", "physical.other.65+")
setnames(dt09, "q4024", "public.transport")
setnames(dt09, "q4025", "enclosed.indoor.space")

dt09[, "conversational.home.0-4" := as.numeric(as.character(get("conversational.home.0-4")))]
dt09[, "conversational.home.5-18" := as.numeric(as.character(get("conversational.home.5-18")))]
dt09[, "conversational.home.19-64" := as.numeric(as.character(get("conversational.home.19-64")))]
dt09[, "conversational.home.65+" := as.numeric(as.character(get("conversational.home.65+")))]
dt09[, "conversational.work.0-4" := as.numeric(as.character(get("conversational.work.0-4")))]
dt09[, "conversational.work.5-18" := as.numeric(as.character(get("conversational.work.5-18")))]
dt09[, "conversational.work.19-64" := as.numeric(as.character(get("conversational.work.19-64")))]
dt09[, "conversational.work.65+" := as.numeric(as.character(get("conversational.work.65+")))]
dt09[, "conversational.other.0-4" := as.numeric(as.character(get("conversational.other.0-4")))]
dt09[, "conversational.other.5-18" := as.numeric(as.character(get("conversational.other.5-18")))]
dt09[, "conversational.other.19-64" := as.numeric(as.character(get("conversational.other.19-64")))]
dt09[, "conversational.other.65+" := as.numeric(as.character(get("conversational.other.65+")))]
dt09[, "physical.home.0-4" := as.numeric(as.character(get("physical.home.0-4")))]
dt09[, "physical.home.5-18" := as.numeric(as.character(get("physical.home.5-18")))]
dt09[, "physical.home.19-64" := as.numeric(as.character(get("physical.home.19-64")))]
dt09[, "physical.home.65+" := as.numeric(as.character(get("physical.home.65+")))]
dt09[, "physical.work.0-4" := as.numeric(as.character(get("physical.work.0-4")))]
dt09[, "physical.work.5-18" := as.numeric(as.character(get("physical.work.5-18")))]
dt09[, "physical.work.19-64" := as.numeric(as.character(get("physical.work.19-64")))]
dt09[, "physical.work.65+" := as.numeric(as.character(get("physical.work.65+")))]
dt09[, "physical.other.0-4" := as.numeric(as.character(get("physical.other.0-4")))]
dt09[, "physical.other.5-18" := as.numeric(as.character(get("physical.other.5-18")))]
dt09[, "physical.other.19-64" := as.numeric(as.character(get("physical.other.19-64")))]
dt09[, "physical.other.65+" := as.numeric(as.character(get("physical.other.65+")))]

dt09[,"conversational.home" := get("conversational.home.0-4") +
     get("conversational.home.5-18") + get("conversational.home.19-64") +
     get("conversational.home.65+"), with=F]
dt09[,"conversational.work" := get("conversational.work.0-4") +
     get("conversational.work.5-18") + get("conversational.work.19-64") +
     get("conversational.work.65+"), with=F]
dt09[,"conversational.other" := get("conversational.other.0-4") +
     get("conversational.other.5-18") + get("conversational.other.19-64") +
     get("conversational.other.65+"), with=F]
dt09[,"conversational.0-4" := get("conversational.home.0-4") +
     get("conversational.work.0-4") + get("conversational.other.0-4"), with=F]
dt09[,"conversational.5-18" := get("conversational.home.5-18") +
     get("conversational.work.5-18") + get("conversational.other.5-18"), with=F]
dt09[,"conversational.19-64" := get("conversational.home.19-64") +
     get("conversational.work.19-64") + get("conversational.other.19-64"), with=F]
dt09[,"conversational.65+" := get("conversational.home.65+") +
     get("conversational.work.65+") + get("conversational.other.65+"), with=F]
dt09[,"conversational" := get("conversational.home") +
     get("conversational.work") + get("conversational.other"), with=F]
dt09[,"physical.home" := get("physical.home.0-4") +
     get("physical.home.5-18") + get("physical.home.19-64") +
     get("physical.home.65+"), with=F]
dt09[,"physical.work" := get("physical.work.0-4") +
     get("physical.work.5-18") + get("physical.work.19-64") +
     get("physical.work.65+"), with=F]
dt09[,"physical.other" := get("physical.other.0-4") +
     get("physical.other.5-18") + get("physical.other.19-64") +
     get("physical.other.65+"), with=F]
dt09[,"physical.0-4" := get("physical.home.0-4") +
     get("physical.work.0-4") + get("physical.other.0-4"), with=F]
dt09[,"physical.5-18" := get("physical.home.5-18") +
     get("physical.work.5-18") + get("physical.other.5-18"), with=F]
dt09[,"physical.19-64" := get("physical.home.19-64") +
     get("physical.work.19-64") + get("physical.other.19-64"), with=F]
dt09[,"physical.65+" := get("physical.home.65+") +
     get("physical.work.65+") + get("physical.other.65+"), with=F]
dt09[,"physical" := get("physical.home") +
     get("physical.work") + get("physical.other"), with=F]

## assign some useful variables: ili yes/no, number of reports, symptoms start
## (as date), week of report, weight (for histograms later,
## i.e. 1/(number of reports that week), and birthdate
dt09$ili.notired <- ((dt09$fever  | dt09$headache  |
                      dt09$muscle.and.or.joint.pain) &
                     (dt09$sore.throat  | dt09$cough))
dt09$ili.notired <- as.numeric(dt09$ili.notired)

dt09$ili.fever <- ((dt09$fever ) & (dt09$sore.throat  | dt09$cough))
dt09$ili.fever <- as.numeric(dt09$ili.fever)
dt09$ili.fever <- as.numeric(dt09$ili.fever)

freq <-
    data.table(aggregate(dt09$global.id.number,
                         by=list(dt09$global.id.number),
                         length))
setkey(freq, Group.1)
dt09 <- dt09[freq]
setnames(dt09, "x", "nReports")

mindate <-
    data.table(aggregate(dt09$date,
                         by=list(dt09$global.id.number),
                         min))
setkey(mindate, Group.1)
dt09 <- dt09[mindate]
setnames(dt09, "x", "mindate")
maxdate <-
    data.table(aggregate(dt09$date,
                         by=list(dt09$global.id.number),
                         max))
setkey(maxdate, Group.1)
dt09 <- dt09[maxdate]
setnames(dt09, "x", "maxdate")

dt09$symptoms.start <- as.Date(dt09$symptoms.start, "%Y-%m-%d")
dt09$week <- format(dt09$date, format="%G-%W")
dt09[dt09$week=="2009-00"]$week <- "2009-52"
##dt09$weight <- 1/hist(dt09$week, breaks=seq(0,52), plot=F)$counts[dt09$week]
dt09$birthdate <- as.Date(dt09$birthyear, "%Y")

## more variables to be used later
dt09$norisk <- factor(as.numeric(dt09$riskgroup == 0))
dt09$atrisk <- dt09$norisk
levels(dt09$atrisk) <- c(1,0)
dt09$atrisk <- as.numeric(paste(dt09$atrisk))
dt09$age <-  0
dt09$age <- apply(dt09, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                                   as.Date(x["date"]))})
dt09$agegroup <- cut(dt09$age, breaks=c(0,18,45,65, max(dt09$age, na.rm=T)),
                     include.lowest=T, right=F)
                                        #dt09$vaccine.date <- as.Date(dt09$date.vaccine)
dt09$vaccine <- as.numeric(dt09$vaccine.last.year==1)

dt09$nb.household.0.4 <- as.character(dt09$nb.household.0.4)
dt09$nb.household.5.18 <- as.character(dt09$nb.household.5.18)
dt09$nb.household.19.64 <- as.character(dt09$nb.household.19.64)
dt09[, "nb.household.65+" := as.character(get("nb.household.65+"))]
invalid <- dt09$nb.household.0.4 == "NULL" & dt09$nb.household.5.18 == "NULL" &
    dt09$nb.household.19.64 == "NULL" & dt09$nb.household.65. == "NULL"
dt09[invalid]$nb.household.0.4 <- "NA"
dt09[invalid]$nb.household.5.18 <- "NA"
dt09[invalid]$nb.household.19.64 <- "NA"
dt09[invalid]$nb.household.65. <- "NA"
dt09$nb.household.0.4 <- as.numeric(dt09$nb.household.0.4)
dt09$nb.household.5.18 <- as.numeric(dt09$nb.household.5.18)
dt09$nb.household.19.64 <- as.numeric(dt09$nb.household.19.64)
dt09[, "nb.household.65+" := as.numeric(get("nb.household.65+"))]

dt09$children <- as.numeric((dt09$nb.household.0.4 > 0 | dt09$nb.household.5.18 > 0))

