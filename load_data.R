library(data.table)
library(ggplot2)
library(reshape)

symptoms <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other")

# compute the age in years from a birthdate (from) and the current date (to)
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

sf13 <- read.csv('weekly_13.csv', sep=',', header=T)
bf13 <- read.csv('intake_13.csv', sep=',', header=T)
cf13 <- read.csv('contact_13.csv', sep=',', header=T)

# create translation table so that every participant gets a unique ID number
# (called global.id.number)
translation <- data.frame(global_id = unique(bf13$global_id))
translation$number <- seq(1,nrow(translation))

# assign global id numbers
bf13$global.id.number <- translation$number[match(bf13$global_id,
                                                translation$global_id)]
sf13$global.id.number <- translation$number[match(sf13$global_id,
                                                translation$global_id)]
cf13$global.id.number <- translation$number[match(cf13$global_id,
                                                translation$global_id)]

# put data in data tables (for the rolling join to be used later)
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

# rolling join of symptoms and background, by id number (first) and date
# (second) 
bt13 <-  bt13[!duplicated(bt13[, list(global.id.number, date)], fromLast=T)]
ct13 <-  ct13[!duplicated(ct13[, list(global.id.number, date)], fromLast=T)]
setkey(st13, global.id.number, date)
setkey(bt13, global.id.number, date)
setkey(ct13, global.id.number, date)

# set convenient names
setnames(bt13, "Q0", "self")
setnames(bt13, "Q1", "gender")
setnames(bt13, "Q2", "birthmonth")
setnames(bt13, "Q3", "postcode")
setnames(bt13, "Q4", "occupation")
setnames(bt13, "Q4b_0_open", "work.postcode")
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
setnames(bt13, "Q7", "transport")
setnames(bt13, "Q7b", "howlong.transport")
setnames(bt13, "Q9", "vaccine.last.year")
setnames(bt13, "Q10", "vaccine.this.year")
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
setnames(bt13, "Q13", "smoke")
setnames(bt13, "Q14_1", "allergy.hayfever")
setnames(bt13, "Q14_2", "allergy.dust")
setnames(bt13, "Q14_3", "allergy.animals")
setnames(bt13, "Q14_4", "allergy.other")
setnames(bt13, "Q14_5", "allergy.none")

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
setnames(st13, "Q3_0_open", "symptoms.start.date")
setnames(st13, "Q4_0_open", "symptoms.end.date")
setnames(st13, "Q5", "symptoms.suddenly")
setnames(st13, "Q6_1_open", "fever.start")
setnames(st13, "Q6b", "fever.suddenly")
# setnames(st13, "Q7_0", "visit.medical.service.no")
# setnames(st13, "Q7_1", "visit.medical.service.gp")
# setnames(st13, "Q7_2", "visitp.medical.service.ae")
# setnames(st13, "Q7_3", "visit.medical.service.hospital")
# setnames(st13, "Q7_4", "visit.medical.service.other")
# setnames(st13, "Q7_5", "visit.medical.service.appointment")
# setnames(st13, "Q7b", "visit.medical.service.howsoon")
# setnames(st13, "Q8_0", "contact.medical.service.no")
# setnames(st13, "Q8_1", "contact.medical.service.gp.receptionist")
# setnames(st13, "Q8_2", "contact.medical.service.gp.doctor")
# setnames(st13, "Q8_3", "contact.medical.service.nhs")
# setnames(st13, "Q8_5", "contact.medical.service.other")
# setnames(st13, "Q9_0", "no.medication")
# setnames(st13, "Q9_1", "medication.painkillers")
# setnames(st13, "Q9_2", "medication.cough")
# setnames(st13, "Q9_3", "medication.antiviral")
# setnames(st13, "Q9_4", "medication.antibiotic")
# setnames(st13, "Q9_5", "medication.other")
# setnames(st13, "Q9_6", "medication.dontknow")
# setnames(st13, "Q10", "alter.routine")
# setnames(st13, "Q10c", "howlong.altered")
# setnames(st13, "Q12_multi_row1_col1", "howmany.household.ili")
# setnames(st13, "Q13_multi_row1_col1", "howmany.other.ili")

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

dt13 <- bt13[ct13[st13, roll=TRUE], roll = TRUE]
dt13[,country := "uk"]

#cleanup (some participants have only a weekly survey, no background one)
dt13 <- dt13[!is.na(global.id.number)]

rm(bt13)
rm(st13)
rm(ct13)

dt <- dt13

# assign some useful variables: ili yes/no, number of reports, symptoms start
# (as date), week of report, weight (for histograms later,
# i.e. 1/(number of reports that week), and birthdate
dt$ili <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t" | dt$tired == "t" | dt$headache == "t" |
            dt$muscle.and.or.joint.pain =="t") &
           (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath
            =="t"))
dt$ili <- as.numeric(dt$ili)

dt$ili.notired <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t" | dt$headache == "t" |
            dt$muscle.and.or.joint.pain =="t") &
           (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath
            =="t"))
dt$ili.notired <- as.numeric(dt$ili.notired)

dt$ili.fever <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t") &
           (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath
            =="t"))
dt$ili.fever <- as.numeric(dt$ili.fever)


freq <-
  data.table(aggregate(dt$global.id.number,
                       by=list(dt$global.id.number),
                       length))
setkey(freq, Group.1)
dt <- dt[freq]
setnames(dt, "x", "nReports")

mindate <-
  data.table(aggregate(dt$date,
                       by=list(dt$global.id.number),
                       min))
setkey(mindate, Group.1)
dt <- dt[mindate]
setnames(dt, "x", "mindate")
maxdate <-
  data.table(aggregate(dt$date,
                       by=list(dt$global.id.number),
                       max))
setkey(maxdate, Group.1)
dt <- dt[maxdate]
setnames(dt, "x", "maxdate")

dt$symptoms.start <- as.Date(dt$symptoms.start, "%Y-%m-%d")
dt$week <- format(dt$date, format="%G-%W")
dt[dt$week=="2013-00"]$week <- "2012-53"
dt[dt$week=="2013-53"]$week <- "2012-53"
dt$weekweight <- 1/table(dt$week)[dt$week]
dt$birthdate <- as.Date(paste(dt$birthmonth, "-01",sep=""))

# more variables to be used later
dt$norisk <- factor(dt$norisk)
dt$atrisk <- dt$norisk
levels(dt$atrisk) <- c(1,0)
dt$atrisk <- as.numeric(paste(dt$atrisk))
dt$age <-  0
dt$age <- apply(dt, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                               as.Date(x["date"]))})
dt$agegroup <- cut(dt$age, breaks=c(0,18,45,65, max(dt$age, na.rm=T)),
                   include.lowest=T, right=F)
dt$vaccine.date <- as.Date(dt$date.vaccine, "%Y/%m/%d")
## dt$vaccine <- as.numeric(dt$vaccine.this.year==0 & (is.na(dt$vaccine.date) |
##                            dt$vaccine.date <= dt$date)) 
dt$children <- as.numeric((dt$household.0.4 == "t" | dt$household.5.18 == "t"))
dt$symptoms.start.date <- as.Date(dt$symptoms.start.date, "%Y-%m-%d")
dt$symptoms.end.date <- as.Date(dt$symptoms.end.date, "%Y-%m-%d")
dt$symptoms.start.week <- format(dt$symptoms.start.date, format="%G-%W")
dt[dt$symptoms.start.week=="2013-00"]$symptoms.start.week <- "2012-52"
dt[dt$symptoms.start.week=="2013-53"]$symptoms.start.week <- "2012-52"

dt$ili.self <- (dt$Q11 == 0)
dt[is.na(ili.self)]$ili.self <- FALSE

dt$using.transport <- (dt$transport > 0)

uk.ur <- read.csv("urban_rural.csv", header=F, sep=",")

dt$postcode <- sub("[[:blank:]]+$", "", dt$postcode)
dt$postcode <- toupper(dt$postcode)

dt$work.postcode <- sub("[[:blank:]]+$", "", dt$work.postcode)
dt$work.postcode <- toupper(dt$work.postcode)

dt <- dt[country == "uk", "ur" := uk.ur$V3[match(dt[country == "uk",]$postcode,
                            uk.ur$V1)], with=F] 
dt <- dt[country == "uk", "uk.country" := uk.ur$V2[match(dt[country ==
                            "uk"]$postcode, uk.ur$V1)], with=F]
dt <- dt[country == "uk", "urban" := rep(0, length(dt[country ==
                            "uk"]$postcode)), with=F]
dt <- dt[country == "uk", "ur" := uk.ur$V3[match(dt[country == "uk",]$postcode,
                            uk.ur$V1)], with=F] 
dt <- dt[country == "uk", "uk.country" := uk.ur$V2[match(dt[country ==
                            "uk"]$postcode, uk.ur$V1)], with=F]
dt <- dt[country == "uk", "urban" := rep(0, length(dt[country ==
                            "uk"]$postcode)), with=F]

dt[country == "uk" & is.na(dt$ur),]$urban <- 2

dt[dt$uk.country %in% c("E","W") & !(dt$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
dt[dt$uk.country %in% c("E","W") & dt$ur %in% c(1,5),]$urban <- 1

dt[dt$uk.country == "S" & dt$ur %in% c(1,2),]$urban <- 1
dt[dt$uk.country == "S" & dt$ur %in% c(3,4,5,6,7),]$urban <- 0

dt[dt$uk.country == "N" & dt$ur %in% c(1,2,3,4),]$urban <- 1
dt[dt$uk.country == "N" & !(dt$ur %in% c(5,6,7)),]$urban <- 0

dt$urban <- as.factor(dt$urban)

dt <- dt[country == "uk", "work.ur" := uk.ur$V3[match(dt[country ==
                            "uk",]$work.postcode, uk.ur$V1)], with=F] 
dt <- dt[country == "uk", "work.uk.country" := uk.ur$V2[match(dt[country ==
                            "uk"]$work.postcode, uk.ur$V1)], with=F]
dt <- dt[country == "uk", "work.urban" := rep(0, length(dt[country ==
                            "uk"]$work.postcode)), with=F]

dt[country == "uk" & is.na(dt$work.ur),]$work.urban <- 2

dt[dt$work.uk.country %in% c("E","W") & !(dt$work.ur %in% c(2,3,4,6,7,8)),]$work.urban <- 0
dt[dt$work.uk.country %in% c("E","W") & dt$work.ur %in% c(1,5),]$work.urban <- 1

dt[dt$work.uk.country == "S" & dt$work.ur %in% c(1,2),]$work.urban <- 1
dt[dt$work.uk.country == "S" & dt$work.ur %in% c(3,4,5,6,7),]$work.urban <- 0

dt[dt$work.uk.country == "N" & dt$work.ur %in% c(1,2,3,4),]$work.urban <- 1
dt[dt$work.uk.country == "N" & !(dt$work.ur %in% c(5,6,7)),]$work.urban <- 0

dt$work.urban <- as.factor(dt$work.urban)

saveRDS(dt, "flusurvey_201213.rds")
