library(data.table)

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

# read tables
sf <- read.csv('symptoms_200910.csv', header=T, sep=';');
bf <- read.csv('background_200910.csv', header=T, sep=';');
#cf <- read.csv('contacts_200910.csv', header=T, sep=';');
#vf <- read.csv('vaccine_200910.csv', header=T, sep=';');

# assign global id numbers
bf$global.id.number <- bf$uid
sf$global.id.number <- sf$uid

# put data in data tables (for the rolling join to be used later)
st <- data.table(sf)
bt <- data.table(bf)
#ct <- data.table(cf)
#vt <- data.table(vf)

rm(sf)
rm(bf)

st <- st[!is.na(uid)]

st$date <- as.Date(st$date)
bt$date <- as.Date(bt$date)
#ct$date <- as.Date(ct$date)
#vt$date <- as.Date(vt$date)

st <- st[!is.na(date)]

#setnames(bt, 1, "bid")
#setnames(ct, 1, "cid")
#setnames(vt, 1, "vid")

st <- st[-(1:6),]

# rolling join of symptoms and background, by id number (first) and date
# (second) 
setkey(st, global.id.number, date)
setkey(bt, global.id.number, date)
#setkey(ct, uid, date)
#setkey(vt, uid, date)
dt <- bt[st, roll=TRUE]
#dt <- bt[vt[ct[st, roll=TRUE], roll=TRUE], roll=TRUE]

rm(bt)
#rm(vt)
#rm(ct)
rm(st)

setnames(dt, "q1000", "postcode")
setnames(dt, "q1001", "gender")
setnames(dt, "q1002", "birthyear")
setnames(dt, "q2001", "transport")
setnames(dt, "q2040", "vaccine.last.year")
setnames(dt, "q2004", "riskgroup")
setnames(dt, "q2005", "smoke")
setnames(dt, "q2008", "exercise")
setnames(dt, "q2009001", "nb.household")
setnames(dt, "q2009002", "nb.household.0.4")
setnames(dt, "q2009003", "nb.household.5.18")
setnames(dt, "q2009004", "nb.household.19.64")
setnames(dt, "q2009005", "nb.household.65+")
setnames(dt, "q2010", "school.nursery")
setnames(dt, "q2011", "frequent.contact")
setnames(dt, "q2004_1", "chronic.heart.disease")
setnames(dt, "q2004_2", "diabetes")
setnames(dt, "q2004_3", "asthma")
setnames(dt, "q2004_4", "other.chronic.lung.disease")
setnames(dt, "q2004_5", "pregnant")
setnames(dt, "q2004_6", "immunocompromised")
setnames(dt, "q2004_7", "other.chronic")
setnames(dt, "q2011_1", "frequent.contact.children")
setnames(dt, "q2011_2", "frequent.contact.patients")
setnames(dt, "q2011_3", "frequent.contact.elderly")
setnames(dt, "q2011_4", "frequent.contact.people")
## setnames(dt, 88, "offered.swineflu.vaccine")
## setnames(dt, 89, "why.offered.swineflu.vaccine")
## setnames(dt, 90, "had.swineflu.vaccine")
## setnames(dt, 91, "intend.vaccine")
## setnames(dt, 92, "date.swineflu.vaccine")
## setnames(dt, 93, "why.not.swineflu.vaccine")
## setnames(dt, 94, "why.swineflu.vaccine")
## setnames(dt, 95, "offered.seasonal.vaccine")
## setnames(dt, 96, "why.offered.seasonal.vaccine")
## setnames(dt, 97, "had.seasonal.vaccine")
## setnames(dt, 98, "intend.vaccine")
## setnames(dt, 99, "date.seasonal.vaccine")
## setnames(dt, 100, "why.not.seasonal.vaccine")
## setnames(dt, 101, "why.seasonal.vaccine")
## setnames(dt, 102, "why.offered.seasonal.vaccine.healthcare.worker")
## setnames(dt, 103, "why.offered.seasonal.vaccine.hospital.patient")
## setnames(dt, 104, "why.offered.seasonal.vaccine.underlying.health.condition")
## setnames(dt, 105, "why.offered.seasonal.vaccine.pregnant")
## setnames(dt, 106, "why.offered.seasonal.vaccine.close.contact.with.high.risk")
## setnames(dt, 107, "why.offered.seasonal.vaccine.over.65")
## setnames(dt, 108, "why.offered.seasonal.vaccine.under.5")
## setnames(dt, 110, "why.offered.seasonal.vaccine.other")
## setnames(dt, 111, "why.offered.seasonal.vaccine.dont.know")
## setnames(dt, 112, "why.not.swineflu.vaccine.doctor")
## setnames(dt, 113, "why.not.swineflu.vaccine.already")
## setnames(dt, 114, "why.not.swineflu.vaccine.not.risk")
## setnames(dt, 115, "why.not.swineflu.vaccine.side.effects")
## setnames(dt, 116, "why.not.swineflu.vaccine.other")
## setnames(dt, 117, "why.not.swineflu.vaccine.dont.know")
## setnames(dt, 118, "why.swineflu.vaccine.doctor")
## setnames(dt, 119, "why.swineflu.vaccine.highrisk")
## setnames(dt, 120, "why.swineflu.vaccine.protected")
## setnames(dt, 121, "why.swineflu.vaccine.protect.others")
## setnames(dt, 122, "why.swineflu.vaccine.other")
## setnames(dt, 123, "why.swineflu.vaccine.dontknow")
## setnames(dt, 124, "why.offered.seasonal.vaccine.healthcare.worker")
## setnames(dt, 125, "why.offered.seasonal.vaccine.hospital.patient")
## setnames(dt, 126, "why.offered.seasonal.vaccine.underlying.health.condition")
## setnames(dt, 127, "why.offered.seasonal.vaccine.pregnant")
## setnames(dt, 128, "why.offered.seasonal.vaccine.over.65")
## setnames(dt, 129, "why.offered.seasonal.vaccine.other")
## setnames(dt, 130, "why.offered.seasonal.vaccine.dont.know")
## setnames(dt, 131, "why.not.seasonal.vaccine.doctor")
## setnames(dt, 132, "why.not.seasonal.vaccine.already")
## setnames(dt, 133, "why.not.seasonal.vaccine.not.risk")
## setnames(dt, 134, "why.not.seasonal.vaccine.side.effects")
## setnames(dt, 135, "why.not.seasonal.vaccine.other")
## setnames(dt, 136, "why.not.seasonal.vaccine.dont.know")
## setnames(dt, 137, "why.seasonal.vaccine.doctor")
## setnames(dt, 138, "why.seasonal.vaccine.highrisk")
## setnames(dt, 139, "why.seasonal.vaccine.protected")
## setnames(dt, 140, "why.seasonal.vaccine.protect.others")
## setnames(dt, 141, "why.seasonal.vaccine.other")
## setnames(dt, 142, "why.seasonal.vaccine.dontknow")
## setnames(dt, 143, "conversational.home.0-4")
## setnames(dt, 144, "conversational.home.5-18")
## setnames(dt, 145, "conversational.home.19-64")
## setnames(dt, 146, "conversational.home.65+")
## setnames(dt, 147, "conversational.work.0-4")
## setnames(dt, 148, "conversational.work.5-18")
## setnames(dt, 149, "conversational.work.19-64")
## setnames(dt, 150, "conversational.work.65+")
## setnames(dt, 151, "conversational.other.0-4")
## setnames(dt, 152, "conversational.other.5-18")
## setnames(dt, 153, "conversational.other.19-64")
## setnames(dt, 154, "conversational.other.65+")
## setnames(dt, 155, "physical.home.0-4")
## setnames(dt, 156, "physical.home.5-18")
## setnames(dt, 157, "physical.home.19-64")
## setnames(dt, 158, "physical.home.65+")
## setnames(dt, 159, "physical.work.0-4")
## setnames(dt, 160, "physical.work.5-18")
## setnames(dt, 161, "physical.work.19-64")
## setnames(dt, 162, "physical.work.65+")
## setnames(dt, 163, "physical.other.0-4")
## setnames(dt, 164, "physical.other.5-18")
## setnames(dt, 165, "physical.other.19-64")
## setnames(dt, 166, "physical.other.65+")
## setnames(dt, 167, "public.transport")
## setnames(dt, 168, "enclosed.indoor.space")
setnames(dt, "q3000", "symptoms")
setnames(dt, "q3001", "symptoms.start")
setnames(dt, "q3002", "fever")
setnames(dt, "q3003", "fever.start")
setnames(dt, "q30051", "medical.service.visit")
setnames(dt, "q3007", "alter.routine")
setnames(dt, "q3008", "howlong.altered")
setnames(dt, "q3000_1", "blocked.runny.nose")
setnames(dt, "q3000_2", "cough")
setnames(dt, "q3000_3", "sore.throat")
setnames(dt, "q3000_4", "headache")
setnames(dt, "q3000_5", "muscle.and.or.joint.pain")
setnames(dt, "q3000_6", "chest.pain")
setnames(dt, "q3000_7", "stomach.ache")
setnames(dt, "q3000_8", "diarrhoea")
setnames(dt, "q3000_9", "nausea")
setnames(dt, "q3000_10", "chills")
setnames(dt, "q3000_11", "weakness")
setnames(dt, "q3000_12", "eye.irritation")
setnames(dt, "q3000_13", "fever.symptom")
setnames(dt, "q3000_14", "no.symptoms")
setnames(dt, "q30051_1", "medical.service.visit.gp")
setnames(dt, "q30051_2", "medical.service.visit.hospital")
setnames(dt, "q30051_3", "medical.service.visit.other")
setnames(dt, "q30051_4", "medical.service.visit.no")
setnames(dt, "q30051_5", "medical.service.visit.ae")

# assign some useful variables: ili yes/no, number of reports, symptoms start
# (as date), week of report, weight (for histograms later,
# i.e. 1/(number of reports that week), and birthdate
dt$ili.notired <- ((dt$fever  | dt$headache  |
                    dt$muscle.and.or.joint.pain) &
                   (dt$sore.throat  | dt$cough))
dt$ili.notired <- as.numeric(dt$ili.notired)

dt$ili.fever <- ((dt$fever ) & (dt$sore.throat  | dt$cough))
dt$ili.fever <- as.numeric(dt$ili.fever)
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
dt[dt$week=="2009-00"]$week <- "2009-52"
#dt$weight <- 1/hist(dt$week, breaks=seq(0,52), plot=F)$counts[dt$week]
dt$birthdate <- as.Date(dt$birthyear, "%Y")

# more variables to be used later
dt$norisk <- factor(as.numeric(dt$riskgroup == 0))
dt$atrisk <- dt$norisk
levels(dt$atrisk) <- c(1,0)
dt$atrisk <- as.numeric(paste(dt$atrisk))
dt$age <-  0
dt$age <- apply(dt, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                               as.Date(x["date"]))})
dt$agegroup <- cut(dt$age, breaks=c(0,18,45,65, max(dt$age, na.rm=T)),
                   include.lowest=T, right=F)
#dt$vaccine.date <- as.Date(dt$date.vaccine)
dt$vaccine <- as.numeric(dt$vaccine.last.year==1)

dt$nb.household.0.4 <- as.character(dt$nb.household.0.4)
dt$nb.household.5.18 <- as.character(dt$nb.household.5.18)
dt$nb.household.19.64 <- as.character(dt$nb.household.19.64)
dt$nb.household.65. <- as.character(dt$nb.household.65.)
invalid <- dt$nb.household.0.4 == "NULL" & dt$nb.household.5.18 == "NULL" &
  dt$nb.household.19.64 == "NULL" & dt$nb.household.65. == "NULL"
dt[invalid]$nb.household.0.4 <- "NA"
dt[invalid]$nb.household.5.18 <- "NA"
dt[invalid]$nb.household.19.64 <- "NA"
dt[invalid]$nb.household.65. <- "NA"
dt$nb.household.0.4 <- as.numeric(dt$nb.household.0.4)
dt$nb.household.5.18 <- as.numeric(dt$nb.household.5.18)
dt$nb.household.19.64 <- as.numeric(dt$nb.household.19.64)
dt$nb.household.65. <- as.numeric(dt$nb.household.65.)

dt$children <- as.numeric((dt$nb.household.0.4 > 0 | dt$nb.household.5.18 > 0))

temp.data <- dt[!is.na(age) & !is.na(children)]
temp.data$agegroup <- factor(temp.data$agegroup)
levels(temp.data$agegroup) <- c("<18","18-44","45-64","65+")
## r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
##             temp.data$agegroup, temp.data$week,
##             temp.data$ili, row.vars=rev(1:5))
## vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
##                                    unclass(r))
## names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
## write.csv(vaccination.raw.data, "cohorts_200910.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.notired, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_notired_200910.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.fever, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_fever_200910.raw", quote=F, row.names=F)


# GI stuff
dt$gi <- as.numeric(dt$diarrhoea == 1 | dt$nausea == 1)
dt$newgi <- dt$gi
#dt[same==0, newgi := 0]
r <- ftable(dt$week, dt$newgi, row.vars=1)
gi.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(gi.raw.data) <- c("Week", "nongi", "gi")
gi.raw.data$gi.incidence <- gi.raw.data$gi / (gi.raw.data$nongi + gi.raw.data$nongi)
gi.10 <- gi.raw.data[-c(38),]
write.csv(gi.10, "gi_200910.raw", quote=F, row.names=F)


