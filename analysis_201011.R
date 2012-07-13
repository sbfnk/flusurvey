library(data.table)
library(ggplot2)

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
sf <- read.csv('weekly_201011.csv', header=T, sep=',');
bf <- read.csv('intake_201011.csv', header=T, sep=',');
#cf <- read.csv('contacts_201011.csv', header=T, sep=';');

# assign global id numbers
bf$global.id.number <- bf$user_id
sf$global.id.number <- sf$user_id

# put data in data tables (for the rolling join to be used later)
st <- data.table(sf)
bt <- data.table(bf)
#ct <- data.table(cf)

rm(sf)
rm(bf)

st$date <- as.Date(st$date, "%d/%m/%Y")
bt$date <- as.Date(bt$created, "%d/%m/%Y")
#ct$date <- as.Date(ct$date)

# rolling join of symptoms and background, by id number (first) and date
# (second) 
setkey(st, global.id.number, date)
setkey(bt, global.id.number, date)
dt <- bt[st, roll=TRUE]

#setkey(ct, uid, date)
#dt <- bt[ct[st, roll=TRUE], roll=TRUE]

rm(bt)
rm(st)
#rm(ct)

setnames(dt, "IntakeQ1", "gender")
setnames(dt, "IntakeQ2", "birthmonth")
setnames(dt, "IntakeQ3", "postcode")
setnames(dt, "IntakeQ4", "work.postcode")
setnames(dt, "IntakeQ5", "frequent.contact")
setnames(dt, "IntakeQ5.0", "frequent.contact.children")
setnames(dt, "IntakeQ5.1", "frequent.contact.elderly")
setnames(dt, "IntakeQ5.2", "frequent.contact.patients")
setnames(dt, "IntakeQ5.3", "frequent.contact.people")
setnames(dt, "IntakeQ6", "household")
setnames(dt, "IntakeQ6.0", "nb.household.0.4")
setnames(dt, "IntakeQ6.1", "nb.household.5.18")
setnames(dt, "IntakeQ6.2", "nb.household.19.44")
setnames(dt, "IntakeQ6.3", "nb.household.45.64")
setnames(dt, "IntakeQ6.4", "nb.household.65+")
setnames(dt, "IntakeQ7", "transport")
setnames(dt, "IntakeQ7b", "howlong.transport")
setnames(dt, "IntakeQ8", "vaccine.swineflu")
setnames(dt, "IntakeQ8b", "date.vaccine.swineflu")
setnames(dt, "IntakeQ9", "vaccine.last.year")
setnames(dt, "IntakeQ10", "vaccine.this.year")
setnames(dt, "IntakeQ10b", "date.vaccine")
setnames(dt, "IntakeQ10c", "why.vaccine")
setnames(dt, "IntakeQ10c.0", "why.vaccine.riskgroup")
setnames(dt, "IntakeQ10c.1", "why.vaccine.protected")
setnames(dt, "IntakeQ10c.2", "why.vaccine.protect.others")
setnames(dt, "IntakeQ10c.3", "why.vaccine.doctor")
setnames(dt, "IntakeQ10c.4", "why.vaccine.work.recommended")
setnames(dt, "IntakeQ10c.5", "why.vaccine.convenient")
setnames(dt, "IntakeQ10c.6", "why.vaccine.free")
setnames(dt, "IntakeQ10c.7", "why.vaccine.nomiss.work")
setnames(dt, "IntakeQ10c.8", "why.vaccine.always")
setnames(dt, "IntakeQ10c.9", "why.vaccine.other")
setnames(dt, "IntakeQ10d", "why.not.vaccine")
setnames(dt, "IntakeQ10d.0", "why.not.vaccine.notyet")
setnames(dt, "IntakeQ10d.1", "why.not.vaccine.norisk")
setnames(dt, "IntakeQ10d.2", "why.not.vaccine.natural")
setnames(dt, "IntakeQ10d.3", "why.not.vaccine.noteffective")
setnames(dt, "IntakeQ10d.4", "why.not.vaccine.minor")
setnames(dt, "IntakeQ10d.5", "why.not.vaccine.cause")
setnames(dt, "IntakeQ10d.6", "why.not.vaccine.side.effects")
setnames(dt, "IntakeQ10d.7", "why.not.vaccine.unavailable")
setnames(dt, "IntakeQ10d.8", "why.not.vaccine.not.free")
setnames(dt, "IntakeQ10d.9", "why.not.vaccine.dislike.injections")
setnames(dt, "IntakeQ10d.10", "why.not.vaccine.no.reason")
setnames(dt, "IntakeQ10d.11", "why.not.vaccine.doctor")
setnames(dt, "IntakeQ10d.12", "why.not.vaccine.notoffered")
setnames(dt, "IntakeQ12", "risk")
setnames(dt, "IntakeQ12.1", "risk.diabetes")
setnames(dt, "IntakeQ12.2", "risk.lung")
setnames(dt, "IntakeQ12.3", "risk.heart")
setnames(dt, "IntakeQ12.4", "risk.kidney")
setnames(dt, "IntakeQ12.5", "risk.immune")
setnames(dt, "IntakeQ12.6", "norisk")
setnames(dt, "IntakeQ13", "pregnant")
setnames(dt, "IntakeQ14", "smoke")
setnames(dt, "IntakeQ15", "allergy")
setnames(dt, "IntakeQ15.0", "allergy.hayfever")
setnames(dt, "IntakeQ15.1", "allergy.dust")
setnames(dt, "IntakeQ15.2", "allergy.animals")
setnames(dt, "IntakeQ15.3", "allergy.other")
setnames(dt, "WeeklyQ1.0", "no.symptoms")
setnames(dt, "WeeklyQ1.1", "fever")
setnames(dt, "WeeklyQ1.2", "watery.eyes")
setnames(dt, "WeeklyQ1.3", "blocked.runny.nose")
setnames(dt, "WeeklyQ1.4", "sneezing")
setnames(dt, "WeeklyQ1.5", "sore.throat")
setnames(dt, "WeeklyQ1.6", "cough")
setnames(dt, "WeeklyQ1.7", "phlegm")
setnames(dt, "WeeklyQ1.8", "headache")
setnames(dt, "WeeklyQ1.9", "muscle.and.or.joint.pain")
setnames(dt, "WeeklyQ1.10", "chest.pain")
setnames(dt, "WeeklyQ1.11", "tired")
setnames(dt, "WeeklyQ1.12", "loss.appetite")
setnames(dt, "WeeklyQ1.13", "nausea")
setnames(dt, "WeeklyQ1.14", "vomiting")
setnames(dt, "WeeklyQ1.15", "diarrhoea")
setnames(dt, "WeeklyQ1.16", "other")
setnames(dt, "WeeklyQ1.17", "chills")
setnames(dt, "WeeklyQ1.18", "shortness.breath")
setnames(dt, "WeeklyQ1.19", "stomach.ache")
setnames(dt, "WeeklyQ1b", "symptoms.suddenly")
setnames(dt, "WeeklyQ2c", "fever.start")
setnames(dt, "WeeklyQ3", "same")
setnames(dt, "WeeklyQ4", "symptoms.start.date")
setnames(dt, "WeeklyQ5", "symptoms.end.date")
setnames(dt, "WeeklyQ6.0", "visit.medical.service.gp")
setnames(dt, "WeeklyQ6.1", "visit.medical.service.hospital")
setnames(dt, "WeeklyQ6.2", "visit.medical.service.ae")
setnames(dt, "WeeklyQ6.3", "visit.medical.service.other")
setnames(dt, "WeeklyQ6.4", "visit.medical.service.no")
setnames(dt, "WeeklyQ6.5", "visit.medical.service.appointment")
setnames(dt, "WeeklyQ6b", "visit.medical.service.howsoon")
setnames(dt, "WeeklyQ8", "alter.routine")
setnames(dt, "WeeklyQ8c", "howlong.altered")
setnames(dt, "WeeklyQ9a", "howmany.household.ili")
setnames(dt, "WeeklyQ9b", "howmany.other.ili")

# assign some useful variables: ili yes/no, number of reports, symptoms start
# (as date), week of report, weight (for histograms later,
# i.e. 1/(number of reports that week), and birthdate
dt$ili <- ((dt$symptoms.suddenly == 0) &
           (dt$fever | dt$tired | dt$headache |
            dt$muscle.and.or.joint.pain) &
           (dt$sore.throat | dt$cough | dt$shortness.breath))
dt$ili <- as.numeric(dt$ili)

dt$ili.notired <- ((dt$symptoms.suddenly == 0) &
           (dt$fever  | dt$headache  |
            dt$muscle.and.or.joint.pain) &
           (dt$sore.throat  | dt$cough | dt$shortness.breath))
dt$ili.notired <- as.numeric(dt$ili.notired)

dt$ili.fever <- ((dt$symptoms.suddenly == 0) &
           (dt$fever ) &
           (dt$sore.throat  | dt$cough | dt$shortness.breath))
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
dt[dt$week=="2010-00"]$week <- "2010-52"
#dt$weight <- 1/hist(dt$week, breaks=seq(0,52), plot=F)$counts[dt$week]
dt$birthdate <- as.Date(dt$birthmonth, "%Y-%m-%d")

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
dt$vaccine.date <- as.Date(dt$date.vaccine)
dt$vaccine <- as.numeric(dt$vaccine.this.year==0 & (is.na(dt$vaccine.date) |
                           dt$vaccine.date <= dt$date)) 

dt$children <- as.numeric((dt$nb.household.0.4 > 0 | dt$nb.household.5.18 > 0))

temp.data <- dt[!is.na(age)]
levels(temp.data$agegroup) <- c("<18","18-44","45-64","65+")
r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week,
            temp.data$ili, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_201011.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.notired, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_notired_201011.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.fever, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_fever_201011.raw", quote=F, row.names=F)

# GI stuff
dt$gi <- as.numeric(dt$diarrhoea == 1 | dt$vomiting == 1)
dt$newgi <- dt$gi
#dt[same==0, newgi := 0]
r <- ftable(dt$week, dt$newgi, row.vars=1)
gi.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(gi.raw.data) <- c("Week", "nongi", "gi")
gi.raw.data$gi.incidence <- gi.raw.data$gi / (gi.raw.data$nongi + gi.raw.data$nongi)
gi.11 <- gi.raw.data[-c(1, 20),]
write.csv(gi.11, "gi_201011.csv", quote=F, row.names=F)


