library(data.table)
library(ggplot2)
library(reshape)

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
sf <- read.csv('epidb_weekly.csv', sep=',', header=T)
bf <- read.csv('epidb_intake.csv', sep=',', header=T)

# create translation table so that every participant gets a unique ID number
# (called global.id.number)
translation <- data.frame(global_id = unique(bf$global_id))
translation$number <- seq(1,nrow(translation))

# assign global id numbers
bf$global.id.number <- translation$number[match(bf$global_id,
                                                translation$global_id)]
sf$global.id.number <- translation$number[match(sf$global_id,
                                                translation$global_id)]

# put data in data tables (for the rolling join to be used later)
st <- data.table(sf)
bt <- data.table(bf)

rm(sf)
rm(bf)

setnames(bt, 2, "bid")

st$date <- as.Date(st$timestamp)
bt$date <- as.Date(bt$timestamp)

# rolling join of symptoms and background, by id number (first) and date
# (second) 
setkey(st, global.id.number, date)
setkey(bt, global.id.number, date)
dt <- bt[st, roll=TRUE]

#cleanup (some participants have only a weekly survey, no background one)
dt <- dt[!is.na(country)]

rm(bt)
rm(st)

# set convenient names
setnames(dt, "Q0", "self")
setnames(dt, "Q1", "gender")
setnames(dt, "Q2", "birthmonth")
setnames(dt, "Q3", "postcode")
setnames(dt, "Q4", "occupation")
setnames(dt, "Q4b_0_open", "work.postcode")
setnames(dt, "Q4d_0", "no.education")
setnames(dt, "Q4d_1", "education.gcse")
setnames(dt, "Q4d_2", "education.alevels")
setnames(dt, "Q4d_3", "education.bsc")
setnames(dt, "Q4d_4", "education.msc")
setnames(dt, "Q4d_5", "education.stillin")
setnames(dt, "Q5_0", "frequent.contact.children")
setnames(dt, "Q5_1", "frequent.contact.elderly")
setnames(dt, "Q5_2", "frequent.contact.patients")
setnames(dt, "Q5_3", "frequent.contact.people")
setnames(dt, "Q6_0", "household.0.4")
setnames(dt, "Q6_0_open", "nb.household.0.4")
setnames(dt, "Q6_1", "household.5.18")
setnames(dt, "Q6_1_open", "nb.household.5.18")
setnames(dt, "Q6_2", "household.19.44")
setnames(dt, "Q6_2_open", "nb.household.19.44")
setnames(dt, "Q6_3", "household.45.64")
setnames(dt, "Q6_3_open", "nb.household.45.64")
setnames(dt, "Q6_4", "household.65+")
setnames(dt, "Q6_4_open", "nb.household.65+")
setnames(dt, "Q7", "transport")
setnames(dt, "Q7b", "howlong.transport")
setnames(dt, "Q9", "vaccine.last.year")
setnames(dt, "Q10", "vaccine.this.year")
setnames(dt, "Q10b_1_open", "date.vaccine")
setnames(dt, "Q10c_0", "why.vaccine.riskgroup")
setnames(dt, "Q10c_1", "why.vaccine.protected")
setnames(dt, "Q10c_2", "why.vaccine.protect.others")
setnames(dt, "Q10c_3", "why.vaccine.doctor")
setnames(dt, "Q10c_4", "why.vaccine.work.recommended")
setnames(dt, "Q10c_5", "why.vaccine.convenient")
setnames(dt, "Q10c_6", "why.vaccine.free")
setnames(dt, "Q10c_7", "why.vaccine.nomiss.work")
setnames(dt, "Q10c_8", "why.vaccine.always")
setnames(dt, "Q10c_9", "why.vaccine.other")
setnames(dt, "Q10d_0", "why.not.vaccine.notyet")
setnames(dt, "Q10d_1", "why.not.vaccine.notoffered")
setnames(dt, "Q10d_2", "why.not.vaccine.norisk")
setnames(dt, "Q10d_3", "why.not.vaccine.natural")
setnames(dt, "Q10d_4", "why.not.vaccine.noteffective")
setnames(dt, "Q10d_5", "why.not.vaccine.minor")
setnames(dt, "Q10d_6", "why.not.vaccine.unlikely")
setnames(dt, "Q10d_7", "why.not.vaccine.cause")
setnames(dt, "Q10d_8", "why.not.vaccine.side.effects")
setnames(dt, "Q10d_9", "why.not.vaccine.dont.like")
setnames(dt, "Q10d_10", "why.not.vaccine.unavailable")
setnames(dt, "Q10d_11", "why.not.vaccine.not.free")
setnames(dt, "Q10d_12", "why.not.vaccine.no.reason")
setnames(dt, "Q10d_13", "why.not.vaccine.doctor")
setnames(dt, "Q10d_14", "why.not.vaccine.other")
setnames(dt, "Q11_0", "norisk")
setnames(dt, "Q11_1", "risk.asthma")
setnames(dt, "Q11_2", "risk.diabetes")
setnames(dt, "Q11_3", "risk.lung")
setnames(dt, "Q11_4", "risk.heart")
setnames(dt, "Q11_5", "risk.kidney")
setnames(dt, "Q11_6", "risk.immune")
setnames(dt, "Q12", "pregnant")
setnames(dt, "Q13", "smoke")
setnames(dt, "Q14_1", "allergy.hayfever")
setnames(dt, "Q14_2", "allergy.dust")
setnames(dt, "Q14_3", "allergy.animals")
setnames(dt, "Q14_4", "allergy.other")
setnames(dt, "Q14_5", "allergy.none")
setnames(dt, "Q1_0", "no.symptoms")
setnames(dt, "Q1_1", "fever")
setnames(dt, "Q1_2", "chills")
setnames(dt, "Q1_3", "blocked.runny.nose")
setnames(dt, "Q1_4", "sneezing")
setnames(dt, "Q1_5", "sore.throat")
setnames(dt, "Q1_6", "cough")
setnames(dt, "Q1_7", "shortness.breath")
setnames(dt, "Q1_8", "headache")
setnames(dt, "Q1_9", "muscle.and.or.joint.pain")
setnames(dt, "Q1_10", "chest.pain")
setnames(dt, "Q1_11", "tired")
setnames(dt, "Q1_12", "loss.appetite")
setnames(dt, "Q1_13", "phlegm")
setnames(dt, "Q1_14", "watery.eyes")
setnames(dt, "Q1_15", "nausea")
setnames(dt, "Q1_16", "vomiting")
setnames(dt, "Q1_17", "diarrhoea")
setnames(dt, "Q1_18", "stomach.ache")
setnames(dt, "Q1_19", "other")
setnames(dt, "Q2.1", "same")
setnames(dt, "Q3_0_open", "symptoms.start.date")
setnames(dt, "Q4_0_open", "symptoms.end.date")
setnames(dt, "Q5", "symptoms.suddenly")
setnames(dt, "Q6_1_open.1", "fever.start")
setnames(dt, "Q6b.1", "fever.suddenly")
setnames(dt, "Q7_0", "visit.medical.service.no")
setnames(dt, "Q7_1", "visit.medical.service.gp")
setnames(dt, "Q7_2", "visit.medical.service.ae")
setnames(dt, "Q7_3", "visit.medical.service.hospital")
setnames(dt, "Q7_4", "visit.medical.service.other")
setnames(dt, "Q7_5", "visit.medical.service.appointment")
setnames(dt, "Q7b.1", "visit.medical.service.howsoon")
setnames(dt, "Q9_3", "medication.antiviral")
setnames(dt, "Q9_4", "medication.antibiotic")
setnames(dt, "Q10.1", "alter.routine")
setnames(dt, "Q10c", "howlong.altered")
setnames(dt, "Q12_multi_row1_col1", "howmany.household.ili")
setnames(dt, "Q13_multi_row1_col1", "howmany.other.ili")

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
dt[dt$week=="2011-00"]$week <- "2011-52"
dt$weekweight <- 1/table(dt$week)[dt$week]
dt$birthdate <- as.Date(dt$birthmonth, "%Y/%M/%d")

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
dt$vaccine <- as.numeric(dt$vaccine.this.year==0 & (is.na(dt$vaccine.date) |
                           dt$vaccine.date <= dt$date)) 
dt$children <- as.numeric((dt$household.0.4 == "t" | dt$household.5.18 == "t"))

# one-per-user table
ds <- dt[!duplicated(dt$global.id.number)]

ds$ili <- FALSE
ds$nbili <- with(dt, aggregate(ili,
                                    list(global.id.number=global.id.number),
                                    sum))$x
ds$ili <- (ds$nbili > 0)

ds$vaccinated <- with(dt, aggregate(vaccine,
                                         list(global.id.number=global.id.number),
                                         sum))$x > 0
ds$nonili <- 1-ds$ili
ds$smoking <- ds$smoke %in% c(1,2,3)
ds$weight <- 0
for (i in 1:length(levels(factor(ds$country)))) {
  ds[country==levels(factor(ds$country))[i]]$weight <-
    1/nrow(ds[country==levels(factor(ds$country))[i]])
}

png("attack_rate.png")
ggplot(ds[ili==T], aes(x=country, fill=country, weight=weight))+
  geom_bar(color="black")+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("attack rate", limits=c(0,1.01))+
  opts(legend.position="none")
dev.off()

png("vaccination_coverage.png")
ggplot(ds[vaccinated==T], aes(x=country, fill=country, weight=weight))+
  geom_bar(color="black")+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))+
  opts(legend.position="none")
dev.off()

ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  for (j in levels(factor(ds$agegroup))) {
    ds[country==i & agegroup == j]$reweight <-
      1/nrow(ds[country==i & agegroup==j])
  }
}
png("vaccination_coverage_by_age.png")
ggplot(ds[vaccinated==T], aes(x=agegroup, fill=agegroup, weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+  
  facet_grid(.~country)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(),
       axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.title.x =
       theme_blank())+ 
  scale_fill_brewer(name="age group", palette="Set1")+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))
dev.off()

ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  for (j in levels(factor(ds$atrisk))) {
    ds[country==i & atrisk == j & agegroup %in% levels(agegroup)[1:3]]$reweight <-
      1/nrow(ds[country==i & atrisk == j & agegroup %in% levels(agegroup)[1:3]])
  }
}
png("vaccination_coverage_by_risk.png")
ggplot(ds[vaccinated==T & agegroup %in% levels(agegroup)[1:3]],
       aes(x=factor(atrisk), fill=factor(atrisk), weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+  
  facet_grid(.~country)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(),
       axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.title.x =
       theme_blank())+
  scale_fill_brewer(name="Risk group", palette="Set1", labels=c("no", "yes"))+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))
dev.off()

png("age_dist.png")
ggplot(ds, aes(x=country, fill=agegroup, weight=weight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(name="age group", palette="Set1")+
  scale_y_continuous("age distribution", limits=c(0,1.01))
dev.off()

vaccine_time <- data.frame()
for (country in levels(factor(ds$country))) {
  vaccine_country <- data.frame(week=as.character(levels(factor(compare$week))),
                                elderly=0, risk=0, all=0, country=country)
  for (i in 1:nrow(vaccine_country)) {
    vaccine_week <- compare[week <= vaccine_country[i,]$week]
    vaccine_country[i,]$all <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0]) /
      nrow(compare[week == vaccine_country[i,]$week & country == country])
    vaccine_country[i,]$elderly <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0 &
                   agegroup == levels(compare$agegroup)[4]]) / 
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   agegroup == levels(compare$agegroup)[4]])
    vaccine_country[i,]$risk <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0 & atrisk == 1]) /
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   atrisk == 1])
  }
  vaccine_time <- rbind(vaccine_time, vaccine_country)
}

ds$education <- ""
ds[no.education=="t"]$education <- "None"
ds[education.gcse=="t"]$education <- "Intermediate"
ds[education.alevels=="t"]$education <- "High school"
ds[education.bsc=="t"]$education <- "Bachelor"
ds[education.msc=="t"]$education <- "Higher"
ds[education.stillin=="t"]$education <- "Student"
ds$education <- factor(ds$education,
                       levels=levels(factor(ds$education))[c(1,3,2,4,5,6,7)])
ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  ds[country==i]$reweight <-
    1/nrow(ds[education!="" & country==i])
}

png("education_dist.png")
ggplot(ds[education!=""], aes(x=country, fill=education, weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("education distribution")
dev.off()

countries <- data.frame(country=levels(factor(ds$country)), aru = 0, arv = 0,
                        ar = 0, efficacy = 0)
for (i in 1:nrow(countries)) {
  countries[i,]$ar <-
    nrow(ds[country == countries[i,]$country & ili == T]) / 
    nrow(ds[country == countries[i,]$country])
  countries[i,]$aru <-
    nrow(ds[country == countries[i,]$country & ili == T & vaccinated == F]) /
    nrow(ds[country == countries[i,]$country & vaccinated == F])
  countries[i,]$arv <-
    nrow(ds[country == countries[i,]$country & ili == T & vaccinated == T]) /
    nrow(ds[country == countries[i,]$country & vaccinated == T])
  countries[i,]$efficacy <-
    (countries[i,]$aru - countries[i,]$arv) / countries[i,]$aru * 100
}


# HPA stuff
peak <- dt[country=="uk" & date > "2012-03-08" & date < "2012-03-23" & age > 17]
peak <- peak[postcode!=""]
peak$postcode <- tupper(peak$postcode)
#peak$area <- toupper(sub("^([A-Za-z]+).+$", "\\1", peak$postcode))
postcodes <- data.table(read.csv("../postcodes.csv", header=F, sep=","))
setnames(postcodes, "V1", "postcode")
setnames(postcodes, "V2", "region")
postcodes[region=="W99999999"]$region <- "Wales"
postcodes[region=="E12000001"]$region <- "North East England"
postcodes[region=="E12000002"]$region <- "North West England"
postcodes[region=="E12000003"]$region <- "Yorkshire and the Humber"
postcodes[region=="E12000004"]$region <- "East Midlands"
postcodes[region=="E12000005"]$region <- "West Midlands"
postcodes[region=="E12000006"]$region <- "East of England"
postcodes[region=="E12000007"]$region <- "London"
postcodes[region=="E12000008"]$region <- "South East England"
postcodes[region=="E12000009"]$region <- "South West England"
postcodes[region=="L99999999"]$region <- "Channel Islands"
postcodes[region=="N99999999"]$region <- "Northern Ireland"
postcodes[region=="S99999999"]$region <- "Scotland"

postcodes$postcode <- as.character(postcodes$postcode)
peak <- join(peak,postcodes, by='postcode')
peak$region <- factor(peak$region)

peak.users <- peak[!duplicated(peak$global.id.number)]

nrow(peak[gender==0])/nrow(peak[gender==1]) # 0.6723891
nrow(peak[age < 25])/nrow(peak) # 0.08041061
nrow(peak[age > 24 & age < 45])/nrow(peak) # 0.3541488
nrow(peak[age > 44 & age < 65])/nrow(peak) # 0.4153122
nrow(peak[age > 64])/nrow(peak) # 0.1467066




nrow(ds[country=="uk" & ili==T])/nrow(ds[country=="uk"])*100
nrow(ds[country=="uk" & ili==T & age < 20])/nrow(ds[country=="uk" & age < 20])*100
nrow(ds[country=="uk" & ili==T & age >= 20 & age < 45])/nrow(ds[country=="uk" & age >=20 & age < 45])*100
nrow(ds[country=="uk" & ili==T & age >= 45])/nrow(ds[country=="uk" & age >= 45])*100
nrow(ds[country=="uk" & ili==T & atrisk == 1])/nrow(ds[country=="uk" & atrisk == 1])*100

ds$vmsg <- with(dt2, aggregate(vmsg,
                               list(global.id.number=global.id.number),
                               sum))$x
ds$vm <- (ds$vmsg > 0)
nrow(ds[country=="uk" & ili==T & vm == 1])/nrow(ds[country=="uk"])*100

# active users

#dt$active <- (dt$nReports > 4 & 

# cohorts 
# exclude users with bad age
temp.data <- dt[!is.na(age)]
temp.data$agegroup <- factor(temp.data$agegroup)
temp.data <- temp.data[duplicated(dt$global.id.number)]
temp.data$newili <- temp.data$ili
temp.data$newili.notired <- temp.data$ili.notired
temp.data$newili.fever <- temp.data$ili.fever
temp.data[same==0, newili := 0]
temp.data[same==0, newili.notired := 0]
temp.data[same==0, newili.fever := 0]
levels(temp.data$agegroup) <- c("<18","18-44","45-64","65+")

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, temp.data$country,
            temp.data$newili, row.vars=rev(1:6))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","country","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_201112.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, temp.data$country,
            temp.data$newili.notired, row.vars=rev(1:6))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","country","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_notired_201112.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, temp.data$country,
            temp.data$newili.fever, row.vars=rev(1:6))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","country","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_fever_201112.raw", quote=F, row.names=F)

# GI stuff
dt$gi.or <- as.numeric(dt$diarrhoea == "t" | dt$vomiting == "t" | dt$nausea == "t")
dt$gi.and <- as.numeric(dt$diarrhoea == "t" & dt$vomiting == "t" & dt$nausea == "t")
dt$gi.or.novom <- as.numeric(dt$diarrhoea == "t" | dt$nausea == "t")
dt$gi.and.novom <- as.numeric(dt$diarrhoea == "t" & dt$nausea == "t")

dt$newgi.or <- dt$gi.or
dt$newgi.and <- dt$gi.and
dt$newgi.or.novom <- dt$gi.or.novom
dt$newgi.and.novom <- dt$gi.and.novom

dt[same==0, newgi.or := 0]
dt[same==0, newgi.and := 0]
dt[same==0, newgi.or.novom := 0]
dt[same==0, newgi.and.novom := 0]

r.or <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or,
            row.vars=1)
r.and <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and,
            row.vars=1)
r.or.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or.novom,
            row.vars=1)
r.and.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and.novom,
            row.vars=1)

gi.or.raw.data <- data.frame(expand.grid(rev(attr(r.or, "row.vars"))),
                                   unclass(r.or))
gi.and.raw.data <- data.frame(expand.grid(rev(attr(r.and, "row.vars"))),
                                   unclass(r.and))
gi.or.novom.raw.data <- data.frame(expand.grid(rev(attr(r.or.novom, "row.vars"))),
                                   unclass(r.or.novom))
gi.and.novom.raw.data <- data.frame(expand.grid(rev(attr(r.and.novom, "row.vars"))),
                                   unclass(r.and.novom))

names(gi.or.raw.data) <- c("Week", "nongi", "gi")
names(gi.and.raw.data) <- c("Week", "nongi", "gi")
names(gi.or.novom.raw.data) <- c("Week", "nongi", "gi")
names(gi.and.novom.raw.data) <- c("Week", "nongi", "gi")

gi.or.raw.data$gi.incidence <-
  gi.or.raw.data$gi / (gi.or.raw.data$nongi + gi.or.raw.data$nongi)
gi.and.raw.data$gi.incidence <-
  gi.and.raw.data$gi / (gi.and.raw.data$nongi + gi.and.raw.data$nongi)
gi.or.novom.raw.data$gi.incidence <-
  gi.or.novom.raw.data$gi / (gi.or.novom.raw.data$nongi + gi.or.novom.raw.data$nongi)
gi.and.novom.raw.data$gi.incidence <-
  gi.and.novom.raw.data$gi / (gi.and.novom.raw.data$nongi + gi.and.novom.raw.data$nongi)

gi.or.12 <- gi.or.raw.data[-c(1:3, 22:26),]
gi.and.12 <- gi.and.raw.data[-c(1:3, 22:26),]
gi.or.novom.12 <- gi.or.novom.raw.data[-c(1:3, 22:26),]
gi.and.novom.12 <- gi.and.novom.raw.data[-c(1:3, 22:26),]

write.csv(gi.or.12, "gi_or_201112.csv", quote=F, row.names=F)
write.csv(gi.and.12, "gi_and_201112.csv", quote=F, row.names=F)
write.csv(gi.or.novom.12, "gi_or_novom_201112.csv", quote=F, row.names=F)
write.csv(gi.and.novom.12, "gi_and_novom_201112.csv", quote=F, row.names=F)

# antibiotic use
