library(data.table)
library(ggplot2)
library(reshape)

sf <- read.csv('epidb_weekly.csv', sep=',', header=T)
bf <- read.csv('epidb_intake.csv', sep=',', header=T)

translation <- data.frame(global_id = unique(bf$global_id))
translation$number <- seq(1,nrow(translation))

levels(sf$global_id) <- levels(bf$global_id)

bf$global_id_number <- translation$number[match(bf$global_id,
                                                translation$global_id)]
sf$global_id_number <- translation$number[match(sf$global_id,
                                                translation$global_id)]

st <- data.table(sf)
bt <- data.table(bf)

rm(sf)
rm(bf)

setnames(bt, 2, "bid")

st$date <- as.Date(st$timestamp)
bt$date <- as.Date(bt$timestamp)

setkey(st, global_id_number, date)
setkey(bt, global_id_number, date)

dt <- bt[st, roll=TRUE]

rm(bt)
rm(st)

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
setnames(dt, "Q6_0", "household.0-4")
setnames(dt, "Q6_0_open", "nb.household.0-4")
setnames(dt, "Q6_1", "household.5-18")
setnames(dt, "Q6_1_open", "nb.household.5-18")
setnames(dt, "Q6_2", "household.19-44")
setnames(dt, "Q6_2_open", "nb.household.19-44")
setnames(dt, "Q6_3", "household.45-64")
setnames(dt, "Q6_3_open", "nb.household.45-64")
setnames(dt, "Q6_4", "household.65+")
setnames(dt, "Q6_4_open", "nb.household.65+")
setnames(dt, "Q7", "transport")
setnames(dt, "Q7b", "howlong.transport")
setnames(dt, "Q9", "vaccine.last.year")
setnames(dt, "Q10", "vaccine.this.year")
setnames(dt, "Q10b", "date.vaccine")
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
setnames(dt, "Q10.1", "alter.routine")
setnames(dt, "Q10c", "howlong.altered")
setnames(dt, "Q12_multi_row1_col1", "howmany.household.ili")
setnames(dt, "Q13_multi_row1_col1", "howmany.other.ili")

## dt$ili <-((dt$fever =="t" & dt$muscle.and.or.joint.pain =="t" &
##   (dt$symptoms.suddenly == 0 | dt$fever.suddenly == 0) &
##            (dt$cough == "t" | dt$sore.throat == "t" | dt$chest.pain =="t"))==T)
dt$ili <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t" | dt$tired == "t" | dt$headache == "t" |
            dt$muscle.and.or.joint.pain =="t") &
           (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath =="t"))
freq <- data.table(aggregate(dt$global_id_number, by=list(dt$global_id_number), length))
setkey(freq, Group.1)
dt <- dt[freq]
setnames(dt, "x", "nReports")
dt$id <- seq(1,nrow(dt))
dt$symptoms.start <- as.Date(dt$symptoms.start, "%Y-%m-%d")
dt$week <- as.numeric(format(dt$date, format="%W"))
dt[dt$week==0]$week <- 52
dt$weight <- 1/hist(dt$week, freq=T, breaks=seq(0,52))$counts[dt$week]


#dt2 <- dt[dt$nReports>1 & !is.na(dt$ili)]
dt2 <- dt[duplicated(dt$global_id_number)]
dt2$ili <- as.numeric(dt2$ili)
dt2$week <- format(dt2$date, format="%G-%W")
dt2 <- dt2[!is.na(dt2$week)]
dt2[dt2$week=="2011-00"]$week <- "2011-52"
dt2$postcode <- toupper(as.character(dt2$postcode))
dt2$country <- tolower(dt2$country)
dt2$birthdate <- as.Date(dt2$birthmonth, "%Y/%M/%d")

## compare <- dt2[date > "2012-02-19" & date < "2012-03-05" & !is.na(ili) &
##                !is.na(birthdate) & birthdate < as.Date("2012-03-01"),]
compare <- dt2[!is.na(ili) & !is.na(birthdate) & birthdate < as.Date("2012-04-01"),]
#compare$global_id <-  factor(compare$global_id)
compare$norisk <- factor(compare$norisk)
compare$atrisk <- compare$norisk
levels(compare$atrisk) <- c(1,0)
compare$atrisk <- as.numeric(paste(compare$atrisk))

ds <- compare[!duplicated(compare$global_id_number)]

age_years <- function(from, to)
{
     lt <- as.POSIXlt(c(from, to))
     age <- lt$year[2] - lt$year[1]
     mons <- lt$mon + lt$mday/50
     if(mons[2] < mons[1]) age <- age -1
     age
}

ds$age <-  0
ds$ili <- FALSE

ds$nbili <- with(compare, aggregate(ili,
                                    list(global_id_number=global_id_number),
                                    sum))$x
ds$ili <- (ds$nbili > 0)
ds$ili <- (ds$nbili > 0)
ds$age <- apply(ds, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                    as.Date("2012-04-01"))})

pdf("attack_rate.pdf")
ggplot(ds[ili==T], aes(x=country, fill=country, weight=weight))+
  geom_bar()+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("attack rate", limits=c(0,1))+
  opts(legend.position="none")
dev.off()

pdf("vaccination_coverage.pdf")
ggplot(ds[vaccine.this.year==T], aes(x=country, fill=country, weight=weight))+
  geom_bar()+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("vaccination coverage", limits=c(0,1))+
  opts(legend.position="none")
dev.off()

pdf("age_dist.pdf")
ggplot(ds, aes(x=country, fill=agegroups, weight=weight))+
  geom_bar()+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("age distribution", limits=c(0,1))
dev.off()

ds$education <- ""
is.na(ds$education) <- T
ds[no.education=="t"]$education <- "None"
ds[education.gcse=="t"]$education <- "Intermediate"
ds[education.alevels=="t"]$education <- "High school"
ds[education.bsc=="t"]$education <- "Bachelor"
ds[education.msc=="t"]$education <- "Higher"
ds[education.stillin=="t"]$education <- "Student"
ds$education <- factor(ds$education, levels=levels(ds$education)[c(1,3,2,4,5,6)])

pdf("education_dist.pdf")
ggplot(ds[!is.na(education)], aes(x=country, fill=education,
  weight=reweight))+geom_bar()+theme_bw(20)+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank(),
  title)+scale_fill_brewer(palette="Set1")+scale_y_continuous("education distribution")
dev.off()

ds$nonili <- 1-ds$ili
ds$smoking <- ds$smoke %in% c(1,2,3)
ds$vaccinated <- (ds$vaccine.this.year == 0)

m <- melt(ds, id.vars=c("agegroups", "vaccinated", "smoking",
  "gender"), measure.vars=c("ili", "nonili"))
data <- cast(m, agegroups+gender+smoking+vaccinated~variable, sum)
attach(data)
fluglm <- glm(cbind(ili, nonili) ~ agegroups+gender+smoking+vaccinated,
              family=binomial)
summary(fluglm)

swant <- names(dt2)[c(143:146,148,149,151:153,158,160,161)]
vc <- rep(0.0, 11)
vn <- rep(0.0, 11)

j <- 0
for (i in swant[-1]) {
  j <- j+1
  vn[j] <-
    sum(dt3[dt3$no.symptoms==0,i,with=F])/nrow(dt2[dt3$no.symptoms==0,i,with=F])
  vc[j] <-
    sum(dt3[dt3$alter.routine==1,i,with=F])/nrow(dt3[dt3$alter.routine==1,i,with=F])
}

sdc <- data.frame(symptom=rep(swant[-1]), status="change", fraction=vc)
sdn <- data.frame(symptom=rep(swant[-1]), status="no change", fraction=vn)
sd <- rbind(sdc, sdn)
sd$season <- "2011-12"

dta <- dt[!is.na(dt$alter.routine)]

round(sort(table(apply(dta[dta$visit.medical.service.gp=='t'],1,function(x) {
  paste(names(dta)[which(x[144:161]=="t")+144], sep=".", collapse=" + ")})))*100/
  sum(sort(table(apply(dta[dta$visit.medical.service.gp=='t'],1,function(x) {
  paste(names(dta)[which(x[144:161]=="t")+144], sep=".",
  collapse=" + ")})))),1)



#postcodes <- readShapePoly("~/Research/FluSurvey/Shapefiles/uk_convertd4")
#names(postcodes)[1] <- "names"

plot.week <- function(x, color=2)
{
  pc1 <- unique(dt2[dt2$week==x & dt2$ili==1]$postcode)
  pc1 <- as.character(pc1, na.rm=T)
  pc1 <- pc1[!is.na(pc1) & pc1 != "NULL" & pc1 != ""]
  col <- rep(color,length(pc1))
  match <- match.map(postcodes, pc1)
  color <- col[match]
#  png(paste(x, ".png", sep=""))
  plot(postcodes, border="white", col=color)
#  title(main=x)
#  dev.off()
}

