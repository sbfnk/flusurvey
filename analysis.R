library(data.table)
library(ggplot2)
library(reshape)
library(maptools)
library(maps)

# compute the age in years from a birthdate (from) and the current date (to)
age_years <- function(from, to)
{
     lt <- as.POSIXlt(c(from, to))
     age <- lt$year[2] - lt$year[1]
     mons <- lt$mon + lt$mday/50
     if(mons[2] < mons[1]) age <- age -1
     age
}

# read tables
sf <- read.csv('weekly.csv', sep=',', header=T)
bf <- read.csv('intake.csv', sep=',', header=T)
cf <- read.csv('contact.csv', sep=',', header=T)

#write.table(cf[,-6], 'contact.csv', sep=',', row.names=F, col.names=T, quote=F)

translation <- data.frame(global_id = unique(bf$global_id))
translation$number <- seq(1,nrow(translation))

bf$global.id.number <- translation$number[match(bf$global_id,
                                                translation$global_id)]
sf$global.id.number <- translation$number[match(sf$global_id,
                                                translation$global_id)]
cf$global.id.number <- translation$number[match(cf$global_id,
                                                translation$global_id)]

st <- data.table(sf)
bt <- data.table(bf)
ct <- data.table(cf)

rm(sf)
rm(bf)
rm(cf)

setnames(bt, 1, "bid")
setnames(ct, 1, "cid")

st$date <- as.Date(st$timestamp)
bt$date <- as.Date(bt$timestamp)
ct$date <- as.Date(ct$timestamp)

setkey(st, global.id.number, date)
setkey(bt, global.id.number, date)
setkey(ct, global.id.number, date)

ctst <- ct[st, roll=TRUE, allow.cartesian=T]
ctst <- ctst[!duplicated(ctst)]

dt <- bt[ctst, roll=TRUE, allow.cartesian=T]
dt <- dt[!duplicated(dt)]

rm(bt)
rm(ct)
rm(st)

sf13 <- read.csv('weekly_13.csv', sep=',', header=T)
bf13 <- read.csv('intake_13.csv', sep=',', header=T)
cf13 <- read.csv('contact_13.csv', sep=',', header=T)

bf13 <- bf13[,-c(10,101)]

write.table(bf13[,-c(10,101)], 'intake_13.csv', sep=',', row.names=F, col.names=T, quote=F)
write.table(cf13[,-39], 'contact_13.csv', sep=',', row.names=F, col.names=T, quote=F)

translation13 <- data.frame(global_id = unique(bf13$global_id))
translation13$number <- seq(1,nrow(translation13))

bf13$global.id.number <- translation13$number[match(bf13$global_id,
                                                translation$global_id)]
sf13$global.id.number <- translation13$number[match(sf13$global_id,
                                                translation$global_id)]
cf13$global.id.number <- translation13$number[match(cf13$global_id,
                                                translation$global_id)]

st13 <- data.table(sf13)
bt13 <- data.table(bf13)
ct13 <- data.table(cf13)

rm(sf13)
rm(bf13)
rm(cf13)

setnames(bt13, 1, "bid")
setnames(ct13, 1, "cid")

st13$date <- as.Date(st13$timestamp)
bt13$date <- as.Date(bt13$timestamp)
ct13$date <- as.Date(ct13$timestamp)

setkey(st13, global.id.number, date)
setkey(bt13, global.id.number, date)
setkey(ct13, global.id.number, date)

ctst13 <- ct13[st13, roll=TRUE, allow.cartesian=T]
ctst13 <- ctst13[!duplicated(ctst13)]

dt13 <- bt13[ctst13, roll=TRUE, allow.cartesian=T]
dt13 <- dt13[!duplicated(dt13)]

rm(bt13)
rm(ct13)
rm(st13)

dt <- rbind(dt, dt13)

setnames(dt, 8, "self")
setnames(dt, 10, "gender")
setnames(dt, 11, "birthmonth")
setnames(dt, 12, "postcode")
setnames(dt, 16, "occupation")
setnames(dt, 17, "no.education")
setnames(dt, 18, "education.gcse")
setnames(dt, 19, "education.alevels")
setnames(dt, 20, "education.bsc")
setnames(dt, 21, "education.msc")
setnames(dt, 22, "education.stillin")
setnames(dt, 23, "frequent.contact.children")
setnames(dt, 24, "frequent.contact.elderly")
setnames(dt, 25, "frequent.contact.patients")
setnames(dt, 26, "frequent.contact.people")
setnames(dt, 29, "nb.household.0-4")
setnames(dt, 31, "nb.household.5-18")
setnames(dt, 33, "nb.household.19-64")
setnames(dt, 35, "nb.household.65+")
setnames(dt, 39, "transport")
setnames(dt, 40, "howlong.transport")
setnames(dt, 42, "vaccine.last.year")
setnames(dt, 43, "vaccine.this.year")
setnames(dt, 45, "date.vaccine")
setnames(dt, 46, "why.vaccine.riskgroup")
setnames(dt, 47, "why.vaccine.protected")
setnames(dt, 48, "why.vaccine.protect.others")
setnames(dt, 49, "why.vaccine.doctor")
setnames(dt, 50, "why.vaccine.work.recommended")
setnames(dt, 51, "why.vaccine.convenient")
setnames(dt, 52, "why.vaccine.free")
setnames(dt, 53, "why.vaccine.nomiss.work")
setnames(dt, 54, "why.vaccine.always")
setnames(dt, 55, "why.vaccine.other")
setnames(dt, 56, "why.not.vaccine.notyet")
setnames(dt, 57, "why.not.vaccine.notoffered")
setnames(dt, 58, "why.not.vaccine.norisk")
setnames(dt, 59, "why.not.vaccine.natural")
setnames(dt, 60, "why.not.vaccine.noteffective")
setnames(dt, 61, "why.not.vaccine.minor")
setnames(dt, 62, "why.not.vaccine.unlikely")
setnames(dt, 63, "why.not.vaccine.cause")
setnames(dt, 64, "why.not.vaccine.side.effects")
setnames(dt, 65, "why.not.vaccine.dont.like")
setnames(dt, 66, "why.not.vaccine.unavailable")
setnames(dt, 67, "why.not.vaccine.not.free")
setnames(dt, 68, "why.not.vaccine.no.reason")
setnames(dt, 69, "why.not.vaccine.doctor")
setnames(dt, 70, "why.not.vaccine.other")
setnames(dt, 71, "norisk")
setnames(dt, 72, "risk.asthma")
setnames(dt, 73, "risk.diabetes")
setnames(dt, 74, "risk.lung")
setnames(dt, 75, "risk.heart")
setnames(dt, 76, "risk.kidney")
setnames(dt, 77, "risk.immune")
setnames(dt, 78, "pregnant")
setnames(dt, 80, "smoke")
setnames(dt, 81, "allergy.hayfever")
setnames(dt, 82, "allergy.dust")
setnames(dt, 83, "allergy.animals")
setnames(dt, 84, "allergy.other")
setnames(dt, 107, "conversational.home.0-4")
setnames(dt, 108, "conversational.home.5-18")
setnames(dt, 109, "conversational.home.19-44")
setnames(dt, 110, "conversational.home.45-64")
setnames(dt, 111, "conversational.home.65+")
setnames(dt, 112, "conversational.work.0-4")
setnames(dt, 113, "conversational.work.5-18")
setnames(dt, 114, "conversational.work.19-44")
setnames(dt, 115, "conversational.work.45-64")
setnames(dt, 116, "conversational.work.65+")
setnames(dt, 117, "conversational.other.0-4")
setnames(dt, 118, "conversational.other.5-18")
setnames(dt, 119, "conversational.other.19-44")
setnames(dt, 120, "conversational.other.45-64")
setnames(dt, 121, "conversational.other.65+")
setnames(dt, 122, "physical.home.0-4")
setnames(dt, 123, "physical.home.5-18")
setnames(dt, 124, "physical.home.19-44")
setnames(dt, 125, "physical.home.45-64")
setnames(dt, 126, "physical.home.65+")
setnames(dt, 127, "physical.work.0-4")
setnames(dt, 128, "physical.work.5-18")
setnames(dt, 129, "physical.work.19-44")
setnames(dt, 130, "physical.work.45-64")
setnames(dt, 131, "physical.work.65+")
setnames(dt, 132, "physical.other.0-4")
setnames(dt, 133, "physical.other.5-18")
setnames(dt, 134, "physical.other.19-44")
setnames(dt, 135, "physical.other.45-64")
setnames(dt, 136, "physical.other.65+")
setnames(dt, 137, "public.transport")
setnames(dt, 138, "enclosed.indoor.space")
setnames(dt, 139, "furthest.travelled")
setnames(dt, 146, "no.symptoms")
setnames(dt, 147, "fever")
setnames(dt, 148, "chills")
setnames(dt, 149, "blocked.runny.nose")
setnames(dt, 150, "sneezing")
setnames(dt, 151, "sore.throat")
setnames(dt, 152, "cough")
setnames(dt, 153, "shortness.breath")
setnames(dt, 154, "headache")
setnames(dt, 155, "muscle.and.or.joint.pain")
setnames(dt, 156, "chest.pain")
setnames(dt, 157, "tired")
setnames(dt, 158, "loss.appetite")
setnames(dt, 159, "phlegm")
setnames(dt, 160, "watery.eyes")
setnames(dt, 161, "nausea")
setnames(dt, 162, "vomiting")
setnames(dt, 163, "diarrhoea")
setnames(dt, 164, "stomach.ache")
setnames(dt, 165, "other")
setnames(dt, 166, "stillill")
setnames(dt, 169, "symptoms.start")
setnames(dt, 170, "symptoms.end")
setnames(dt, 171, "symptoms.end.when")
setnames(dt, 172, "symptoms.suddenly")
setnames(dt, 174, "fever.start")
setnames(dt, 175, "fever.suddenly")
setnames(dt, 178, "visit.medical.service.no")
setnames(dt, 179, "visit.medical.service.gp")
setnames(dt, 180, "visit.medical.service.ae")
setnames(dt, 181, "visit.medical.service.hospital")
setnames(dt, 182, "visit.medical.service.other")
setnames(dt, 183, "visit.medical.service.appointment")
setnames(dt, 184, "visit.medical.service.howsoon")
setnames(dt, 200, "alter.routine")
setnames(dt, 202, "howlong.altered")
setnames(dt, 204, "howmany.household.ili")
setnames(dt, 205, "howmany.other.ili")

#dt$ili <-((dt$fever =="t" & dt$muscle.and.or.joint.pain =="t" &
#  (dt$symptoms.suddenly == 0 | dt$fever.suddenly == 0) &
#           (dt$cough == "t" | dt$sore.throat == "t" | dt$chest.pain =="t"))==T)
dt$ili <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t" | dt$tired == "t" | dt$headache == "t" |
            dt$muscle.and.or.joint.pain =="t") &
           (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath =="t"))
freq <- data.table(aggregate(dt$global.id.number, by=list(dt$global.id.number), length))
setkey(freq, Group.1)
dt <- dt[freq]
setnames(dt, "x", "nReports")
dt$id <- seq(1,nrow(dt))
dt$symptoms.start <- as.Date(dt$symptoms.start, "%Y-%m-%d")
dt$symptoms.end <- as.Date(dt$symptoms.end, "%Y-%m-%d")
dt$week <- as.numeric(format(dt$date, format="%W"))
dt[dt$week==0]$week <- 52
dt$weight <- 1/hist(dt$week, freq=T, breaks=seq(0,52), plot=F)$counts[dt$week]
dt$birthdate <- as.Date(paste(dt$birthmonth, "-01", sep=""), "%Y-%m-%d")

#dt2 <- dt[dt$nReports>1 & !is.na(dt$ili)]
dt2 <- dt[duplicated(dt$global.id.number)]
dt2$ili <- as.numeric(dt2$ili)
dt2$week <- format(dt2$date, format="%G-%W")
dt2 <- dt2[!is.na(dt2$week)]
dt2[dt2$week=="2011-00"]$week <- "2011-52"
dt2$postcode <- toupper(as.character(dt2$postcode))

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

weekly.incidence <- function(x, variable, range=c(), weeks=c())
{

  d <- split(x, x[,which(names(x)==variable),with=F])
  if (length(range)>0) {d <- d[range]}
  df <- c()

  for (i in 1:length(d)) {
    a <- data.table(d[[i]])
    bins <- cut(a[ili==T & symptoms.start > "2011-11-02" &
                  symptoms.start < "2012-04-01"]$symptoms.start,
                breaks="week")
    weights <- 1/hist(a$week, freq=T, breaks=seq(0,52))$counts[a[ili==T &
                                        symptoms.start > "2011-11-02" & 
                                        symptoms.start < "2012-04-01"]$week]
    wsums <- tapply(weights, bins, sum)
    binlevels <- levels(bins)
    if (length(weeks)>0) {
      wsums <- wsums[weeks]
      binlevels <- binlevels[weeks]
    }
      
    df[[i]] <- data.frame(incidence=wsums, variable=names(d)[i], week=binlevels)
  }

  weekly.incidence <- df[[1]]
  for (i in 2:length(d)) {
    weekly.incidence <- rbind(weekly.incidence, df[[i]])
  }

  names(weekly.incidence)[2] <- variable
  weekly.incidence$week <- as.Date(weekly.incidence$week)
  weekly.incidence
}

plot.binary <- function(x, name, yes=c(0), no=c(1), cname="test", weeks=c())
{
  x$compare <- NA
  for (i in yes) {
    x$compare[x[,which(names(x)==name),with=F] == i] <- "yes"
  }
  for (i in no) {
    x$compare[x[,which(names(x)==name),with=F] == i] <- "no"
  }
  ggplot(weekly.incidence(x, "compare", 1:2, weeks=weeks),
         aes(x=week, y=incidence, color=compare))+
           geom_line(lwd=1.5)+scale_color_discrete(name=cname)
}

  
  for (i in 1:
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
