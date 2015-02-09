library(data.table)
library(ggplot2)
library(reshape)
library(maptools)
library(maps)
library(lubridate)

# read tables
sf12 <- read.csv('weekly_12.csv', sep=',', header=T)
bf12 <- read.csv('intake_12.csv', sep=',', header=T)
cf12 <- read.csv('contact_12.csv', sep=',', header=T)

translation <- data.frame(global_id = unique(bf12$global_id))
translation$number <- seq(1,nrow(translation))

bf12$global.id.number <- translation$number[match(bf12$global_id,
                                                translation$global_id)]
sf12$global.id.number <- translation$number[match(sf12$global_id,
                                                translation$global_id)]
cf12$global.id.number <- translation$number[match(cf12$global_id,
                                                translation$global_id)]

st12 <- data.table(sf12)
bt12 <- data.table(bf12)
ct12 <- data.table(cf12)

rm(sf12)
rm(bf12)
rm(cf12)

setnames(bt12, 1, "bid")
setnames(ct12, 1, "cid")

st12$date <- as.Date(st12$timestamp)
bt12$date <- as.Date(bt12$timestamp)
ct12$date <- as.Date(ct12$timestamp)

setkey(st12, global.id.number, date)
setkey(bt12, global.id.number, date)
setkey(ct12, global.id.number, date)

ctst <- ct12[st12, roll=TRUE, allow.cartesian=T]
ctst <- ctst[!duplicated(ctst)]

dt <- bt12[ctst, roll=TRUE, allow.cartesian=T]
dt <- dt[!duplicated(dt)]

rm(bt12)
rm(ct12)
rm(st12)

sf13 <- read.csv('weekly_13.csv', sep=',', header=T)
bf13 <- read.csv('intake_13.csv', sep=',', header=T)
cf13 <- read.csv('contact_13.csv', sep=',', header=T)

#bf13 <- bf13[,-c(10,101)]

## write.table(bf13[,-c(10,101)], 'intake_13.csv', sep=',', row.names=F, col.names=T, quote=F)
## write.table(cf13[,-39], 'contact_13.csv', sep=',', row.names=F, col.names=T, quote=F)

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

dt <- rbind(dt[, setdiff(names(dt), setdiff(names(dt), names(dt13))), with = FALSE],
            dt13[, setdiff(names(dt13), setdiff(names(dt13), names(dt))),
                 with = FALSE])

sf14 <- read.csv('weekly_14.csv', sep=',', header=T)
bf14 <- read.csv('intake_14.csv', sep=',', header=T)

## bf14 <- bf14[,-c(10,101)]

## write.table(bf14[,-c(10,101)], 'intake_14.csv', sep=',', row.names=F, col.names=T, quote=F)
## write.table(cf14[,-39], 'contact_14.csv', sep=',', row.names=F, col.names=T, quote=F)

translation14 <- data.frame(global_id = unique(bf14$global_id))
translation14$number <- seq(1,nrow(translation14))

bf14$global.id.number <- translation14$number[match(bf14$global_id,
                                                translation$global_id)]
sf14$global.id.number <- translation14$number[match(sf14$global_id,
                                                translation$global_id)]

st14 <- data.table(sf14)
bt14 <- data.table(bf14)

rm(sf14)
rm(bf14)

setnames(bt14, 1, "bid")

st14$date <- as.Date(st14$timestamp)
bt14$date <- as.Date(bt14$timestamp)

setkey(st14, global.id.number, date)
setkey(bt14, global.id.number, date)

dt14 <- bt14[st14, roll=TRUE, allow.cartesian=T]
dt14 <- dt14[!duplicated(dt14)]

rm(bt14)
rm(st14)

dt <- rbind(dt[, setdiff(names(dt), setdiff(names(dt), names(dt14))), with = FALSE],
            dt14[, setdiff(names(dt14), setdiff(names(dt14), names(dt))),
                 with = FALSE])

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
setnames(dt, "Q10.1", "alter.routine")
setnames(dt, "Q10c", "howlong.altered")
setnames(dt, "Q12_multi_row1_col1", "howmany.household.ili")
setnames(dt, "Q13_multi_row1_col1", "howmany.other.ili")


#dt$ili <-((dt$fever =="t" & dt$muscle.and.or.joint.pain =="t" &
#  (dt$symptoms.suddenly == 0 | dt$fever.suddenly == 0) &
#           (dt$cough == "t" | dt$sore.throat == "t" | dt$chest.pain =="t"))==T)
dt$ili <- ((dt$symptoms.suddenly == 0) &
           (dt$fever == "t" | dt$tired == "t" | dt$headache == "t" |
            dt$muscle.and.or.joint.pain =="t") &
                (dt$sore.throat == "t" | dt$cough =="t" | dt$shortness.breath =="t"))
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

freq <- data.table(aggregate(dt$global.id.number, by=list(dt$global.id.number), length))
setkey(freq, Group.1)
setkey(dt, global.id.number)
dt <- dt[freq]
setnames(dt, "x", "nReports")
dt$id <- seq(1,nrow(dt))
dt$symptoms.start <- as.Date(dt$symptoms.start, "%Y-%m-%d")
dt$symptoms.end <- as.Date(dt$symptoms.end, "%Y-%m-%d")
dt$week <- as.numeric(format(dt$date, format="%W"))
dt[dt$week==0]$week <- 52
dt$weight <- 1/hist(dt$week, freq=T, breaks=seq(0,52), plot=F)$counts[dt$week]
dt$birthdate <- as.Date(paste(dt$birthmonth, "-01", sep=""), "%Y-%m-%d")

# more variables to be used later
dt$norisk <- factor(dt$norisk)
dt$atrisk <- dt$norisk
levels(dt$atrisk) <- c(1,0)
dt$atrisk <- as.numeric(paste(dt$atrisk))
dt$age <-  0
dt[, age := as.period(date - birthdate)$year]

dt$agegroup <- cut(dt$age, breaks=c(0,18,45,65, max(dt$age, na.rm=T)),
                   include.lowest=T, right=F)
dt[, vaccine.date := as.Date(as.character(date.vaccine), format = "%Y-%m-%d")]
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
write.csv(vaccination.raw.data, "cohorts_201114.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.notired, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_notired_201114.raw", quote=F, row.names=F)

r <- ftable(temp.data$vaccine, temp.data$atrisk, temp.data$children,
            temp.data$agegroup, temp.data$week, 
            temp.data$ili.fever, row.vars=rev(1:5))
vaccination.raw.data <- data.frame(expand.grid(rev(attr(r, "row.vars"))),
                                   unclass(r))
names(vaccination.raw.data) <- c("vaccinated","risk","children","agegroup","year-week","non_ili","ili")
write.csv(vaccination.raw.data, "cohorts_fever_201114.raw", quote=F, row.names=F)

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
