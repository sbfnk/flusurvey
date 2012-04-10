library(data.table)
library(maptools)
library(maps)

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

setnames(dt, 3, "country")
setnames(dt, 6, "self")
setnames(dt, 8, "gender")
setnames(dt, 9, "birthmonth")
setnames(dt, 10, "postcode")
setnames(dt, 11, "occupation")
setnames(dt, 12, "work.postcode")
setnames(dt, 15, "frequent.contact.children")
setnames(dt, 16, "frequent.contact.elderly")
setnames(dt, 17, "frequent.contact.patients")
setnames(dt, 18, "frequent.contact.people")
setnames(dt, 21, "nb.household.0-4")
setnames(dt, 23, "nb.household.5-18")
setnames(dt, 25, "nb.household.19-44")
setnames(dt, 27, "nb.household.45-64")
setnames(dt, 29, "nb.household.65+")
setnames(dt, 32, "howlong.transport")
setnames(dt, 34, "vaccine.last.year")
setnames(dt, 35, "vaccine.this.year")
setnames(dt, 37, "date.vaccine")
setnames(dt, 38, "why.vaccine.riskgroup")
setnames(dt, 39, "why.vaccine.protected")
setnames(dt, 40, "why.vaccine.protect.others")
setnames(dt, 41, "why.vaccine.doctor")
setnames(dt, 42, "why.vaccine.work.recommended")
setnames(dt, 43, "why.vaccine.convenient")
setnames(dt, 44, "why.vaccine.free")
setnames(dt, 45, "why.vaccine.nomiss.work")
setnames(dt, 46, "why.vaccine.always")
setnames(dt, 47, "why.vaccine.other")
setnames(dt, 48, "why.not.vaccine.notyet")
setnames(dt, 49, "why.not.vaccine.notoffered")
setnames(dt, 50, "why.not.vaccine.norisk")
setnames(dt, 51, "why.not.vaccine.natural")
setnames(dt, 52, "why.not.vaccine.noteffective")
setnames(dt, 53, "why.not.vaccine.minor")
setnames(dt, 54, "why.not.vaccine.unlikely")
setnames(dt, 55, "why.not.vaccine.cause")
setnames(dt, 56, "why.not.vaccine.side.effects")
setnames(dt, 57, "why.not.vaccine.dont.like")
setnames(dt, 58, "why.not.vaccine.unavailable")
setnames(dt, 59, "why.not.vaccine.not.free")
setnames(dt, 60, "why.not.vaccine.no.reason")
setnames(dt, 61, "why.not.vaccine.doctor")
setnames(dt, 62, "why.not.vaccine.other")
setnames(dt, 63, "norisk")
setnames(dt, 64, "risk.asthma")
setnames(dt, 65, "risk.diabetes")
setnames(dt, 66, "risk.lung")
setnames(dt, 67, "risk.heart")
setnames(dt, 68, "risk.kidney")
setnames(dt, 69, "risk.immune")
setnames(dt, 70, "pregnant")
setnames(dt, 72, "smoke")
setnames(dt, 73, "allergy.hayfever")
setnames(dt, 74, "allergy.dust")
setnames(dt, 75, "allergy.animals")
setnames(dt, 76, "allergy.other")
setnames(dt, 77, "allergy.none")
setnames(dt, 97, "no.symptoms")
setnames(dt, 98, "fever")
setnames(dt, 99, "chills")
setnames(dt, 100, "blocked.runny.nose")
setnames(dt, 101, "sneezing")
setnames(dt, 102, "sore.throat")
setnames(dt, 103, "cough")
setnames(dt, 104, "shortness.breath")
setnames(dt, 105, "headache")
setnames(dt, 106, "muscle.and.or.joint.pain")
setnames(dt, 107, "chest.pain")
setnames(dt, 108, "tired")
setnames(dt, 109, "loss.appetite")
setnames(dt, 110, "phlegm")
setnames(dt, 111, "watery.eyes")
setnames(dt, 112, "nausea")
setnames(dt, 113, "vomiting")
setnames(dt, 114, "diarrhoea")
setnames(dt, 115, "stomach.ache")
setnames(dt, 116, "other")
setnames(dt, 117, "same")
setnames(dt, 120, "symptoms.start.date")
setnames(dt, 121, "symptoms.end")
setnames(dt, 122, "symptoms.end.date")
setnames(dt, 123, "symptoms.suddenly")
setnames(dt, 125, "fever.start")
setnames(dt, 126, "fever.suddenly")
setnames(dt, 129, "visit.medical.service.no")
setnames(dt, 130, "visit.medical.service.gp")
setnames(dt, 131, "visit.medical.service.ae")
setnames(dt, 132, "visit.medical.service.hospital")
setnames(dt, 133, "visit.medical.service.other")
setnames(dt, 134, "visit.medical.service.appointment")
setnames(dt, 135, "visit.medical.service.howsoon")
setnames(dt, 151, "alter.routine")
setnames(dt, 153, "howlong.altered")
setnames(dt, 155, "howmany.household.ili")
setnames(dt, 156, "howmany.other.ili")

dt$ili <-((dt$fever =="t" & dt$muscle.and.or.joint.pain =="t" &
  (dt$symptoms.suddenly == 0 | dt$fever.suddenly == 0) &
           (dt$cough == "t" | dt$sore.throat == "t" | dt$chest.pain =="t"))==T)
freq <- data.table(aggregate(dt$global_id_number, by=list(dt$global_id_number), length))
setkey(freq, Group.1)
dt <- dt[freq]
setnames(dt, "x", "nReports")

#dt2 <- dt[dt$nReports>1 & !is.na(dt$ili)]
dt2 <- dt[duplicated(dt$user)]
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
