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
ct <- data.table(cf)

rm(sf)
rm(bf)
rm(cf)

setnames(bt, 1, "bid")
setnames(ct, 1, "cid")

st$date <- as.Date(st$timestamp)
bt$date <- as.Date(bt$timestamp)
ct$date <- as.Date(ct$timestamp)

setkey(st, global_id_number, date)
setkey(bt, global_id_number, date)
setkey(ct, global_id_number, date)

dt <- bt[ct[st, roll=TRUE], roll=TRUE]

rm(bt)
rm(ct)
rm(st)

setnames(dt, 7, "self")
setnames(dt, 8, "gender")
setnames(dt, 10, "birthmonth")
setnames(dt, 11, "postcode")
setnames(dt, 15, "occupation")
setnames(dt, 16, "education")
setnames(dt, 22, "frequent.contact.children")
setnames(dt, 23, "frequent.contact.elderly")
setnames(dt, 24, "frequent.contact.patients")
setnames(dt, 25, "frequent.contact.people")
setnames(dt, 28, "nb.household.0-4")
setnames(dt, 30, "nb.household.5-18")
setnames(dt, 32, "nb.household.19-64")
setnames(dt, 34, "nb.household.65+")
setnames(dt, 38, "transport")
setnames(dt, 39, "howlong.transport")
setnames(dt, 41, "vaccine.last.year")
setnames(dt, 42, "vaccine.this.year")
setnames(dt, 44, "date.vaccine")
setnames(dt, 45, "why.vaccine.riskgroup")
setnames(dt, 46, "why.vaccine.protected")
setnames(dt, 47, "why.vaccine.protect.others")
setnames(dt, 48, "why.vaccine.doctor")
setnames(dt, 49, "why.vaccine.work.recommended")
setnames(dt, 50, "why.vaccine.convenient")
setnames(dt, 51, "why.vaccine.free")
setnames(dt, 52, "why.vaccine.nomiss.work")
setnames(dt, 53, "why.vaccine.always")
setnames(dt, 54, "why.vaccine.other")
setnames(dt, 55, "why.not.vaccine.notyet")
setnames(dt, 56, "why.not.vaccine.notoffered")
setnames(dt, 57, "why.not.vaccine.norisk")
setnames(dt, 58, "why.not.vaccine.natural")
setnames(dt, 59, "why.not.vaccine.noteffective")
setnames(dt, 60, "why.not.vaccine.minor")
setnames(dt, 61, "why.not.vaccine.unlikely")
setnames(dt, 62, "why.not.vaccine.cause")
setnames(dt, 63, "why.not.vaccine.side.effects")
setnames(dt, 64, "why.not.vaccine.dont.like")
setnames(dt, 65, "why.not.vaccine.unavailable")
setnames(dt, 66, "why.not.vaccine.not.free")
setnames(dt, 67, "why.not.vaccine.no.reason")
setnames(dt, 68, "why.not.vaccine.doctor")
setnames(dt, 69, "why.not.vaccine.other")
setnames(dt, 70, "norisk")
setnames(dt, 71, "risk.asthma")
setnames(dt, 72, "risk.diabetes")
setnames(dt, 73, "risk.lung")
setnames(dt, 74, "risk.heart")
setnames(dt, 75, "risk.kidney")
setnames(dt, 76, "risk.immune")
setnames(dt, 77, "pregnant")
setnames(dt, 79, "smoke")
setnames(dt, 80, "allergy.hayfever")
setnames(dt, 81, "allergy.dust")
setnames(dt, 82, "allergy.animals")
setnames(dt, 83, "allergy.other")
setnames(dt, 106, "conversational.home.0-4")
setnames(dt, 107, "conversational.home.5-18")
setnames(dt, 108, "conversational.home.19-44")
setnames(dt, 109, "conversational.home.45-64")
setnames(dt, 110, "conversational.home.65+")
setnames(dt, 111, "conversational.work.0-4")
setnames(dt, 112, "conversational.work.5-18")
setnames(dt, 113, "conversational.work.19-44")
setnames(dt, 114, "conversational.work.45-64")
setnames(dt, 115, "conversational.work.65+")
setnames(dt, 116, "conversational.other.0-4")
setnames(dt, 117, "conversational.other.5-18")
setnames(dt, 118, "conversational.other.19-44")
setnames(dt, 119, "conversational.other.45-64")
setnames(dt, 120, "conversational.other.65+")
setnames(dt, 121, "physical.home.0-4")
setnames(dt, 122, "physical.home.5-18")
setnames(dt, 123, "physical.home.19-44")
setnames(dt, 124, "physical.home.45-64")
setnames(dt, 125, "physical.home.65+")
setnames(dt, 126, "physical.work.0-4")
setnames(dt, 127, "physical.work.5-18")
setnames(dt, 128, "physical.work.19-44")
setnames(dt, 129, "physical.work.45-64")
setnames(dt, 130, "physical.work.65+")
setnames(dt, 131, "physical.other.0-4")
setnames(dt, 132, "physical.other.5-18")
setnames(dt, 133, "physical.other.19-44")
setnames(dt, 134, "physical.other.45-64")
setnames(dt, 135, "physical.other.65+")
setnames(dt, 136, "public.transport")
setnames(dt, 137, "enclosed.indoor.space")
setnames(dt, 138, "furthest.travelled")
setnames(dt, 143, "no.symptoms")
setnames(dt, 144, "fever")
setnames(dt, 145, "chills")
setnames(dt, 146, "blocked.runny.nose")
setnames(dt, 147, "sneezing")
setnames(dt, 148, "sore.throat")
setnames(dt, 149, "cough")
setnames(dt, 150, "shortness.breath")
setnames(dt, 151, "headache")
setnames(dt, 152, "muscle.and.or.joint.pain")
setnames(dt, 153, "chest.pain")
setnames(dt, 154, "tired")
setnames(dt, 155, "loss.appetite")
setnames(dt, 156, "phlegm")
setnames(dt, 157, "watery.eyes")
setnames(dt, 158, "nausea")
setnames(dt, 159, "vomiting")
setnames(dt, 160, "diarrhoea")
setnames(dt, 161, "stomach.ache")
setnames(dt, 162, "other")
setnames(dt, 163, "stillill")
setnames(dt, 166, "symptoms.start")
setnames(dt, 168, "symptoms.end")
setnames(dt, 169, "symptoms.suddenly")
setnames(dt, 171, "fever.start")
setnames(dt, 172, "fever.suddenly")
setnames(dt, 175, "visit.medical.service.no")
setnames(dt, 176, "visit.medical.service.gp")
setnames(dt, 177, "visit.medical.service.ae")
setnames(dt, 178, "visit.medical.service.hospital")
setnames(dt, 179, "visit.medical.service.other")
setnames(dt, 180, "visit.medical.service.appointment")
setnames(dt, 181, "visit.medical.service.howsoon")
setnames(dt, 197, "alter.routine")
setnames(dt, 199, "howlong.altered")
setnames(dt, 201, "howmany.household.ili")
setnames(dt, 202, "howmany.other.ili")

dt$ili <-((dt$fever =="t" & dt$muscle.and.or.joint.pain =="t" &
  (dt$symptoms.suddenly == 0 | dt$fever.suddenly == 0) &
           (dt$cough == "t" | dt$sore.throat == "t" | dt$chest.pain =="t"))==T)
freq <- data.table(aggregate(dt$user, by=list(dt$user), length))
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
