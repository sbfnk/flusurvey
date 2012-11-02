library(ggplot2)

peak <- dt[country=="uk" & date > "2012-02-25" & date < "2012-04-09" & age > 17]
peak <- peak[postcode!=""]

peak$postcode <- toupper(peak$postcode)
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

peak <- peak[!(region %in% c("Scotland", "Channel Islands", "Northern Ireland"))]
setkey(peak, global.id.number)

nb.users.noclean <- nrow(peak[!duplicated(peak$global.id.number)])

# need to have reported in 3 intervals

peak1 <- peak[date < "2012-03-12"]
peak2 <- peak[date > "2012-03-10" & date < "2012-03-26"]
peak3 <- peak[date > "2012-03-25"]

max1date <- data.table(aggregate(peak1$date, by=list(peak1$global.id.number),
                                 max))
setkey(max1date, Group.1)
peak <- peak[max1date]
setnames(peak, "x", "max1date")

min2date <- data.table(aggregate(peak2$date, by=list(peak2$global.id.number),
                                 min))
setkey(min2date, Group.1)
peak <- peak[min2date]
setnames(peak, "x", "min2date")


max2date <- data.table(aggregate(peak2$date, by=list(peak2$global.id.number),
                                 max))
setkey(max2date, Group.1)
peak <- peak[max2date]
setnames(peak, "x", "max2date")

min3date <- data.table(aggregate(peak3$date, by=list(peak3$global.id.number),
                                 min))
setkey(min3date, Group.1)
peak <- peak[min3date]
setnames(peak, "x", "min3date")

maxdate <- data.table(aggregate(peak$date, by=list(peak$global.id.number),
                                min))
setkey(maxdate, Group.1)
peak <- peak[maxdate]
setnames(peak, "x", "maxdate")

peak$diff1 <- difftime(peak$min2date, peak$max1date, units='days')
peak$diff2 <- difftime(peak$min3date, peak$max2date, units='days')

peak <- peak[global.id.number %in%
             intersect(
                       intersect(
                                 peak[date < "2012-03-12"]$global.id.number,
                                 peak[date > "2012-03-10" & date <
                                      "2012-03-26"]$global.id.number),
                       peak[date > "2012-03-24"]$global.id.number)]
peak <- peak[diff1<16 & diff2 < 16]

peak.users <- peak[!duplicated(peak$global.id.number)]
nb.users.clean <- nrow(peak.users)

nb.users.noclean
nb.users.clean
nb.users.clean/nb.users.noclean # 0.5874409

# HPA definition
peak.users[is.na(fever.suddenly)]$fever.suddenly <- 1
peak$ili.hpa <- as.numeric(peak$fever.suddenly == 0 & peak$cough =="t")
peak[is.na(ili.hpa)]$ili.hpa <- 0

# remove the ones that were reported to have ended earlier or started later
peak[symptoms.start.date > "2012-04-08"]$ili <- 0
peak[symptoms.start.date > "2012-04-08"]$ili.hpa <- 0
peak[symptoms.end.date < "2012-02-26"]$ili <- 0
peak[symptoms.end.date < "2012-02-26"]$ili.hpa <- 0

nrow(peak.users[gender==0])/nrow(peak.users[gender==1]) # 0.7365269
nrow(peak.users[age < 25])/nrow(peak.users) # 0.02988506
nrow(peak.users[age > 24 & age < 45])/nrow(peak.users) # 0.3436782
nrow(peak.users[age > 44 & age < 65])/nrow(peak.users) # 0.4551724
nrow(peak.users[age > 64])/nrow(peak.users) # 0.1712644

table(peak.users$region) / nrow(peak.users)

table(peak.users$education.msc) / nrow(peak.users)
table(peak.users$education.bsc) / nrow(peak.users)
table(peak.users$education.alevels) / nrow(peak.users)
table(peak.users$education.gcse) / nrow(peak.users)
table(peak.users$no.education) / nrow(peak.users)

table(peak.users$occupation) / nrow(peak.users)

t <- rowSums(peak.users[,c(27,29,31,33,35),with=F], na.rm=T)
table(t[t>0])/length(t[t>0])

nrow(peak.users[age<65])

table(dt[(dt$global.id.number %in% peak.users$global.id.number) &
         (!duplicated(dt$global.id.number)) &
         (norisk == "f") &
         (age < 65)
         ]$vaccine) /
  nrow(dt[(dt$global.id.number %in% peak.users$global.id.number) &
          (!duplicated(dt$global.id.number)) &
          (norisk == "f") &
          (age < 65)
          ])

table(dt[(dt$global.id.number %in% peak.users$global.id.number) &
         (!duplicated(dt$global.id.number)) &
         (age >= 65)
         ]$vaccine) /
  nrow(dt[(dt$global.id.number %in% peak.users$global.id.number) &
          (!duplicated(dt$global.id.number)) &
          (age >= 65)
          ])

table(dt[(dt$global.id.number %in% peak.users$global.id.number) &
         (!duplicated(dt$global.id.number)) &
         (pregnant == 0)
         ]$vaccine) /
  nrow(dt[(dt$global.id.number %in% peak.users$global.id.number) &
          (!duplicated(dt$global.id.number)) &
          (pregnant == 0)
          ])

table(peak.users[
         (norisk == "f") &
         (age < 65)
         ]$vaccine) /
  nrow(peak.users[
          (norisk == "f") &
          (age < 65)
          ])

table(peak.users[
         (age >= 65)
         ]$vaccine) /
  nrow(peak.users[
          (age >= 65)
          ])

table(peak.users[
         (pregnant == 0)
         ]$vaccine) /
  nrow(peak.users[
          (pregnant == 0)
          ])

for (symptom in c("fever", "chills", "blocked.runny.nose", "sneezing",
  "sore.throat", "cough", "shortness.breath", "headache",
  "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
  "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache", "other")) { 
  peak.users$nb <- with(peak, aggregate((get(symptom) == "t"),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  peak.users <- peak.users[, which(!grepl(symptom, colnames(peak.users))), with=FALSE]
  peak.users <- peak.users[,symptom:=(nb>0), with=F]
}

for (symptom in c("fever.suddenly")) {
  peak <- peak[is.na(get(symptom)), symptom := -1, with=F]  
  peak.users$nb <- with(peak, aggregate((get(symptom) == 0),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  peak.users <- peak.users[, which(!grepl(symptom, colnames(peak.users))), with=FALSE]
  peak.users <- peak.users[,symptom:=(nb>0), with=F]
}

for (change in c("visit.medical.service.no", "contact.medical.service.no",
                 "no.medication")) {
  peak.users$nb <- with(peak, aggregate((get(change) == "t"),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  peak.users <- peak.users[, which(!grepl(change, colnames(peak.users))), with=FALSE]
  peak.users <- peak.users[,change:=(nb>0), with=F]
}

for (change in c("alter.routine")) {
  peak <- peak[is.na(get(change)), change := -1, with=F]  
  peak.users$nb <- with(peak, aggregate((get(change) > 0),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  peak.users <- peak.users[, which(!grepl(change, colnames(peak.users))), with=FALSE]
  peak.users <- peak.users[,change:=(nb>0), with=F]
}

for (change in c("absent")) {
  peak.users$nb <- with(peak, aggregate((get("alter.routine") == 1),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  peak.users <- peak.users[, which(!grepl(change, colnames(peak.users))), with=FALSE]
  peak.users <- peak.users[,change:=(nb>0), with=F]
}

peak.users <- peak.users[, which(!grepl("nb", colnames(peak.users))), with=FALSE]


peak.users$ili <- FALSE
peak.users$nbili <- with(peak, aggregate(ili,
                                 list(global.id.number=global.id.number),
                                 sum))$x
peak.users$ili <- (peak.users$nbili > 0)

peak.users$ili.hpa <- FALSE
peak.users$nbili.hpa <- with(peak, aggregate(ili.hpa,
                                 list(global.id.number=global.id.number),
                                 sum))$x
peak.users$ili.hpa <- (peak.users$nbili.hpa > 0)

peak.users$ili.fever <- FALSE
peak.users$nbili.fever <- with(peak, aggregate(ili.fever,
                                 list(global.id.number=global.id.number),
                                 sum))$x
peak.users$ili.fever <- (peak.users$nbili.fever > 0)

peak$ili.self <- (peak$Q11 == 0)
peak[is.na(ili.self)]$ili.self <- FALSE
peak.users$ili.self <- FALSE
peak.users$nbili.self <- with(peak, aggregate(ili.self,
                                              list(global.id.number=global.id.number),
                                              sum))$x
peak.users$ili.self <- (peak.users$nbili.self > 0)

# self-reported ILI

table(peak.users$ili.self)
table(peak.users$ili.self)/nrow(peak.users)

table(peak.users[(norisk == "f") & (age < 65)]$ili.self)
table(peak.users[(norisk == "f") & (age < 65)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age < 65)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.self)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.self)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)])

table(peak.users[(norisk == "f") & (age >= 65)]$ili.self)
table(peak.users[(norisk == "f") & (age >= 65)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age >= 65)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.self)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.self)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.self) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)])

table(peak.users[pregnant == 0]$ili.self)
table(peak.users[pregnant == 0]$ili.self) /
  nrow(peak.users[pregnant == 0])

table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.self)
table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.self) /
  nrow(peak.users[pregnant == 0 & (vaccine == 1)])

table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.self)
table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.self) /
  nrow(peak.users[pregnant == 0 & (vaccine == 0)])

peak.users$agegroup <- cut(peak.users$age, breaks=c(0,18,25,35,45,55,65,75,
                           max(dt$age, na.rm=T)), 
                           include.lowest=T, right=F)

table(peak.users[ili.self==1]$agegroup)
table(peak.users[ili.self==1]$agegroup) / table(peak.users$agegroup)

table(peak.users[ili.self==1]$gender)
table(peak.users[ili.self==1]$gender) / table(peak.users$gender)

table(peak.users[ili.self==1]$cough)
table(peak.users[ili.self==1]$cough) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$sore.throat)
table(peak.users[ili.self==1]$sore.throat) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$muscle.and.or.joint.pain)
table(peak.users[ili.self==1]$muscle.and.or.joint.pain) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$fever.suddenly)
table(peak.users[ili.self==1]$fever.suddenly) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$shortness.breath)
table(peak.users[ili.self==1]$shortness.breath) / nrow(peak.users[ili.self==1])

table((peak.users[ili.self==1]$chills == TRUE |
       peak.users[ili.self==1]$blocked.runny.nose == TRUE |
       peak.users[ili.self==1]$sneezing == TRUE |
       peak.users[ili.self==1]$headache == TRUE |
       peak.users[ili.self==1]$chest.pain  == TRUE |
       peak.users[ili.self==1]$tired == TRUE |
       peak.users[ili.self==1]$loss.appetite  == TRUE |
       peak.users[ili.self==1]$phlegm == TRUE |
       peak.users[ili.self==1]$watery.eyes  == TRUE |
       peak.users[ili.self==1]$nausea  == TRUE |
       peak.users[ili.self==1]$vomiting == TRUE |
       peak.users[ili.self==1]$diarrhoea == TRUE |
       peak.users[ili.self==1]$stomach.ache == TRUE |
       peak.users[ili.self==1]$other == TRUE |
       (peak.users[ili.self==1]$fever == TRUE &
        peak.users[ili.self==1]$fever.suddenly == FALSE)))
table((peak.users[ili.self==1]$chills == TRUE |
       peak.users[ili.self==1]$blocked.runny.nose == TRUE |
       peak.users[ili.self==1]$sneezing == TRUE |
       peak.users[ili.self==1]$headache == TRUE |
       peak.users[ili.self==1]$chest.pain  == TRUE |
       peak.users[ili.self==1]$tired == TRUE |
       peak.users[ili.self==1]$loss.appetite  == TRUE |
       peak.users[ili.self==1]$phlegm == TRUE |
       peak.users[ili.self==1]$watery.eyes  == TRUE |
       peak.users[ili.self==1]$nausea  == TRUE |
       peak.users[ili.self==1]$vomiting == TRUE |
       peak.users[ili.self==1]$diarrhoea == TRUE |
       peak.users[ili.self==1]$stomach.ache == TRUE |
       peak.users[ili.self==1]$other == TRUE |
       (peak.users[ili.self==1]$fever == TRUE &
        peak.users[ili.self==1]$fever.suddenly == FALSE))) /
  nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$no.medication)
table(peak.users[ili.self==1]$no.medication) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$visit.medical.service.no)
table(peak.users[ili.self==1]$visit.medical.service.no) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$contact.medical.service.no)
table(peak.users[ili.self==1]$contact.medical.service.no) / nrow(peak.users[ili.self==1])

table(peak.users[ili.self==1]$absent)
table(peak.users[ili.self==1]$absent) / nrow(peak.users[ili.self==1])

# HPA definition

table(peak.users$ili.hpa)
table(peak.users$ili.hpa)/nrow(peak.users)

table(peak.users[(norisk == "f") & (age < 65)]$ili.hpa)
table(peak.users[(norisk == "f") & (age < 65)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age < 65)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.hpa)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.hpa)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)])

table(peak.users[(norisk == "f") & (age >= 65)]$ili.hpa)
table(peak.users[(norisk == "f") & (age >= 65)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age >= 65)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.hpa)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.hpa)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.hpa) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)])

table(peak.users[pregnant == 0]$ili.hpa)
table(peak.users[pregnant == 0]$ili.hpa) /
  nrow(peak.users[pregnant == 0])

table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.hpa)
table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.hpa) /
  nrow(peak.users[pregnant == 0 & (vaccine == 1)])

table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.hpa)
table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.hpa) /
  nrow(peak.users[pregnant == 0 & (vaccine == 0)])

peak.users$agegroup <- cut(peak.users$age, breaks=c(0,18,25,35,45,55,65,75,
                           max(dt$age, na.rm=T)), 
                           include.lowest=T, right=F)

table(peak.users[ili.hpa==1]$agegroup)
table(peak.users[ili.hpa==1]$agegroup) / table(peak.users$agegroup)

table(peak.users[ili.hpa==1]$gender)
table(peak.users[ili.hpa==1]$gender) / table(peak.users$gender)

table(peak.users[ili.hpa==1]$cough)
table(peak.users[ili.hpa==1]$cough) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$sore.throat)
table(peak.users[ili.hpa==1]$sore.throat) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$muscle.and.or.joint.pain)
table(peak.users[ili.hpa==1]$muscle.and.or.joint.pain) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$fever.suddenly)
table(peak.users[ili.hpa==1]$fever.suddenly) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$shortness.breath)
table(peak.users[ili.hpa==1]$shortness.breath) / nrow(peak.users[ili.hpa==1])

table((peak.users[ili.hpa==1]$chills == TRUE |
       peak.users[ili.hpa==1]$blocked.runny.nose == TRUE |
       peak.users[ili.hpa==1]$sneezing == TRUE |
       peak.users[ili.hpa==1]$headache == TRUE |
       peak.users[ili.hpa==1]$chest.pain  == TRUE |
       peak.users[ili.hpa==1]$tired == TRUE |
       peak.users[ili.hpa==1]$loss.appetite  == TRUE |
       peak.users[ili.hpa==1]$phlegm == TRUE |
       peak.users[ili.hpa==1]$watery.eyes  == TRUE |
       peak.users[ili.hpa==1]$nausea  == TRUE |
       peak.users[ili.hpa==1]$vomiting == TRUE |
       peak.users[ili.hpa==1]$diarrhoea == TRUE |
       peak.users[ili.hpa==1]$stomach.ache == TRUE |
       peak.users[ili.hpa==1]$other == TRUE |
       (peak.users[ili.hpa==1]$fever == TRUE &
        peak.users[ili.hpa==1]$fever.suddenly == FALSE)))
table((peak.users[ili.hpa==1]$chills == TRUE |
       peak.users[ili.hpa==1]$blocked.runny.nose == TRUE |
       peak.users[ili.hpa==1]$sneezing == TRUE |
       peak.users[ili.hpa==1]$headache == TRUE |
       peak.users[ili.hpa==1]$chest.pain  == TRUE |
       peak.users[ili.hpa==1]$tired == TRUE |
       peak.users[ili.hpa==1]$loss.appetite  == TRUE |
       peak.users[ili.hpa==1]$phlegm == TRUE |
       peak.users[ili.hpa==1]$watery.eyes  == TRUE |
       peak.users[ili.hpa==1]$nausea  == TRUE |
       peak.users[ili.hpa==1]$vomiting == TRUE |
       peak.users[ili.hpa==1]$diarrhoea == TRUE |
       peak.users[ili.hpa==1]$stomach.ache == TRUE |
       peak.users[ili.hpa==1]$other == TRUE |
       (peak.users[ili.hpa==1]$fever == TRUE &
        peak.users[ili.hpa==1]$fever.suddenly == FALSE))) /
  nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$no.medication)
table(peak.users[ili.hpa==1]$no.medication) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$visit.medical.service.no)
table(peak.users[ili.hpa==1]$visit.medical.service.no) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$contact.medical.service.no)
table(peak.users[ili.hpa==1]$contact.medical.service.no) / nrow(peak.users[ili.hpa==1])

table(peak.users[ili.hpa==1]$absent)
table(peak.users[ili.hpa==1]$absent) / nrow(peak.users[ili.hpa==1])

# ECDC

table(peak.users$ili)
table(peak.users$ili)/nrow(peak.users)

table(peak.users[(norisk == "f") & (age < 65)]$ili)
table(peak.users[(norisk == "f") & (age < 65)]$ili) /
  nrow(peak.users[(norisk == "f") & (age < 65)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)])

table(peak.users[(norisk == "f") & (age >= 65)]$ili)
table(peak.users[(norisk == "f") & (age >= 65)]$ili) /
  nrow(peak.users[(norisk == "f") & (age >= 65)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)])

table(peak.users[pregnant == 0]$ili)
table(peak.users[pregnant == 0]$ili) /
  nrow(peak.users[pregnant == 0])

table(peak.users[pregnant == 0 & (vaccine == 1)]$ili)
table(peak.users[pregnant == 0 & (vaccine == 1)]$ili) /
  nrow(peak.users[pregnant == 0 & (vaccine == 1)])

table(peak.users[pregnant == 0 & (vaccine == 0)]$ili)
table(peak.users[pregnant == 0 & (vaccine == 0)]$ili) /
  nrow(peak.users[pregnant == 0 & (vaccine == 0)])

peak.users$agegroup <- cut(peak.users$age, breaks=c(0,18,25,35,45,55,65,75,
                           max(dt$age, na.rm=T)), 
                           include.lowest=T, right=F)

table(peak.users[ili==1]$agegroup)
table(peak.users[ili==1]$agegroup) / table(peak.users$agegroup)

table(peak.users[ili==1]$gender)
table(peak.users[ili==1]$gender) / table(peak.users$gender)

table(peak.users[ili==1]$cough)
table(peak.users[ili==1]$cough) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$sore.throat)
table(peak.users[ili==1]$sore.throat) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$muscle.and.or.joint.pain)
table(peak.users[ili==1]$muscle.and.or.joint.pain) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$fever.suddenly)
table(peak.users[ili==1]$fever.suddenly) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$shortness.breath)
table(peak.users[ili==1]$shortness.breath) / nrow(peak.users[ili==1])

table((peak.users[ili==1]$chills == TRUE |
       peak.users[ili==1]$blocked.runny.nose == TRUE |
       peak.users[ili==1]$sneezing == TRUE |
       peak.users[ili==1]$headache == TRUE |
       peak.users[ili==1]$chest.pain  == TRUE |
       peak.users[ili==1]$tired == TRUE |
       peak.users[ili==1]$loss.appetite  == TRUE |
       peak.users[ili==1]$phlegm == TRUE |
       peak.users[ili==1]$watery.eyes  == TRUE |
       peak.users[ili==1]$nausea  == TRUE |
       peak.users[ili==1]$vomiting == TRUE |
       peak.users[ili==1]$diarrhoea == TRUE |
       peak.users[ili==1]$stomach.ache == TRUE |
       peak.users[ili==1]$other == TRUE |
       (peak.users[ili==1]$fever == TRUE &
        peak.users[ili==1]$fever.suddenly == FALSE)))
table((peak.users[ili==1]$chills == TRUE |
       peak.users[ili==1]$blocked.runny.nose == TRUE |
       peak.users[ili==1]$sneezing == TRUE |
       peak.users[ili==1]$headache == TRUE |
       peak.users[ili==1]$chest.pain  == TRUE |
       peak.users[ili==1]$tired == TRUE |
       peak.users[ili==1]$loss.appetite  == TRUE |
       peak.users[ili==1]$phlegm == TRUE |
       peak.users[ili==1]$watery.eyes  == TRUE |
       peak.users[ili==1]$nausea  == TRUE |
       peak.users[ili==1]$vomiting == TRUE |
       peak.users[ili==1]$diarrhoea == TRUE |
       peak.users[ili==1]$stomach.ache == TRUE |
       peak.users[ili==1]$other == TRUE |
       (peak.users[ili==1]$fever == TRUE &
        peak.users[ili==1]$fever.suddenly == FALSE))) /
  nrow(peak.users[ili==1])

table(peak.users[ili==1]$no.medication)
table(peak.users[ili==1]$no.medication) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$visit.medical.service.no)
table(peak.users[ili==1]$visit.medical.service.no) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$contact.medical.service.no)
table(peak.users[ili==1]$contact.medical.service.no) / nrow(peak.users[ili==1])

table(peak.users[ili==1]$absent)
table(peak.users[ili==1]$absent) / nrow(peak.users[ili==1])

# ECDC + fever

table(peak.users$ili.fever)
table(peak.users$ili.fever)/nrow(peak.users)

table(peak.users[(norisk == "f") & (age < 65)]$ili.fever)
table(peak.users[(norisk == "f") & (age < 65)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age < 65)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.fever)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.fever)
table(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age < 65) & (vaccine == 0)])

table(peak.users[(norisk == "f") & (age >= 65)]$ili.fever)
table(peak.users[(norisk == "f") & (age >= 65)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age >= 65)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.fever)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 1)])

table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.fever)
table(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)]$ili.fever) /
  nrow(peak.users[(norisk == "f") & (age >= 65) & (vaccine == 0)])

table(peak.users[pregnant == 0]$ili.fever)
table(peak.users[pregnant == 0]$ili.fever) /
  nrow(peak.users[pregnant == 0])

table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.fever)
table(peak.users[pregnant == 0 & (vaccine == 1)]$ili.fever) /
  nrow(peak.users[pregnant == 0 & (vaccine == 1)])

table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.fever)
table(peak.users[pregnant == 0 & (vaccine == 0)]$ili.fever) /
  nrow(peak.users[pregnant == 0 & (vaccine == 0)])

peak.users$agegroup <- cut(peak.users$age, breaks=c(0,18,25,35,45,55,65,75,
                           max(dt$age, na.rm=T)), 
                           include.lowest=T, right=F)

table(peak.users[ili.fever==1]$agegroup)
table(peak.users[ili.fever==1]$agegroup) / table(peak.users$agegroup)

table(peak.users[ili.fever==1]$gender)
table(peak.users[ili.fever==1]$gender) / table(peak.users$gender)

table(peak.users[ili.fever==1]$cough)
table(peak.users[ili.fever==1]$cough) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$sore.throat)
table(peak.users[ili.fever==1]$sore.throat) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$muscle.and.or.joint.pain)
table(peak.users[ili.fever==1]$muscle.and.or.joint.pain) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$fever.suddenly)
table(peak.users[ili.fever==1]$fever.suddenly) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$shortness.breath)
table(peak.users[ili.fever==1]$shortness.breath) / nrow(peak.users[ili.fever==1])

table((peak.users[ili.fever==1]$chills == TRUE |
       peak.users[ili.fever==1]$blocked.runny.nose == TRUE |
       peak.users[ili.fever==1]$sneezing == TRUE |
       peak.users[ili.fever==1]$headache == TRUE |
       peak.users[ili.fever==1]$chest.pain  == TRUE |
       peak.users[ili.fever==1]$tired == TRUE |
       peak.users[ili.fever==1]$loss.appetite  == TRUE |
       peak.users[ili.fever==1]$phlegm == TRUE |
       peak.users[ili.fever==1]$watery.eyes  == TRUE |
       peak.users[ili.fever==1]$nausea  == TRUE |
       peak.users[ili.fever==1]$vomiting == TRUE |
       peak.users[ili.fever==1]$diarrhoea == TRUE |
       peak.users[ili.fever==1]$stomach.ache == TRUE |
       peak.users[ili.fever==1]$other == TRUE |
       (peak.users[ili.fever==1]$fever == TRUE &
        peak.users[ili.fever==1]$fever.suddenly == FALSE)))
table((peak.users[ili.fever==1]$chills == TRUE |
       peak.users[ili.fever==1]$blocked.runny.nose == TRUE |
       peak.users[ili.fever==1]$sneezing == TRUE |
       peak.users[ili.fever==1]$headache == TRUE |
       peak.users[ili.fever==1]$chest.pain  == TRUE |
       peak.users[ili.fever==1]$tired == TRUE |
       peak.users[ili.fever==1]$loss.appetite  == TRUE |
       peak.users[ili.fever==1]$phlegm == TRUE |
       peak.users[ili.fever==1]$watery.eyes  == TRUE |
       peak.users[ili.fever==1]$nausea  == TRUE |
       peak.users[ili.fever==1]$vomiting == TRUE |
       peak.users[ili.fever==1]$diarrhoea == TRUE |
       peak.users[ili.fever==1]$stomach.ache == TRUE |
       peak.users[ili.fever==1]$other == TRUE |
       (peak.users[ili.fever==1]$fever == TRUE &
        peak.users[ili.fever==1]$fever.suddenly == FALSE))) /
  nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$no.medication)
table(peak.users[ili.fever==1]$no.medication) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$visit.medical.service.no)
table(peak.users[ili.fever==1]$visit.medical.service.no) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$contact.medical.service.no)
table(peak.users[ili.fever==1]$contact.medical.service.no) / nrow(peak.users[ili.fever==1])

table(peak.users[ili.fever==1]$absent)
table(peak.users[ili.fever==1]$absent) / nrow(peak.users[ili.fever==1])

peak.users$agegroup2 <- cut(peak.users$age, breaks=c(0,20,30,40,50,60,70,80,
                           max(dt$age, na.rm=T)), include.lowest=T, right=F)

# table(peak.users[ili.self==1 &
#                  absent==T]$agegroup2)/table(peak.users[ili.self==1]$agegroup2)

absent.age <- as.vector(table(peak.users[ili.self==1 &
                                         absent==T]$agegroup2)/
                        table(peak.users[ili.self==1]$agegroup2)) 
absent.age[is.nan(absent.age)] <- 0
agegroup.absent <- data.table(age=levels(peak.users$agegroup2),
                                         absent=absent.age)

png("absenteeism.png", width=640)
ggplot(agegroup.absent[-1], aes(x=age, y=absent*100, group=1))+ geom_line()+
  theme_bw(20)+  opts(panel.grid.major=theme_blank(),
                      panel.grid.minor=theme_blank())+
  scale_y_continuous("%", limits=c(0,80))
dev.off()

table(peak.users[vaccine==0]$ili.self)
table(peak.users[vaccine==1]$ili.self)

table(peak.users[vaccine==0]$ili.hpa)
table(peak.users[vaccine==1]$ili.hpa)

m <- data.table(melt(peak, measure.vars=c("ili.self", "ili.hpa")))

png("ili_date.png", width=640)
ggplot(m[value == 1 & symptoms.start.date>"2012-02-23"],
       aes(x=symptoms.start.date, fill=variable))+ geom_histogram(binwidth=2,
                                    position="dodge")+
  scale_fill_brewer("ILI", labels=c("Self-reported", "HPA definition"),
                    palette="Set1")+ theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank())+
  scale_y_continuous("Count")+ scale_x_date("Date")
dev.off()

png("ili_week.png", width=640)
ggplot(m[value == 1 & symptoms.start.date>"2012-02-23"],
       aes(x=symptoms.start.date, fill=variable))+ geom_histogram(binwidth=7,
                                    position="dodge")+
  scale_fill_brewer("ILI", labels=c("Self-reported", "HPA definition"),
                    palette="Set1")+ theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank())+
  scale_y_continuous("Count")+ scale_x_date("Week", labels=c("",8,9,10,11,12,""))
dev.off()

# higher education etc

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

