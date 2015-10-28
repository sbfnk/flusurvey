library(ggplot2)
library(data.table)
library(plyr)

render.table <- function(data, caption, include.colnames = F,
                         header = " & \\% \\\\", ...)
{
    cat("\\subsubsection*{", caption, "}", sep="")
    print(xtable(data, caption = caption), include.colnames = include.colnames,
          floating=F, booktabs = T,
          add.to.row = list(pos=list(0), command = header), ...)
}

peak <- dt[country=="uk" & date > "2012-02-25" & date < "2012-04-09" & age > 17]
peak <- peak[postcode!=""]

peak$postcode <- toupper(peak$postcode)
#peak$area <- toupper(sub("^([A-Za-z]+).+$", "\\1", peak$postcode))
postcodes <- data.table(read.csv("postcodes.csv", header=F, sep=","))
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

peak <- peak[!(region %in% c("Scotland", "Channel Islands",
                             "Northern Ireland"))]
peak$region <- factor(peak$region)

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
setnames(peak, "x", "max3date")

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

peak$agegroup <- cut(peak$age, breaks=c(18,25,45,65, max(peak$age, na.rm=T)),
                   include.lowest=T, right=F,
                     labels=c("18-24", "25-44", "45-64", "65+")) 

peak$agegroup2 <- cut(peak$age, breaks=c(18,25,35,45,55,65,75, max(peak$age, na.rm=T)),
                   include.lowest=T, right=F,
                     labels=c("18-24", "25-34", "35-44", "45-54",
                   "55-64", "65-74", "75+")) 

peak$gender <- factor(peak$gender, labels=c("male", "female"))

# HPA definition
peak[is.na(fever.suddenly), fever.suddenly := as.integer(1)]
peak$ili.hpa <- as.numeric(peak$fever.suddenly == 0 & peak$cough =="t")
peak[is.na(ili.hpa), ili.hpa := 0]

# remove the ones that were reported to have ended earlier or started later
peak[symptoms.start.date > "2012-04-08", ili := 0]
peak[symptoms.start.date > "2012-04-08", ili.hpa := 0]
peak[symptoms.end.date < "2012-02-26", ili := 0]
peak[symptoms.end.date < "2012-02-26", ili.hpa := 0]

peak[,education := "None"]

#peak[no.education == "t", education := "None"]
peak[education.gcse == "t", education := "GCSE"]
peak[education.alevels == "t", education := "A-Level"]
peak[education.bsc == "t" | education.msc == "t", education :=
           "Higher"]

peak$education <- factor(peak$education,
                               levels=c("Higher", "A-Level", "GCSE",
                               "None")) 

peak$occupation <- factor(peak$occupation,
                               labels=c("Full-time", "Part-time",
                               "Self-employed", "Student",
                               "Home-maker", "Unemployed",
                               "Long-term leave", "Retired", "Other"))

setnames(peak, "nb.household.65+", "nb.household.65")

peak[is.na(nb.household.0.4), nb.household.0.4 := as.integer(0)]
peak[is.na(nb.household.5.18), nb.household.5.18 := as.integer(0)]
peak[is.na(nb.household.19.44), nb.household.19.44 := as.integer(0)]
peak[is.na(nb.household.45.64), nb.household.45.64 := as.integer(0)]
peak[is.na(nb.household.65), nb.household.65 := as.integer(0)]

peak[, nb.household := nb.household.0.4 + nb.household.5.18 +
     nb.household.19.44 + nb.household.45.64 + nb.household.65]

peak$household <- cut(peak$nb.household,
                      breaks=c(1, 2, 3, 4, 5, 6, 
                          max(peak$nb.household, na.rm=T)), 
                      include.lowest=T, right=F, 
                      labels= c(1, 2, 3, 4, 5, "6+"))

peak[, risk := "None"]
peak[risk.heart == "t", risk := "Heart problem"]
peak[risk.asthma == "t", risk := "Asthma"]
peak[risk.lung == "t", risk := "Lung problem"]
peak[risk.kidney == "t", risk := "Kidney disease"]
peak[risk.diabetes == "t", risk := "Diabetes"]
peak[risk.immune == "t", risk := "Immunosuppression"]

npreg <- nrow(peak[!duplicated(peak$global.id.number) & pregnant == 0])
groups <- c("Under 65 (risk group)", "Over 65",
            paste("Pregnant women (n=", npreg, ")",
                  sep=""))
peak[, group := "None"]
peak[risk != "None" & age < 65, group := groups[1]]
peak[age >= 65, group := groups[2]]
peak[pregnant == 0, group := groups[3]]

peak[, vaccine.status := "Unvaccinated"]
peak[vaccine == 1, vaccine.status := "Vaccinated"]

peak[, sudden.onset := "f"]
peak[(fever.suddenly == 0 | symptoms.suddenly == 0), sudden.onset := "t"]

peak.users <- peak[!duplicated(peak$global.id.number)]

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
  peak <- peak[is.na(get(change)), change := as.integer(-1), with=F]  
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

peak.users <- peak.users[!is.na(ili)]

nb.users.clean <- nrow(peak.users)

sink("temp.Rnw")

cat("
\\documentclass{article}
\\usepackage{times}
\\usepackage{fullpage}
\\usepackage{booktabs}
\\usepackage{Sweave}
\\parskip=12pt
\\begin{document}
")
cat("\\subsection*{Sample description}")

cat(sprintf("Of %d participants that completed at least a symptom survey between 26/2/2012 and 8/4/2012, %d~(%.0f", nb.users.noclean, nb.users.clean, nb.users.clean / nb.users.noclean * 100))

cat("\\%) completed enough surveys to cover the whole period between 26/2/2012 and 25/3/2012. \\par\n")

## cat(sprintf("Male:female ratio: %.2f.\\par\n", nrow(peak.users[gender==0])/nrow(peak.users[gender==1]))) # 0.7365269

## nrow(peak.users[age < 25])/nrow(peak.users) # 0.02988506
## nrow(peak.users[age > 24 & age < 45])/nrow(peak.users) # 0.3436782
## nrow(peak.users[age > 44 & age < 65])/nrow(peak.users) # 0.4551724
## nrow(peak.users[age > 64])/nrow(peak.users) # 0.1712644

render.table(table(peak.users$gender) / nrow(peak.users) * 100,
             "Sex")
cat("\\par male:female ratio ",
                   sprintf("%.2f", nrow(peak.users[gender == "male"]) /
                           nrow(peak.users[gender=="female"])), "\\par")

render.table(table(peak.users$agegroup) / nrow(peak.users) * 100,
            "Age group")

render.table(table(peak.users$region) / nrow(peak.users) * 100,
             "Region")

render.table(table(peak.users$education) / nrow(peak.users) * 100,
             "Education")

render.table(table(peak.users$occupation) / nrow(peak.users) * 100,
             "Employment")

render.table(table(peak.users$household) / nrow(peak.users) * 100,
             "Household size")

#cat("\\subsection*{Risk factors}")
render.table(table(peak.users[age<65]$risk) / nrow(peak.users[age<65]) * 100,
             paste("Risk factor in under 65 year olds. n=",
                   nrow(peak.users[age<65]), sep=""))

#cat("\\subsection*{Vaccine uptake}")
vaccine.uptake <- data.frame(uptake = sapply(groups, function(x)
  {
      nrow(peak.users[group==x & vaccine.status == "Vaccinated"]) /
          nrow(peak.users[group==x])
  }))

render.table(vaccine.uptake * 100,
             "Vaccine uptake",
             include.rownames = T)

cat("\\subsection*{ILI attack rates}")

cat("\\begin{itemize}")
cat("\\item \\emph{self-reported}: people reporting that they believe their symptoms have been caused by ILI")
cat("\\item \\emph{HPA def.}: Cough + sudden fever")
cat("\\item \\emph{ECDC def.}: Sudden onset + (fever or headache or muscle / joint pain or feeling tired) + a respiratory symptom")
cat("\\item \\emph{ECDC + fever}: as above but requiring fever")
cat("\\end{itemize}")

ili.overall <- data.frame(self.N = sum(peak.users$ili.self),
                          self.percent = sum(peak.users$ili.self)/length(peak.users$ili.self)*100,
                          hpa.N = sum(peak.users$ili.hpa),
                          hpa.percent = sum(peak.users$ili.hpa)/length(peak.users$ili.hpa)*100,
                          ecdc.N = sum(peak.users$ili),
                          ecdc.percent = sum(peak.users$ili)/length(peak.users$ili)*100,
                          ecdc.fever.N = sum(peak.users$ili.fever),
                          ecdc.fever.percent = sum(peak.users$ili.fever)/length(peak.users$ili.fever)*100)

ili.vaccination <- ddply(peak.users, .(vaccine.status), summarise,
                   self.N = sum(ili.self),
                   self.percent = sum(ili.self)/length(ili.self)*100,
                   hpa.N = sum(ili.hpa),
                   hpa.percent = sum(ili.hpa)/length(ili.hpa)*100,
                   ecdc.N = sum(ili),
                   ecdc.percent = sum(ili)/length(ili)*100,
                   ecdc.fever.N = sum(ili.fever),
                   ecdc.fever.percent = sum(ili.fever)/length(ili.fever)*100)


ili.groups <- ddply(peak.users[group != "None"], .(group,
                                  vaccine.status), summarise,
                   self.N = sum(ili.self),
                   self.percent = sum(ili.self)/length(ili.self)*100,
                   hpa.N = sum(ili.hpa),
                   hpa.percent = sum(ili.hpa)/length(ili.hpa)*100,
                   ecdc.N = sum(ili),
                   ecdc.percent = sum(ili)/length(ili)*100,
                   ecdc.fever.N = sum(ili.fever),
                   ecdc.fever.percent = sum(ili.fever)/length(ili.fever)*100)

ili.agegroups <- ddply(peak.users, .(agegroup2),
                   summarise, 
                   self.N = sum(ili.self),
                   self.percent = sum(ili.self)/length(ili.self)*100,
                   hpa.N = sum(ili.hpa),
                   hpa.percent = sum(ili.hpa)/length(ili.hpa)*100,
                   ecdc.N = sum(ili),
                   ecdc.percent = sum(ili)/length(ili)*100,
                   ecdc.fever.N = sum(ili.fever),
                   ecdc.fever.percent = sum(ili.fever)/length(ili.fever)*100)

ili.gender <- ddply(peak.users, .(gender), summarise,
                   self.N = sum(ili.self),
                   self.percent = sum(ili.self)/length(ili.self)*100,
                   hpa.N = sum(ili.hpa),
                   hpa.percent = sum(ili.hpa)/length(ili.hpa)*100,
                   ecdc.N = sum(ili),
                   ecdc.percent = sum(ili)/length(ili)*100,
                   ecdc.fever.N = sum(ili.fever),
                   ecdc.fever.percent = sum(ili.fever)/length(ili.fever)*100)

render.table(ili.overall, "Overall ILI attack rate",
             include.rownames = F,
             header =
             paste("\\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   "N & \\% & N & \\% & N & \\% & N & \\% \\\\", sep=""))

render.table(ili.vaccination, "ILI attack rate by vaccination status",
             include.rownames = F,
             header =
             paste("& \\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   "& N & \\% & N & \\% & N & \\% & N & \\% \\\\", sep=""))

render.table(ili.groups, "ILI attack rate in risk groups",
             include.rownames = F,
             header =
             paste(" & & \\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   " & & N & \\% & N & \\% & N & \\% & N & \\% \\\\", sep=""))

render.table(ili.agegroups, "ILI attack rate by age group",
             include.rownames = F,
             header =
             paste(" & \\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   " & N & \\% & N & \\% & N & \\% & N & \\% \\\\", sep=""))

render.table(ili.gender, "ILI attack rate by sex",
             include.rownames = F,
             header =
             paste(" & \\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   " & N & \\% & N & \\% & N & \\% & N & \\% \\\\", sep=""))

symptoms <- c("sudden.onset", "fever", "chills", "blocked.runny.nose", "sneezing",
  "sore.throat", "cough", "shortness.breath", "headache",
  "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
  "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache",
  "other")

symptom.distribution <-
    data.frame(self.N = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == "t")
    }), self.percent = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == "t") / 
        sum(peak.users$ili.self == T) * 100
    }), hpa.N = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == "t")
    }), hpa.percent = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == "t") / 
        sum(peak.users$ili.hpa == T) * 100
    }), ecdc.N = sapply(symptoms, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == "t")
    }), ecdc.percent = sapply(symptoms, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == "t") / 
        sum(peak.users$ili == 1) * 100
    }), ecdc.fever.N = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == "t")
    }), ecdc.fever.percent = sapply(symptoms, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == "t") / 
        sum(peak.users$ili.fever == T) * 100
    }))

nochanges <- c("no.medication", "visit.medical.service.no",
                "contact.medical.service.no")
changes <- c("alter.routine")

change.distribution <-
    data.frame(self.N = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == F)
    }), self.percent = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == F) / 
        sum(peak.users$ili.self == T) * 100
    }), hpa.N = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == F)
    }), hpa.percent = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == F) / 
        sum(peak.users$ili.hpa == T) * 100
    }), ecdc.N = sapply(nochanges, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == F)
    }), ecdc.percent = sapply(nochanges, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == F) / 
        sum(peak.users$ili == 1) * 100
    }), ecdc.fever.N = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == F)
    }), ecdc.fever.percent = sapply(nochanges, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == F) / 
        sum(peak.users$ili.fever == T) * 100
    }))

change.distribution <- rbind(change.distribution, 
    data.frame(self.N = sapply(changes, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == T)
    }), self.percent = sapply(changes, function(x)
    {
        sum(peak.users$ili.self == T & peak.users[,x,with=F] == T) / 
        sum(peak.users$ili.self == T) * 100
    }), hpa.N = sapply(changes, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == T)
    }), hpa.percent = sapply(changes, function(x)
    {
        sum(peak.users$ili.hpa == T & peak.users[,x,with=F] == T) / 
        sum(peak.users$ili.hpa == T) * 100
    }), ecdc.N = sapply(changes, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == T)
    }), ecdc.percent = sapply(changes, function(x)
    {
        sum(peak.users$ili == 1 & peak.users[,x,with=F] == T) / 
        sum(peak.users$ili == 1) * 100
    }), ecdc.fever.N = sapply(changes, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == T)
    }), ecdc.fever.percent = sapply(changes, function(x)
    {
        sum(peak.users$ili.fever == T & peak.users[,x,with=F] == T) / 
        sum(peak.users$ili.fever == T) * 100
    })))

rownames(change.distribution) <- c("use of medicines",
                                   "visit medical service", 
                                   "consult medical service",
                                   "change daily routine") 

render.table(change.distribution,
             "Behaviour",
             header =
             paste(" & \\multicolumn{2}{c}{self-reported} & \\multicolumn{2}{c}{HPA def.} & \\multicolumn{2}{c}{ECDC def.} & \\multicolumn{2}{c}{ECDC + fever} \\\\",
                   " & N & \\% & N & \\% & N & \\% & N & \\% \\\\",
             sep=""))



cat("
\\end{document}
")

sink()
Sweave("temp.Rnw")
#compilePdf("temp.Rnw")

## peak.users$agegroup2 <- cut(peak.users$age, breaks=c(0,20,30,40,50,60,70,80,
##                            max(dt$age, na.rm=T)), include.lowest=T, right=F)

## # table(peak.users[ili.self==1 &
## #                  absent==T]$agegroup2)/table(peak.users[ili.self==1]$agegroup2)

## absent.age <- as.vector(table(peak.users[ili.self==1 &
##                                          absent==T]$agegroup2)/
##                         table(peak.users[ili.self==1]$agegroup2)) 
## absent.age[is.nan(absent.age)] <- 0
## agegroup.absent <- data.table(age=levels(peak.users$agegroup2),
##                                          absent=absent.age)

## #png("absenteeism.png", width=640)
## ggplot(agegroup.absent[-1], aes(x=age, y=absent*100, group=1))+ geom_line()+
##   theme_bw(20)+  opts(panel.grid.major=theme_blank(),
##                       panel.grid.minor=theme_blank())+
##   scale_y_continuous("%", limits=c(0,80))
## #dev.off()

## ## table(peak.users[vaccine==0]$ili.self)
## ## table(peak.users[vaccine==1]$ili.self)

## ## table(peak.users[vaccine==0]$ili.hpa)
## ## table(peak.users[vaccine==1]$ili.hpa)

## ## m <- data.table(melt(peak, measure.vars=c("ili.self", "ili.hpa")))

## ## png("ili_date.png", width=640)
## ## ggplot(m[value == 1 & symptoms.start.date>"2012-02-23"],
## ##        aes(x=symptoms.start.date, fill=variable))+ geom_histogram(binwidth=2,
## ##                                     position="dodge")+
## ##   scale_fill_brewer("ILI", labels=c("Self-reported", "HPA definition"),
## ##                     palette="Set1")+ theme_bw(20)+
## ##   opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank())+
## ##   scale_y_continuous("Count")+ scale_x_date("Date")
## ## dev.off()


## ## #png("ili_week.png", width=640)
## ## ggplot(m[value == 1 & symptoms.start.date>"2012-02-23"],
## ##        aes(x=symptoms.start.date, fill=variable))+ geom_histogram(binwidth=7,
## ##                                     position="dodge")+
## ##   scale_fill_brewer("ILI", labels=c("Self-reported", "HPA definition"),
## ##                     palette="Set1")+ theme_bw(20)+
## ##   opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank())+
## ##   scale_y_continuous("Count")+ scale_x_date("Week", labels=c("",8,9,10,11,12,""))
## ## #dev.off()

