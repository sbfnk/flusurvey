library(data.table)
library(ggplot2)

logistic.regression.or.ci <- function(regress.out, level=0.95)
{
################################################################
# #
# This function takes the output from a glm #
# (logistic model) command in R and provides not #
# only the usual output from the summary command, but #
# adds confidence intervals for all coefficients and OR’s. #
# #
# This version accommodates multiple regression parameters #
# #
################################################################
  usual.output <- summary(regress.out)
  z.quantile <- qnorm(1-(1-level)/2)
  number.vars <- length(regress.out$coefficients)
  OR <- exp(regress.out$coefficients[-1])
  temp.store.result <- matrix(rep(NA, number.vars*2), nrow=number.vars)
  for(i in 1:number.vars)
    {
      temp.store.result[i,] <- summary(regress.out)$coefficients[i] +
        c(-1, 1) * z.quantile * summary(regress.out)$coefficients[i+number.vars]
    }
  intercept.ci <- temp.store.result[1,]
  slopes.ci <- temp.store.result[-1,]
  
  OR.ci <- exp(slopes.ci)
  output <- list(regression.table = usual.output, intercept.ci = intercept.ci,
                 slopes.ci = slopes.ci, OR=OR, OR.ci = OR.ci)
  return(output)
}

# whole season
whole.users <- dt[!duplicated(dt$global.id.number)]

whole.users$vaccinated <- with(dt, aggregate(vaccine,
                                         ## list(global.id.number=global.id.number),
                                         list(global.id.number=global.id.number),
                                         sum))$x > 0

for (symptom in c("fever", "chills", "blocked.runny.nose", "sneezing",
  "sore.throat", "cough", "shortness.breath", "headache",
  "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
  "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache", "other")) { 
  whole.users$nb <- with(dt, aggregate((get(symptom) == "t"),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  whole.users <- whole.users[, which(!grepl(symptom, colnames(whole.users))), with=FALSE]
  whole.users <- whole.users[,symptom:=(nb>0), with=F]
}

for (symptom in c("fever.suddenly")) {
  dt <- dt[is.na(get(symptom)), symptom := -1, with=F]  
  whole.users$nb <- with(dt, aggregate((get(symptom) == 0),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  whole.users <- whole.users[, which(!grepl(symptom, colnames(whole.users))), with=FALSE]
  whole.users <- whole.users[,symptom:=(nb>0), with=F]
}

for (change in c("visit.medical.service.no", "contact.medical.service.no",
                 "no.medication")) {
  whole.users$nb <- with(dt, aggregate((get(change) == "t"),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  whole.users <- whole.users[, which(!grepl(change, colnames(whole.users))), with=FALSE]
  whole.users <- whole.users[,change:=(nb>0), with=F]
}

for (change in c("alter.routine")) {
  dt <- dt[is.na(get(change)), change := -1, with=F]  
  whole.users$nb <- with(dt, aggregate((get(change) > 0),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  whole.users <- whole.users[, which(!grepl(change, colnames(whole.users))), with=FALSE]
  whole.users <- whole.users[,change:=(nb>0), with=F]
}

for (change in c("absent")) {
  whole.users$nb <- with(dt, aggregate((get("alter.routine") == 1),
                                        list(global.id.number=global.id.number),
                                        sum))$x
  whole.users <- whole.users[, which(!grepl(change, colnames(whole.users))), with=FALSE]
  whole.users <- whole.users[,change:=(nb>0), with=F]
}

whole.users <- whole.users[, which(!grepl("nb", colnames(whole.users))), with=FALSE]


dt2 <- dt[!duplicated(dt[,c("symptoms.start.date", "global.id.number"),with=F])]
whole.users$ili <- FALSE
whole.users$nbili <- with(dt2, aggregate(ili,
                                 list(global.id.number=global.id.number),
                                 sum))$x
whole.users$ili <- (whole.users$nbili > 0)

whole.users$ili.fever <- FALSE
whole.users$nbili.fever <- with(dt2, aggregate(ili.fever,
                                 list(global.id.number=global.id.number),
                                 sum))$x
whole.users$ili.fever <- (whole.users$nbili.fever > 0)
whole.users[is.na(ili.fever)]$ili.fever <- F

whole.users$ili.self <- FALSE
whole.users$nbili.self <- with(dt2, aggregate(ili.self,
                                              list(global.id.number=global.id.number),
                                              sum))$x
whole.users$ili.self <- (whole.users$nbili.self > 0)

whole.users$agegroup2 <- cut(whole.users$age, breaks=c(0,20,30,40,50,60,70,80,
                                                max(dt$age, na.rm=T)),
                                                include.lowest=T, right=F) 

whole.users$daycare <- (whole.users$Q6b>0)
whole.users[is.na(daycare)]$daycare <- F

whole.users$frequent.contact <- (whole.users$frequent.contact.children == "t" |
                                 whole.users$frequent.contact.elderly == "t" |
                                 whole.users$frequent.contact.people == "t")


whole.users$smoking <- whole.users$smoke %in% c(1,2,3)

# logistic regressions

whole.users$notvaccinated <- !(whole.users$vaccinated)
whole.users <- whole.users[!is.na(agegroup)]
season <- logistic.regression.or.ci(glm(ili ~ notvaccinated + children +
                                        agegroup + country +
                                        frequent.contact + atrisk +
                                        gender, data=whole.users,
                                        family=binomial))  
season.results <- data.frame(row.names = names(season$OR),
                             pvalue = signif(season$regression.table$coefficients[-1,4], 1),
                             OR = round(season$OR, 2),
                             OR.ci.low = round(season$OR.ci[,1], 2),
                             OR.ci.high = round(season$OR.ci[,2], 2)
                             )

regressions <- data.table(yearweek=levels(factor(dt$week)))
setkey(regressions, yearweek)
for (variable in names(season$OR)) {
  regressions <- regressions[,variable := 0.0,with=F]
  regressions <- regressions[,paste(variable, ".ci.low", sep="") := 0.0,with=F]
  regressions <- regressions[,paste(variable, ".ci.high", sep="") := 0.0,with=F]
}

startweek <- min(dt$week)
for (thisweek in levels(factor(dt$week))) {
  endweek <- thisweek
  week.all <- dt[week >= startweek & week < endweek]
  week.users <- week.all[!duplicated(week.all$bid)]
  week.users$vaccinated <- with(week.all,
                                aggregate(vaccine,
                                          ## list(global.id.number=global.id.number),
                                          list(global.id.number=global.id.number),
                                          sum))$x > 0

  for (symptom in c("fever", "chills", "blocked.runny.nose", "sneezing",
                    "sore.throat", "cough", "shortness.breath", "headache",
                    "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
                    "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache", "other")) { 
    week.users$nb <- with(week.all,
                          aggregate((get(symptom) == "t"),
                                    list(global.id.number=global.id.number),
                                    sum))$x
    week.users <- week.users[, which(!grepl(symptom, colnames(week.users))), with=FALSE]
    week.users <- week.users[,symptom:=(nb>0), with=F]
  }

  for (symptom in c("fever.suddenly")) {
    week.all <- week.all[is.na(get(symptom)), symptom := -1, with=F]  
    week.users$nb <- with(week.all, aggregate((get(symptom) == 0),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(symptom, colnames(week.users))), with=FALSE]
    week.users <- week.users[,symptom:=(nb>0), with=F]
  }

  for (change in c("visit.medical.service.no", "contact.medical.service.no",
                   "no.medication")) {
    week.users$nb <- with(week.all, aggregate((get(change) == "t"),
                                               list(global.id.number=global.id.number),
                                               sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  for (change in c("alter.routine")) {
    week.all <- week.all[is.na(get(change)), change := -1, with=F]  
    week.users$nb <- with(week.all, aggregate((get(change) > 0),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  for (change in c("absent")) {
    week.users$nb <- with(week.all, aggregate((get("alter.routine") == 1),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  week.users <- week.users[, which(!grepl("nb", colnames(week.users))), with=FALSE]


  week.users$ili <- FALSE
  week.users$nbili <- with(week.all, aggregate(ili,
                                               list(global.id.number=global.id.number),
                                               sum))$x
  week.users$ili <- (week.users$nbili > 0)

  week.users$ili.fever <- FALSE
  week.users$nbili.fever <- with(week.all, aggregate(ili.fever,
                                                     list(global.id.number=global.id.number),
                                                     sum))$x
  week.users$ili.fever <- (week.users$nbili.fever > 0)
  week.users <- week.users[is.na(ili.fever), "ili.fever" := F, with=F]

  week.all$ili.self <- (week.all$Q11 == 0)
  week.all[is.na(ili.self)]$ili.self <- FALSE
  week.users$ili.self <- FALSE
  week.users$nbili.self <- with(week.all, aggregate(ili.self,
                                                    list(global.id.number=global.id.number),
                                                    sum))$x
  week.users$ili.self <- (week.users$nbili.self > 0)

  week.users$agegroup2 <- cut(week.users$age, breaks=c(0,20,30,40,50,60,70,80,
                                                max(dt$age, na.rm=T)),
                              include.lowest=T, right=F) 

  week.users$daycare <- (week.users$Q6b>0)
  week.users[is.na(daycare)]$daycare <- F

  week.users$frequent.contact <- (week.users$frequent.contact.children == "t" |
                                  week.users$frequent.contact.elderly == "t" |
                                  week.users$frequent.contact.people == "t")

  week.users$smoking <- week.users$smoke %in% c(1,2,3)

  week.users$notvaccinated <- !(week.users$vaccinated)
  week.regression <-
    logistic.regression.or.ci(glm(ili ~ notvaccinated + children +
                                  agegroup + country +
                                  frequent.contact + atrisk + 
                                  gender,
                                  data=week.users, family=binomial))

  for (i in 1:length(names(week.regression$OR))) {
#  for (variable in names(week.regression$OR)) {
    ## regressions <- regressions[thisweek, variable :=
    ##                            week.regression$OR[[variable]], with=F] 
    regressions <- regressions[thisweek, names(week.regression$OR[i]) :=
                               week.regression$OR[[i]], with=F] 
    regressions <- regressions[thisweek, paste(names(week.regression$OR[i]), ".ci.low", sep="") :=
                               week.regression$OR.ci[i,1], with=F] 
    regressions <- regressions[thisweek, paste(names(week.regression$OR[i]), ".ci.high", sep="") :=
                               week.regression$OR.ci[i,2], with=F] 
  }
}

regressions$year <- as.numeric(substr(regressions$yearweek, 1, 4))
regressions$week <- as.numeric(substr(regressions$yearweek, 6, 7))
regressions$date <- as.Date(strptime(paste(regressions$year, regressions$week*7,sep=" "),format="%Y %j"))+5

for (variable in names(season$OR)) {
#  filename <- gsub("[,\\[)]", "", gsub("\\]", "", names(season$OR)[1]))
  pdf(paste(variable, "3.pdf", sep=""))
  print(
        ggplot(regressions[yearweek > "2011-45" & yearweek < "2012-14"], aes(x=date, y=get(variable)))+
        geom_line()+
        geom_errorbar(aes(ymin = get(paste(variable, ".ci.low", sep="")),
                          ymax = get(paste(variable, ".ci.high", sep=""))))+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Weekly odds ratio")+
        theme_bw(30)+
        scale_x_date(name="")+
        theme(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())+
        geom_abline(intercept=1, slope=0)
        )
  dev.off()
  png(paste(variable, "3.png", sep=""))
  print(
        ggplot(regressions[yearweek > "2011-45" & yearweek < "2012-14"], aes(x=date, y=get(variable)))+
        geom_line()+
        geom_errorbar(aes(ymin = get(paste(variable, ".ci.low", sep="")),
                          ymax = get(paste(variable, ".ci.high", sep=""))))+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Weekly odds ratio")+
        theme_bw(30)+
        scale_x_date(name="")+
        theme(panel.grid.major=element_blank(),
             panel.grid.minor=element_blank())+
        geom_abline(intercept=1, slope=0)
        )
  dev.off()
}

# cumulative regressions

cregressions <- data.table(yearweek=levels(factor(dt$week)))
setkey(cregressions, yearweek)
for (variable in names(season$OR)) {
  cregressions <- cregressions[,variable := 0.0,with=F]
}

## startweek <- min(dt[week != min(dt$week)]$week)
startweek <- min(dt$week)
for (thisweek in levels(factor(dt$week))) {
  endweek <- thisweek
  ## week.all <- dt[country == "uk" & !is.na(agegroup) &
  ##                week >= startweek & week <= endweek] 
  week.all <- dt[!is.na(agegroup) &
                 week >= startweek & week <= endweek] 
  week.users <- week.all[!duplicated(week.all$bid)]
  week.users$vaccinated <- with(week.all,
                                aggregate(vaccine,
                                          ## list(global.id.number=global.id.number),
                                          list(global.id.number=global.id.number),
                                          sum))$x > 0

  for (symptom in c("fever", "chills", "blocked.runny.nose", "sneezing",
                    "sore.throat", "cough", "shortness.breath", "headache",
                    "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
                    "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache", "other")) { 
    week.users$nb <- with(week.all,
                          aggregate((get(symptom) == "t"),
                                    list(global.id.number=global.id.number),
                                    sum))$x
    week.users <- week.users[, which(!grepl(symptom, colnames(week.users))), with=FALSE]
    week.users <- week.users[,symptom:=(nb>0), with=F]
  }

  for (symptom in c("fever.suddenly")) {
    week.all <- week.all[is.na(get(symptom)), symptom := -1, with=F]  
    week.users$nb <- with(week.all, aggregate((get(symptom) == 0),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(symptom, colnames(week.users))), with=FALSE]
    week.users <- week.users[,symptom:=(nb>0), with=F]
  }

  for (change in c("visit.medical.service.no", "contact.medical.service.no",
                   "no.medication")) {
    week.users$nb <- with(week.all, aggregate((get(change) == "t"),
                                               list(global.id.number=global.id.number),
                                               sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  for (change in c("alter.routine")) {
    week.all <- week.all[is.na(get(change)), change := -1, with=F]  
    week.users$nb <- with(week.all, aggregate((get(change) > 0),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  for (change in c("absent")) {
    week.users$nb <- with(week.all, aggregate((get("alter.routine") == 1),
                                              list(global.id.number=global.id.number),
                                              sum))$x
    week.users <- week.users[, which(!grepl(change, colnames(week.users))), with=FALSE]
    week.users <- week.users[,change:=(nb>0), with=F]
  }

  week.users <- week.users[, which(!grepl("nb", colnames(week.users))), with=FALSE]


  week.users$ili <- FALSE
  week.users$nbili <- with(week.all, aggregate(ili,
                                               list(global.id.number=global.id.number),
                                               sum))$x
  week.users$ili <- (week.users$nbili > 0)

  week.users$ili.fever <- FALSE
  week.users$nbili.fever <- with(week.all, aggregate(ili.fever,
                                                     list(global.id.number=global.id.number),
                                                     sum))$x
  week.users$ili.fever <- (week.users$nbili.fever > 0)
  week.users[is.na(ili.fever)]$ili.fever <- F

  week.all$ili.self <- (week.all$Q11 == 0)
  week.all[is.na(ili.self)]$ili.self <- FALSE
  week.users$ili.self <- FALSE
  week.users$nbili.self <- with(week.all, aggregate(ili.self,
                                                    list(global.id.number=global.id.number),
                                                    sum))$x
  week.users$ili.self <- (week.users$nbili.self > 0)

  week.users$agegroup2 <- cut(week.users$age, breaks=c(0,20,30,40,50,60,70,80,
                                                max(dt$age, na.rm=T)),
                              include.lowest=T, right=F) 

  week.users$daycare <- (week.users$Q6b>0)
  week.users[is.na(daycare)]$daycare <- F

  week.users$frequent.contact <- (week.users$frequent.contact.children == "t" |
                                  week.users$frequent.contact.elderly == "t" |
                                  week.users$frequent.contact.people == "t")

  week.users$frequent.contact.with.children <- (week.users$frequent.contact.children == "t")
  week.users$frequent.contact.with.elderly <- (week.users$frequent.contact.elderly == "t")
  week.users$frequent.contact.with.people <- (week.users$frequent.contact.people == "t")

  week.users$smoking <- week.users$smoke %in% c(1,2,3)

  week.users$notvaccinated <- !(week.users$vaccinated)
  week.regression <-
    logistic.regression.or.ci(glm(ili ~ notvaccinated + children +
                                  transport, 
                                  data=week.users, family=binomial))

  week.results <- data.frame(row.names = names(week.regression$OR),
                             pvalue = signif(week.regression$regression.table$coefficients[-1,4], 1),
                             OR = round(week.regression$OR, 2),
                             OR.ci.low = round(week.regression$OR.ci[,1], 2),
                             OR.ci.high = round(week.regression$OR.ci[,2], 2)
                             )

  for (i in 1:length(names(week.regression$OR))) {
  ## for (variable in names(week.regression$OR)) {
  ##   cregressions <- cregressions[thisweek, variable :=
  ##                              week.regression$OR[[variable]], with=F] 
    cregressions <- cregressions[thisweek, names(week.regression$OR[i]) :=
                                 week.regression$OR[[i]], with=F] 
    cregressions <- cregressions[thisweek, paste(names(week.regression$OR[i]), ".ci.low", sep="") :=
                                 week.regression$OR.ci[i,1], with=F] 
    cregressions <- cregressions[thisweek, paste(names(week.regression$OR[i]), ".ci.high", sep="") :=
                                 week.regression$OR.ci[i,2], with=F] 
  }
}

cregressions$year <- as.numeric(substr(cregressions$yearweek, 1, 4))
cregressions$week <- as.numeric(substr(cregressions$yearweek, 6, 7))
cregressions$date <- as.Date(strptime(paste(cregressions$year, cregressions$week*7,sep=" "),format="%Y %j"))+5

for (variable in names(season$OR)) {
  ## filename <- gsub("[,\\[)]", "", gsub("\\]", "", variable))
  ## cat(filename, "\n")
  pdf(paste("c_", variable, "3.pdf", sep=""))
  print(
        ggplot(cregressions[yearweek > "2011-45" & yearweek < "2012-14"], aes(x=date, y=get(variable)))+
        geom_line()+
        geom_errorbar(aes(ymin = get(paste(variable, ".ci.low", sep="")),
                          ymax = get(paste(variable, ".ci.high", sep=""))))+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Cumulative odds ratio")+
        theme_bw(30)+
        scale_x_date(name="")+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())+
        geom_abline(intercept=1, slope=0)
        )
  dev.off()
  png(paste("c_", variable, "3.png", sep=""))
  print(
        ggplot(cregressions[yearweek > "2011-45" & yearweek < "2012-14"], aes(x=date, y=get(variable)))+
        geom_line()+
        geom_errorbar(aes(ymin = get(paste(variable, ".ci.low", sep="")),
                          ymax = get(paste(variable, ".ci.high", sep=""))))+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Cumulative odds ratio")+
        theme_bw(30)+
        scale_x_date(name="")+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())+
        geom_abline(intercept=1, slope=0)
  )
  dev.off()
}

