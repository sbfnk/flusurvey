library(ISOweek)
library(data.table)
library(ggplot2)
library(reshape)

data <- readRDS("flusurvey_200914_raw.rds")
ct <- data$contact[!is.na(week)]

avg.weekly.physical.contacts <- data.table(ddply(ct, .(week), summarise, contacts = if (length(physical) > 5) {mean(physical)} else {NA}))
avg.weekly.physical.contacts <- avg.weekly.physical.contacts[, year := sub("-.*", "", week)]
avg.weekly.physical.contacts <- avg.weekly.physical.contacts[, yearweek := as.numeric(sub(".*-", "", week))]
avg.weekly.physical.contacts <- avg.weekly.physical.contacts[,ISOweek := paste(year, "-W", sprintf("%02i", yearweek), "-1", sep="")]
avg.weekly.physical.contacts <- avg.weekly.physical.contacts[,date := ISOweek2date(ISOweek)]

ct <- ct[, yearweek := as.numeric(sub(".*-", "", week))]
avg.week.physical.contacts <- data.table(ddply(ct, .(yearweek), summarise, contacts = if (length(physical) > 5) {mean(physical)} else {NA}))

ggplot(avg.weekly.physical.contacts, aes(x=date, y = contacts))+geom_point()
ggplot(ct, aes(x=factor(yearweek), y = physical))+geom_boxplot()
ggplot(ct, aes(x=physical, y = conversational))+geom_jitter()+scale_x_log10()+scale_y_log10()
