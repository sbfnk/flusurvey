co.all <- read.csv("cohorts_all.dat", sep=",", header=T)
co.country <- read.csv("cohorts_country.dat", sep=",", header=T)

co.all$date <- as.Date(strptime(paste(co.all$year, co.all$week*7,sep=" "),format="%Y %j"))+5
co.country$date <- as.Date(strptime(paste(co.country$year, co.country$week*7,sep=" "),format="%Y %j"))+5

co.notired.all <- read.csv("cohorts_notired_all.dat", sep=",", header=T)
co.notired.country <- read.csv("cohorts_notired_country.dat", sep=",", header=T)

co.notired.all$date <- as.Date(strptime(paste(co.notired.all$year, co.notired.all$week*7,sep=" "),format="%Y %j"))+5
co.notired.country$date <- as.Date(strptime(paste(co.notired.country$year, co.notired.country$week*7,sep=" "),format="%Y %j"))+5

co.fever.all <- read.csv("cohorts_fever_all.dat", sep=",", header=T)
co.fever.country <- read.csv("cohorts_fever_country.dat", sep=",", header=T)

co.fever.all$date <- as.Date(strptime(paste(co.fever.all$year, co.fever.all$week*7,sep=" "),format="%Y %j"))+5
co.fever.country$date <- as.Date(strptime(paste(co.fever.country$year, co.fever.country$week*7,sep=" "),format="%Y %j"))+5
