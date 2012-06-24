co.all.12 <- read.csv("cohorts_all_201112.csv", sep=",", header=T)
co.country.12 <- read.csv("cohorts_country_201112.csv", sep=",", header=T)
co.all.12$date <- as.Date(strptime(paste(co.all.12$year, co.all.12$week*7-1,sep=" "),format="%Y %j"))
co.country.12$date <- as.Date(strptime(paste(co.country.12$year, co.country.12$week*7-1,sep=" "),format="%Y %j"))+5
co.notired.all.12 <- read.csv("cohorts_all_notired_201112.csv", sep=",", header=T)
co.notired.country.12 <- read.csv("cohorts_country_notired_201112.csv", sep=",", header=T)
co.notired.all.12$date <- as.Date(strptime(paste(co.notired.all.12$year, co.notired.all.12$week*7,sep=" "),format="%Y %j"))+5
co.notired.country.12$date <- as.Date(strptime(paste(co.notired.country.12$year, co.notired.country.12$week*7,sep=" "),format="%Y %j"))+5
co.fever.all.12 <- read.csv("cohorts_all_fever_201112.csv", sep=",", header=T)
co.fever.country.12 <- read.csv("cohorts_country_fever_201112.csv", sep=",", header=T)
co.fever.all.12$date <- as.Date(strptime(paste(co.fever.all.12$year, co.fever.all.12$week*7,sep=" "),format="%Y %j"))+5
co.fever.country.12$date <- as.Date(strptime(paste(co.fever.country.12$year, co.fever.country.12$week*7,sep=" "),format="%Y %j"))+5

co.all.11 <- read.csv("cohorts_all_201011.csv", sep=",", header=T)
co.all.11$date <- as.Date(strptime(paste(co.all.11$year, co.all.11$week*7-1,sep=" "),format="%Y %j"))
co.notired.all.11 <- read.csv("cohorts_all_notired_201011.csv", sep=",", header=T)
co.notired.all.11$date <- as.Date(strptime(paste(co.notired.all.11$year, co.notired.all.11$week*7,sep=" "),format="%Y %j"))+5
co.fever.all.11 <- read.csv("cohorts_all_fever_201011.csv", sep=",", header=T)
co.fever.all.11$date <- as.Date(strptime(paste(co.fever.all.11$year, co.fever.all.11$week*7,sep=" "),format="%Y %j"))+5

co.notired.all.10 <- read.csv("cohorts_all_notired_200910.csv", sep=",", header=T)
co.notired.all.10$date <- as.Date(strptime(paste(co.notired.all.10$year, co.notired.all.10$week*7,sep=" "),format="%Y %j"))+5
co.fever.all.10 <- read.csv("cohorts_all_fever_200910.csv", sep=",", header=T)
co.fever.all.10$date <- as.Date(strptime(paste(co.fever.all.10$year, co.fever.all.10$week*7,sep=" "),format="%Y %j"))+5

co.all <- rbind(co.all.12,co.all.11)
co.notired.all <- rbind(co.notired.all.12,co.notired.all.11,co.notired.all.10)
co.fever.all <- rbind(co.fever.all.12,co.fever.all.11,co.fever.all.10)
co.country <- co.country.12
co.fever.country <- co.fever.country.12
co.notired.country <- co.notired.country.12
