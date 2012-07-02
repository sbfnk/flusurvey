library(data.table)
library(ggplot2)

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

startdate <- as.Date("2011-11-28")
enddate <- as.Date("2012-04-19")
virology <- data.table(read.csv("virology_clean.csv", header=T, sep=","))
influenza <- virology[strain=="Influenza"]
setnames(influenza, "strain", "variable")
setnames(influenza, "samples", "value")
compare <- data.table(rbind(influenza, co.fever.all.12))
compare$date <- as.Date(compare$date)
compare[variable=="Influenza"]$value <-
  compare[variable=="Influenza"]$value *
  sum(compare[variable=="vaccinated" & date < enddate]$value) /
  sum(compare[date > startdate & date < enddate &
              variable=="Influenza"]$value)

png(filename="cohorts.png", width=960)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file="cohorts.pdf", width=12, height=6)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

png(filename="cohorts_no_virology.png", width=960)
ggplot(compare[variable != "Influenza" & date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file="cohorts_no_virology.pdf", width=12, height=6)
ggplot(compare[variable != "Influenza" & date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()
