#!/usr/bin/Rscript

library(data.table)
library(ggplot2)
library(reshape2)

cmd_args <- commandArgs();
filename <- cmd_args[length(cmd_args)];

data <- read.csv(filename, sep=",", header=T)
data$date <- as.Date(strptime(paste(data$year, data$week*7,sep=" "),format="%Y %j"))+5

startdate <- as.Date("2011-11-28")
enddate <- as.Date("2012-04-19")
virology <- data.table(read.csv("virology_clean.csv", header=T, sep=","))
influenza <- virology[strain=="Influenza"]
setnames(influenza, "strain", "variable")
setnames(influenza, "samples", "value")
compare <- data.table(rbind(influenza, data))
compare$date <- as.Date(compare$date)

maxsum <- 0
for (var in levels(data$variable)) {
  cursum <-
    sum(compare[variable==var & date > startdate & date < enddate]$value)
  if (cursum > maxsum) {
    maxsum <- cursum
  }
}


compare[variable=="Influenza"]$value <-
       compare[variable=="Influenza"]$value * maxsum /
       sum(compare[date > startdate & date < enddate &
                                 variable=="Influenza"]$value)

filebase <- sapply(strsplit(basename(filename),"\\."),
                   function(x) paste(x[1:(length(x)-1)], collapse="."))
     
png(file = paste(filebase, ".png", sep=""), width=960)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+theme(panel.grid.major=element_blank(),
  panel.grid.minor=element_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file = paste(filebase, ".pdf", sep=""), width=12, height=6)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+theme(panel.grid.major=element_blank(),
  panel.grid.minor=element_blank())+scale_color_brewer(palette="Set1")
dev.off()

dt <- data.table(dcast(data, week~variable))
dt$year <- 2012
dt[week>15]$year <- 2011
dt$date <- as.Date(strptime(paste(dt$year, dt$week*7,sep=" "),format="%Y %j"))+5
setnames(dt, "'vaccinated'", "vaccinated")
setnames(dt, "'unvaccinated'", "unvaccinated")

pdf(file = paste(filebase, "_div.pdf", sep=""), width=12, height=6)
ggplot(dt[date > startdate & date < enddate], aes(x=date, y=unvaccinated/vaccinated))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+theme(panel.grid.major=element_blank(),
  panel.grid.minor=element_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file = paste(filebase, "_or.pdf", sep=""), width=12, height=6)
gplot(dt[date > startdate & date < enddate], aes(x=date, y=unvaccinated/100*(1-vaccinated/100)/((1-unvaccinated/100)*vaccinated/100)))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+theme(panel.grid.major=element_blank(),
  panel.grid.minor=element_blank())+scale_color_brewer(palette="Set1")
dev.off()

