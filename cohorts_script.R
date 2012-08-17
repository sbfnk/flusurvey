#!/usr/bin/Rscript

library(data.table)
library(ggplot2)

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
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file = paste(filebase, ".pdf", sep=""), width=12, height=6)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

