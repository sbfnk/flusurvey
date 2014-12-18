library(data.table)
library(ggplot2)

co.fever.all.12 <- read.csv("transport_all_fever_201112.csv", sep=",", header=T)
co.fever.all.12$date <- as.Date(strptime(paste(co.fever.all.12$year, co.fever.all.12$week*7,sep=" "),format="%Y %j"))+5

co.fever.all.12 <- read.csv("test.csv", sep=",", header=T)
co.fever.all.12$date <- as.Date(strptime(paste(co.fever.all.12$year, co.fever.all.12$week*7,sep=" "),format="%Y %j"))+5

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
  sum(compare[variable=="transport" & date < enddate]$value) /
  sum(compare[date > startdate & date < enddate &
              variable=="Influenza"]$value)

png(filename="test.png", width=960)
ggplot(compare[date > startdate & date < enddate], aes(x=date, y=value,
  color=variable))+geom_line(lwd=1.5)+scale_y_continuous(name="Weekly incidence (in %)")+theme_bw(30)+scale_x_date(name="")+opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank())+scale_color_brewer(palette="Set1")
dev.off()

pdf(file="test.pdf", width=12, height=6)
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
