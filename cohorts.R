library(data.table)
library(ggplot2)
library(ISOweek)
library(scales)
library(binom)
library(reshape2)
library(ggthemr)

ggthemr('fresh')

dt <- readRDS("flusurvey_200915.rds")
dt$agegroup <- cut(dt$age, breaks=c(0,18,45,65, max(dt$age, na.rm=T)),
                   include.lowest=T, right=F)
dt$vaccine <- as.numeric(dt$vaccine.this.year==0 & (is.na(dt$vaccine.date) |
                                                        dt$vaccine.date <= dt$date))
dt[, week.date := date - wday(date) + 1]
dt[, symptoms.start.week.date := symptoms.start.date - wday(symptoms.start.date) + 1]
dt[is.na(symptoms.start.week.date), symptoms.start.week.date := week.date]

levels(dt$agegroup) <- c("<18","18-44","45-64","65+")

temp.data <- dt[!is.na(age) & !is.na(ili.fever) & !is.na(vaccine)]

# write.csv(data, "cohorts_fever_201114.raw", quote=F, row.names=F)

## data <- temp.data[, list(non.ili = sum(ili.fever == 0), ili = sum(ili.fever == 1)), by = list(vaccine, atrisk, children, agegroup, week.date)]
## setkey(data, week.date, agegroup, vaccine, atrisk, children)
data <- temp.data[, list(no.vaccine = sum(vaccine == 0), vaccine = sum(vaccine == 1), no.vaccine.ili = sum(vaccine == 0 & ili.fever == 1), vaccine.ili = sum(vaccine == 1 & ili.fever == 1), no.vaccine.no.ili = sum(vaccine == 0 & ili.fever == 0), vaccine.no.ili = sum(vaccine == 1 & ili.fever == 0)), by = list(gender, atrisk, children, agegroup, symptoms.start.week.date)]
data[, cohort.size := min(vaccine, no.vaccine), by = 1:nrow(data)]
data <- data[cohort.size > 0]
data <- data[symptoms.start.week.date <= as.Date(Sys.time())]

data[, nvi := no.vaccine.ili / no.vaccine * cohort.size]
data[, vi := vaccine.ili / vaccine * cohort.size]

setnames(data, "symptoms.start.week.date", "date")
setkey(data, date, agegroup, vaccine, atrisk, children)

data[, season := year(date + 182)]

## vi
ve <- data[, (sum(no.vaccine.ili) / sum(no.vaccine) - sum(vaccine.ili) / sum(vaccine)) / (sum(no.vaccine.ili) / sum(no.vaccine))] * 100

cohorts <- data[, list(cohort.size = sum(cohort.size),
                       unvaccinated = sum(nvi),
                       vaccinated = sum(vi)),
                by = date]
cohorts <- cohorts[cohort.size > 50]
setkey(cohorts, date)

mc <- data.table(melt(cohorts, id.vars = c("date", "cohort.size")))
setnames(mc, "value", "ili")
setnames(mc, "variable", "status")
mc[, season := year(date + 182)]
mc[, season := year(date + 182)]

for (this.season in unique(mc[, season]))
{
    season.min.date <- mc[season == this.season, min(date)]
    mc <- mc[!(date == season.min.date)]
}

confints <- data.table(binom.confint(mc[, ili], mc[, cohort.size],
                                     methods = "lrt"))
mc <- cbind(mc, confints[, list(mean, lower, upper)])
setnames(mc, "mean", "prevalence")

write.csv(data, "cohorts_fever_200915.csv", quote=F, row.names=F)

min.season <- min(mc[, season])

p <- ggplot(mc[season == min.season],
            aes(x = date, y = prevalence * 100,
                ymin = lower * 100, ymax = upper * 100,
                color = status, fill = status))
p <- p + geom_ribbon(alpha = 0.2)
p <- p + geom_line(lwd = 1.2)
for (next.season in setdiff(unique(mc[, season]), min.season))
{
    p <- p + geom_ribbon(data = mc[season == next.season], alpha = 0.2)
    p <- p + geom_line(data = mc[season == next.season], lwd = 1.2)
}
p <- p + scale_color_brewer("", palette = "Set1")
p <- p + scale_fill_brewer("", palette = "Set1")
p <- p + scale_x_date("")
p <- p + scale_y_continuous("Prevalence (in %)")
p <- p + theme(legend.position = "top")
ggsave("cohort_timeline_2015.pdf", p, width = 10)

mc[, year.date := as.Date(sub("^201[0-9]", 2015 + year(date) - season, date),
               format = "%Y-%m-%d"), by = 1:nrow(mc)]

p <- ggplot(mc,
            aes(x = year.date, y = prevalence * 100,
                ymin = lower * 100, ymax = upper * 100,
                color = status, fill = status))
p <- p + geom_ribbon(alpha = 0.2)
p <- p + geom_line(lwd = 1.2)
p <- p + facet_grid(season ~ ., scales = "free_y")
p <- p + scale_color_brewer("", palette = "Set1")
p <- p + scale_fill_brewer("", palette = "Set1")
p <- p + scale_x_date("")
p <- p + scale_y_continuous("Prevalence of ILI + fever (in %)")
p <- p + theme(legend.position = "top")
ggsave("cohort_seasons_2015.pdf", p)

p <- ggplot(mc[season == 2015], 
            aes(x = year.date, y = prevalence * 100,
                ymin = lower * 100, ymax = upper * 100,
                color = status, fill = status))
p <- p + geom_ribbon(alpha = 0.2)
p <- p + geom_line(lwd = 1.2)
p <- p + scale_color_brewer("", palette = "Set1")
p <- p + scale_fill_brewer("", palette = "Set1")
p <- p + scale_x_date("")
p <- p + scale_y_continuous("Prevalence of ILI + fever (in %)")
p <- p + theme(legend.position = "top")
ggsave("cohorts_2015.pdf", p)

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

transport <- data.table(read.csv("transport.csv", quote="''"))
transport.countries <- data.table(transport[0,], country=character(0))

transport$date <- ISOweek2date(paste(transport$year,"-W",sprintf("%02d",transport$week),"-",4,sep=""))

for (country in c("BE","IT","NL","PT","SE","fr","uk")) {
  transport.countries <- rbind(
    transport.countries,
    data.table(read.csv(paste("transport_", country, ".csv", sep=""),
                        quote="''"), country=country) 
    )
}
transport.countries$date <- ISOweek2date(paste(transport.countries$year,"-W",sprintf("%02d",transport.countries$week),"-",4,sep=""))

transport.uk.london <- data.table(read.csv("transport_uk_london.csv", quote="''"))
transport.uk.london$date <- ISOweek2date(paste(transport.uk.london$year,"-W",sprintf("%02d",transport.uk.london$week),"-",4,sep=""))

transport.uk <- data.table(read.csv("transport_uk.csv", quote="''"))
transport.uk$date <- ISOweek2date(paste(transport.uk$year,"-W",sprintf("%02d",transport.uk$week),"-",4,sep=""))

transport.no.uk <- data.table(read.csv("transport_no_uk.csv", quote="''"))
transport.no.uk$date <- ISOweek2date(paste(transport.no.uk$year,"-W",sprintf("%02d",transport.no.uk$week),"-",4,sep=""))

transport.country <- data.table(read.csv("transport_country.csv", quote="''"))
transport.country$date <- ISOweek2date(paste(transport.country$year,"-W",sprintf("%02d",transport.country$week),"-",4,sep=""))

transport.agegroup2.uk <- data.table(read.csv("transport_agegroup2_uk.csv", quote="''"))
transport.agegroup2.uk$date <- ISOweek2date(paste(transport.agegroup2.uk$year,"-W",sprintf("%02d",transport.agegroup2.uk$week),"-",4,sep=""))

transport.agegroup2.employment.uk <- data.table(read.csv("transport_agegroup2_employment_uk.csv", quote="''"))
transport.agegroup2.employment.uk$date <- ISOweek2date(paste(transport.agegroup2.employment.uk$year,"-W",sprintf("%02d",transport.agegroup2.employment.uk$week),"-",4,sep=""))

transport.agegroup2.employment.london.uk <- data.table(read.csv("transport_agegroup2_employment_london_uk.csv", quote="''"))
transport.agegroup2.employment.london.uk$date <- ISOweek2date(paste(transport.agegroup2.employment.london.uk$year,"-W",sprintf("%02d",transport.agegroup2.employment.london.uk$week),"-",4,sep=""))

transport.agegroup2 <- data.table(read.csv("transport_agegroup2.csv", quote="''"))
transport.agegroup2$date <- ISOweek2date(paste(transport.agegroup2$year,"-W",sprintf("%02d",transport.agegroup2$week),"-",4,sep=""))

transport.agegroup2.employment <- data.table(read.csv("transport_agegroup2_employment.csv", quote="''"))
transport.agegroup2.employment$date <- ISOweek2date(paste(transport.agegroup2.employment$year,"-W",sprintf("%02d",transport.agegroup2.employment$week),"-",4,sep=""))


pdf("transport_europe.pdf", width=10, height=5)
ggplot(transport[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_countries.pdf", width=10, height=5)
ggplot(transport.countries[date >= "2012-12-01"],
       aes(x=date, y=value, color=country, linetype=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Dark2")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_country.pdf", width=10, height=5)
ggplot(transport.country[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_london.pdf", width=10, height=5)
ggplot(transport.uk.london[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_no_uk.pdf", width=10, height=5)
ggplot(transport.no.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_agegroup2.pdf", width=10, height=5)
ggplot(transport.agegroup2[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_agegroup2_employment.pdf", width=10, height=5)
ggplot(transport.agegroup2.employment[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_agegroup2_uk.pdf", width=10, height=5)
ggplot(transport.agegroup2.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_agegroup2_employment_uk.pdf", width=10, height=5)
ggplot(transport.agegroup2.employment.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport_agegroup2_employment_london_uk.pdf", width=10, height=5)
ggplot(transport.agegroup2.employment.london.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

using.transport.uk <- transport.uk[variable=="using public transport"]
using.transport.uk$popsize <- using.transport.uk$total*2
using.transport.uk$cases <-
  transport.uk[variable=="using public transport"]$ili+
  transport.uk[variable=="not using public transport"]$ili
using.transport.uk$ili.risk <- using.transport.uk$cases / using.transport.uk$popsize

using.transport.europe <- transport[variable=="using public transport"]
using.transport.europe$popsize <- using.transport.europe$total*2
using.transport.europe$cases <-
  transport[variable=="using public transport"]$ili+
  transport[variable=="not using public transport"]$ili
using.transport.europe$ili.risk <- using.transport.europe$cases / using.transport.europe$popsize

transport2 <- data.table(read.csv("transport2.csv", quote="''"))
transport2.countries <- data.table(transport2[0,], country=character(0))

transport2$date <- ISOweek2date(paste(transport2$year,"-W",sprintf("%02d",transport2$week),"-",4,sep=""))

for (country in c("BE","IT","NL","PT","SE","fr","uk")) {
  transport2.countries <- rbind(
    transport2.countries,
    data.table(read.csv(paste("transport2_", country, ".csv", sep=""),
                        quote="''"), country=country) 
    )
}
transport2.countries$date <- ISOweek2date(paste(transport2.countries$year,"-W",sprintf("%02d",transport2.countries$week),"-",4,sep=""))

transport2.uk.london <- data.table(read.csv("transport2_uk_london.csv", quote="''"))
transport2.uk.london$date <- ISOweek2date(paste(transport2.uk.london$year,"-W",sprintf("%02d",transport2.uk.london$week),"-",4,sep=""))

transport2.uk <- data.table(read.csv("transport2_uk.csv", quote="''"))
transport2.uk$date <- ISOweek2date(paste(transport2.uk$year,"-W",sprintf("%02d",transport2.uk$week),"-",4,sep=""))

transport2.no.uk <- data.table(read.csv("transport2_no_uk.csv", quote="''"))
transport2.no.uk$date <- ISOweek2date(paste(transport2.no.uk$year,"-W",sprintf("%02d",transport2.no.uk$week),"-",4,sep=""))

transport2.country <- data.table(read.csv("transport2_country.csv", quote="''"))
transport2.country$date <- ISOweek2date(paste(transport2.country$year,"-W",sprintf("%02d",transport2.country$week),"-",4,sep=""))

transport2.agegroup2.uk <- data.table(read.csv("transport2_agegroup2_uk.csv", quote="''"))
transport2.agegroup2.uk$date <- ISOweek2date(paste(transport2.agegroup2.uk$year,"-W",sprintf("%02d",transport2.agegroup2.uk$week),"-",4,sep=""))

transport2.agegroup2.employment.uk <- data.table(read.csv("transport2_agegroup2_employment_uk.csv", quote="''"))
transport2.agegroup2.employment.uk$date <- ISOweek2date(paste(transport2.agegroup2.employment.uk$year,"-W",sprintf("%02d",transport2.agegroup2.employment.uk$week),"-",4,sep=""))

transport2.agegroup2.employment.london.uk <- data.table(read.csv("transport2_agegroup2_employment_london_uk.csv", quote="''"))
transport2.agegroup2.employment.london.uk$date <- ISOweek2date(paste(transport2.agegroup2.employment.london.uk$year,"-W",sprintf("%02d",transport2.agegroup2.employment.london.uk$week),"-",4,sep=""))

transport2.agegroup2 <- data.table(read.csv("transport2_agegroup2.csv", quote="''"))
transport2.agegroup2$date <- ISOweek2date(paste(transport2.agegroup2$year,"-W",sprintf("%02d",transport2.agegroup2$week),"-",4,sep=""))

transport2.agegroup2.employment <- data.table(read.csv("transport2_agegroup2_employment.csv", quote="''"))
transport2.agegroup2.employment$date <- ISOweek2date(paste(transport2.agegroup2.employment$year,"-W",sprintf("%02d",transport2.agegroup2.employment$week),"-",4,sep=""))

pdf("transport2_europe.pdf", width=10, height=5)
ggplot(transport2[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_countries.pdf", width=10, height=5)
ggplot(transport2.countries[date >= "2012-12-01"],
       aes(x=date, y=value, color=country, linetype=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Dark2")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_country.pdf", width=10, height=5)
ggplot(transport2.country[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_london.pdf", width=10, height=5)
ggplot(transport2.uk.london[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_no_uk.pdf", width=10, height=5)
ggplot(transport2.no.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_agegroup2.pdf", width=10, height=5)
ggplot(transport2.agegroup2[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_agegroup2_employment.pdf", width=10, height=5)
ggplot(transport2.agegroup2.employment[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_agegroup2_uk.pdf", width=10, height=5)
ggplot(transport2.agegroup2.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_agegroup2_employment_uk.pdf", width=10, height=5)
ggplot(transport2.agegroup2.employment.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

pdf("transport2_agegroup2_employment_london_uk.pdf", width=10, height=5)
ggplot(transport2.agegroup2.employment.london.uk[date >= "2012-12-01"], aes(x=date, y=value, color=variable))+
  theme_bw(20)+
  geom_line(lwd=1.5)+
  scale_color_brewer(palette="Set1")+
  theme(legend.title=element_blank(), legend.position="bottom")+
  scale_y_continuous("Incidence", labels=percent)+
  scale_x_date("")
dev.off()

using.transport2.uk <-
  transport2.uk[variable=="using public transport 1.5hrs+"]
using.transport2.uk$popsize <- using.transport2.uk$total*2
using.transport2.uk$cases <-
  transport2.uk[variable=="using public transport 1.5hrs+"]$ili+
  transport2.uk[variable=="not using public transport"]$ili
using.transport2.uk$ili.risk <- using.transport2.uk$cases / using.transport2.uk$popsize

1-pbinom(53,850,0.05)
1-pbinom(52,850,0.05)
1-pbinom(52,850,0.06)
1-pbinom(52,850,0.07)
1-pbinom(52,850,0.08)
1-pbinom(52,850,0.09)
1-pbinom(52,850,0.1)

1-pbinom(12,150,0.05)
1-pbinom(11,150,0.05)
1-pbinom(11,150,0.06)
1-pbinom(11,150,0.07)
1-pbinom(11,150,0.08)
1-pbinom(11,150,0.09)
1-pbinom(11,150,0.1)
1-pbinom(11,150,0.15)
