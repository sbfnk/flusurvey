library('data.table')
library('ISOweek')
library('stringr')
library('ggthemr')

ggthemr('fresh')

cohorts <- data.table(read.csv("vaccination_cohorts_fever.csv"))
cohorts[, date := ISOweek2date(paste0(year, "-W", str_pad(week, 2, pad = "0"), "-1"))]
cohorts[, variable := factor(gsub("'", "", variable))]

first_season <- cohorts[date > as.Date("2013-11-15") & date < as.Date("2014-4-1")]
second_season <- cohorts[date > as.Date("2014-9-1") & date < as.Date("2015-4-1")]

p <- ggplot(first_season, aes(x = date, y = value * 100, color = variable))
p <- p + geom_line(lwd = 1.5)
p <- p + geom_line(data = second_season, lwd = 1.5)
p <- p + scale_color_brewer("", palette = "Set1")
p <- p + theme(legend.position = "top")
p <- p + scale_y_continuous("Prevalence of ILI (in %)")
p <- p + scale_x_date("")
p <- p + expand_limits(y = 10)

ggsave("vaccine_cohorts_2015.pdf", p)
