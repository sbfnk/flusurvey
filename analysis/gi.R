library(data.table)

dt11 <- readRDS("flusurvey_201011.rds")
dt12 <- readRDS("flusurvey_201112.rds")
dt13 <- readRDS("flusurvey_201213.rds")
dt14 <- readRDS("flusurvey_201314.rds")

## merge what we can merge
join.vertical <- function (...) {
    x <- list(...)
    for (i in 1:(length(x) - 1)) {
        for (j in 2:(i + 1)) {
            names.diff <- setdiff(names(x[[j - 1]]), names(x[[i + 1]]))
            if (length(names.diff) > 0) {
                x[[j - 1]] <- x[[j - 1]][, !names.diff, with = F]
            }
        }
        names.diff.reverse <- setdiff(names(x[[i + 1]]), names(x[[i]]))
        if (length(names.diff.reverse) > 0) {
            x[[i + 1]] <- x[[i + 1]][, !names.diff.reverse, with = F]
        }
    }
    res <- do.call("rbind", c(x, list(use.names = T)))
}

dt <- join.vertical(dt14, dt13, dt12, dt11)

dt$agegroup <- cut(dt$age, breaks=c(0,18,45,65, max(dt$age, na.rm=T)),
                   include.lowest=T, right=F)
dt[, week := date2ISOweek(date)]
dt[, week.date := ISOweek2date(sub("-[1-7]", "-1", week))]
dt[, symptoms.start.week := date2ISOweek(symptoms.start.date)]
dt[, symptoms.start.week.date := ISOweek2date(sub("-[1-7]", "-1", symptoms.start.week))]
dt[is.na(symptoms.start.week.date), symptoms.start.week.date := week.date]

levels(dt$agegroup) <- c("<18","18-44","45-64","65+")

dt[, gi := as.integer(diarrhoea | vomiting)]
dt <- dt[!is.na(gi) & !is.na(ili)]

gi.summary <- dt[, list(reports = length(global.id.number),
                        ili = sum(ili == 1),
                        gi = sum(gi == 1),
                        ili.gi = sum(ili == 1 & gi == 1)),
                 by = list(symptoms.start.week.date, agegroup)]
setnames(gi.summary, "symptoms.start.week.date", "week.beginning")
setkey(gi.summary, week.beginning, agegroup)
gi.summary[, season := year(week.beginning + 182)]

gi.summary.noage <- gi.summary[, list(reports = sum(reports), ili = sum(ili),
                                      gi = sum(gi), ili.gi = sum(ili.gi)), 
                               by = list(week.beginning, season)]
gi.summary.noage <- gi.summary.noage[season > 2010 & season < 2015]
# cleaning
for (this.season in unique(gi.summary.noage[, season]))
{
    mean.reports <- mean(gi.summary.noage[season == this.season, reports])
    gi.summary.noage <- gi.summary.noage[!(season == this.season & reports < mean.reports / 2)]
}

gi.summary <- gi.summary[week.beginning %in% gi.summary.noage[, week.beginning]]
gi.summary <- gi.summary[!is.na(agegroup)]

gi.summary.plot <- copy(gi.summary)
gi.summary.plot[, gi.non.ili := (gi - ili.gi)/ reports]
gi.summary.plot[, ili := ili/ reports]
gi.summary.plot[, gi := gi/ reports]
gi.summary.plot[, ili.gi := NULL]

mgi <- data.table(melt(gi.summary.plot, id.vars = c("week.beginning", "reports", "season")))
min.season <- min(mgi[, season])

mgi[, week.beginning.season := as.Date(sub("^201[0-9]",
                            2015 + year(week.beginning) - season,
                           week.beginning),
               format = "%Y-%m-%d"), by = 1:nrow(mgi)]

p <- ggplot(mgi,
            aes(x = week.beginning.season, y = value * 100,
                color = variable))
p <- p + geom_line(lwd = 1.2)
p <- p + facet_grid(season ~ ., scales = "free_y")
p <- p + scale_color_brewer("", palette = "Set1", labels = c("ILI", "GI", "GI without ILI"))
p <- p + scale_x_date("")
p <- p + scale_y_continuous("Prevalence (in %)")
p <- p + theme(legend.position = "top")
ggsave("gi_seasons.pdf", p)

gi.summary[, season := NULL]
write.table(gi.summary, "flusurvey_gi.csv", quote = FALSE, sep = ",", row.names = FALSE)

# GI stuff
## dt$gi.or <- as.numeric(dt$diarrhoea == "t" | dt$vomiting == "t" | dt$nausea == "t")
## dt$gi.and <- as.numeric(dt$diarrhoea == "t" & dt$vomiting == "t" & dt$nausea == "t")
## dt$gi.or.novom <- as.numeric(dt$diarrhoea == "t" | dt$nausea == "t")
## dt$gi.and.novom <- as.numeric(dt$diarrhoea == "t" & dt$nausea == "t")

## dt$newgi.or <- dt$gi.or
## dt$newgi.and <- dt$gi.and
## dt$newgi.or.novom <- dt$gi.or.novom
## dt$newgi.and.novom <- dt$gi.and.novom

## dt[same==0, newgi.or := 0]
## dt[same==0, newgi.and := 0]
## dt[same==0, newgi.or.novom := 0]
## dt[same==0, newgi.and.novom := 0]

## r.or <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or,
##             row.vars=1)
## r.and <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and,
##             row.vars=1)
## r.or.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or.novom,
##             row.vars=1)
## r.and.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and.novom,
##             row.vars=1)

## gi.or.raw.data <- data.frame(expand.grid(rev(attr(r.or, "row.vars"))),
##                                    unclass(r.or))
## gi.and.raw.data <- data.frame(expand.grid(rev(attr(r.and, "row.vars"))),
##                                    unclass(r.and))
## gi.or.novom.raw.data <- data.frame(expand.grid(rev(attr(r.or.novom, "row.vars"))),
##                                    unclass(r.or.novom))
## gi.and.novom.raw.data <- data.frame(expand.grid(rev(attr(r.and.novom, "row.vars"))),
##                                    unclass(r.and.novom))

## names(gi.or.raw.data) <- c("Week", "nongi", "gi")
## names(gi.and.raw.data) <- c("Week", "nongi", "gi")
## names(gi.or.novom.raw.data) <- c("Week", "nongi", "gi")
## names(gi.and.novom.raw.data) <- c("Week", "nongi", "gi")

## gi.or.raw.data$gi.incidence <-
##   gi.or.raw.data$gi / (gi.or.raw.data$nongi + gi.or.raw.data$nongi)
## gi.and.raw.data$gi.incidence <-
##   gi.and.raw.data$gi / (gi.and.raw.data$nongi + gi.and.raw.data$nongi)
## gi.or.novom.raw.data$gi.incidence <-
##   gi.or.novom.raw.data$gi / (gi.or.novom.raw.data$nongi + gi.or.novom.raw.data$nongi)
## gi.and.novom.raw.data$gi.incidence <-
##   gi.and.novom.raw.data$gi / (gi.and.novom.raw.data$nongi + gi.and.novom.raw.data$nongi)

## gi.or.12 <- gi.or.raw.data[-c(1:3, 22:26),]
## gi.and.12 <- gi.and.raw.data[-c(1:3, 22:26),]
## gi.or.novom.12 <- gi.or.novom.raw.data[-c(1:3, 22:26),]
## gi.and.novom.12 <- gi.and.novom.raw.data[-c(1:3, 22:26),]

## write.csv(gi.or.12, "gi_or_201112.csv", quote=F, row.names=F)
## write.csv(gi.and.12, "gi_and_201112.csv", quote=F, row.names=F)
## write.csv(gi.or.novom.12, "gi_or_novom_201112.csv", quote=F, row.names=F)
## write.csv(gi.and.novom.12, "gi_and_novom_201112.csv", quote=F, row.names=F)
