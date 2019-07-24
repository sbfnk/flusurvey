library('flusurvey')
library('reshape2')
library('data.table')

dt <- extract_data(data = "data/flusurvey_raw_2010_2018.rds",
                   years=2012:2015, surveys=c("background", "symptom"),
                   age_breaks=c(5, 65))

dt[, self.gi := factor(ifelse(what.do.you.think == "gastro", "t", "f"), levels=c("f", "t"))]
dt[, self.ili := factor(ifelse(what.do.you.think == "ili", "t", "f"), levels=c("f", "t"))]
dt[, visit.medical.service := factor(ifelse(visit.medical.service.no == "f", "t",) "f")]
dt[, gi.think.gi := factor(ifelse(gi == "t" & self.gi == "t", "t", "f"), levels=c("f", "t"))]
dt[, gi.fever := factor(ifelse(gi == "t" & fever == "t", "t", "f"), levels=c("f", "t"))]
dt[, gi.noro := factor(ifelse(gi == "t" & (fever == "t" | headache == "t" | muscle.and.or.joint.pain == "t"), "t", "f"), levels=c("f", "t"))]
dt[, gi.not.ili := factor(ifelse(gi == "t" & ili == "f", "t", "f"), levels=c("f", "t"))]
dt[, ili.gi := factor(ifelse(gi == "t" & ili == "t", "t", "f"), levels=c("f", "t"))]
dt[, ili.gi.think.gi := factor(ifelse(ili.gi == "t" & self.gi == "t", "t", "f"), levels=c("f", "t"))]
dt[, ili.gi.think.ili := factor(ifelse(ili.gi == "t" & self.ili == "t", "t", "f"), levels=c("f", "t"))]
dt[, gi.visit.hs := factor(ifelse(gi == "t" & visit.medical.service == "t", "t", "f"), levels=c("f", "t"))]
dt[, ili.gi.visit.hs := factor(ifelse(ili.gi == "t" & visit.medical.service == "t", "t", "f"), levels=c("f", "t"))]

vars <- c("ili", "gi", "gi.not.ili", "self.gi", "self.ili", "ili.gi", "gi.think.gi", "gi.fever", "gi.noro", "ili.gi", "ili.gi.think.gi", "ili.gi.think.ili", "gi.visit.hs", "ili.gi.visit.hs")

gi_incidence <- get_incidence(dt, incidence.columns = vars, by = "agegroup")

for (var in vars)
{
    gi_incidence[is.na(get(var)), paste(var) := 0]
}

round(dt[gi == "t" & ili.gi == "f", prop.table(table(what.do.you.think))], 2)
round(dt[gi == "t" & ili.gi == "t", prop.table(table(what.do.you.think))], 2)

## proportion of GI (but not ILI GI) who think they have GI
gi_incidence[, sum(gi.think.gi - ili.gi.think.gi) / sum(gi - ili.gi)]
## [1] 0.2895166

gi_incidence[, sum(ili.gi.think.gi) / sum(ili.gi)]
## [1] 0.1282468

gi_incidence[, sum(ili.gi.think.ili) / sum(ili.gi)]
## [1] 0.3961039

gi_incidence[, sum(gi.visit.hs - ili.gi.visit.hs) / sum(gi - ili.gi)]
## [1] 0.1205866

gi_incidence[, sum(ili.gi.visit.hs) / sum(ili.gi)]
## [1] 0.1915584

write.table(gi_incidence, "flusurvey_gi.csv", quote = TRUE, sep = ",", row.names = FALSE)

## new

library('tidyverse')

incidence <- gi_incidence %>%
  gather(metric, incidence,
         starts_with("gi"), starts_with("ili"), starts_with("self")) %>%
  replace_na(list(incidence=0)) %>% 
  filter(!is.na(agegroup), N>5) %>%
  mutate(rate=incidence/N)

all <- gi_incidence %>%
  gather(metric, incidence,
         starts_with("gi"), starts_with("ili"), starts_with("self")) %>%
  group_by(week, season, metric) %>%
  summarise(incidence=sum(incidence, na.rm=TRUE),
            N=sum(N, na.rm=TRUE)) %>%
  ungroup %>%
  filter(N>100) %>% 
  mutate(rate=incidence/N)

monthly_incidence <- gi_monthly_incidence %>%
  gather(metric, incidence,
         starts_with("gi"), starts_with("ili"), starts_with("self")) %>%
  replace_na(list(incidence=0)) %>% 
  filter(!is.na(agegroup), month(month) %in% c(11, 12, 1:3)) %>%
  mutate(rate=incidence/N)

dt[start.date.corrected == "f" & !is.na(symptoms.start.date) & !is.na(symptoms.end.date), duration:=symptoms.end.date-symptoms.start.date]

write.table(dt[gi=="t"], "flusurvey_gi_raw.csv", quote = TRUE, sep = ",", row.names = FALSE)

p <- ggplot(gi_duration, aes(x=duration, y=..density..)) + geom_histogram(binwidth=1) + xlab("Duration in days") + ylab("Proportion")
save_plot("gi_duration.pdf", p)
