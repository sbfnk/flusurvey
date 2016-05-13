library('flusurvey')
library('reshape2')
library('data.table')

dt <- extract_data(data = "flusurvey_raw_2012_2015.rds")

dt[, self.gi := as.integer(what.do.you.think == "gastro")]
dt[, self.ili := as.integer(what.do.you.think == "ili")]
dt[, visit.medical.service := as.integer(visit.medical.service.no == "f")]
dt[, gi.think.gi := as.integer(gi == 1 & self.gi == 1)]
dt[, ili.gi := as.integer(gi == 1 & ili == 1)]
dt[, ili.gi.think.gi := as.integer(ili.gi == 1 & self.gi == 1)]
dt[, ili.gi.think.ili := as.integer(ili.gi == 1 & self.ili == 1)]
dt[, gi.visit.hs := as.integer(gi == 1 & visit.medical.service == 1)]
dt[, ili.gi.visit.hs := as.integer(ili.gi == 1 & visit.medical.service == 1)]

vars <- c("ili", "gi", "ili.gi", "gi.think.gi", "ili.gi", "ili.gi.think.gi", "ili.gi.think.ili", "gi.visit.hs", "ili.gi.visit.hs")

gi_incidence <- get_incidence(dt, incidence.columns = vars, by = "agegroup")

wgi <- dcast(gi_incidence, week + season + agegroup + N ~ type, value.var = "new.cases")
for (var in vars)
{
    wgi[is.na(get(var)), paste(var) := 0]
}

round(dt[gi == 1 & ili.gi == 0, prop.table(table(what.do.you.think))], 2)
## what.do.you.think
## allergy_hay_fever       common_cold         dont_know            gastro 
##              0.01              0.15              0.23              0.29 
##               ili             other            asthma 
##              0.06              0.26              0.00 

round(dt[gi == 1 & ili.gi == 1, prop.table(table(what.do.you.think))], 2)

## proportion of GI (but not ILI GI) who think they have GI
wgi[, sum(gi.think.gi - ili.gi.think.gi) / sum(gi - ili.gi)]
## [1] 0.2895166

wgi[, sum(ili.gi.think.gi) / sum(ili.gi)]
## [1] 0.1282468

wgi[, sum(ili.gi.think.ili) / sum(ili.gi)]
## [1] 0.3961039

wgi[, sum(gi.visit.hs - ili.gi.visit.hs) / sum(gi - ili.gi)]
## [1] 0.1205866

wgi[, sum(ili.gi.visit.hs) / sum(ili.gi)]
## [1] 0.1915584

write.table(wgi, "flusurvey_gi.csv", quote = FALSE, sep = ",", row.names = FALSE)
