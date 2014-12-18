library(data.table)
library(ggplot2)

##compare <- dt2[date > "2012-02-19" & date < "2012-03-05" & !is.na(ili) &
##               !is.na(birthdate) & birthdate < as.Date("2012-03-01"),]
 compare <- dt2[!is.na(ili) & !is.na(birthdate) & birthdate <
                as.Date("2012-04-01"),] 
#compare$global_id <-  factor(compare$global_id)
compare$norisk <- factor(compare$norisk)
compare$atrisk <- compare$norisk
levels(compare$atrisk) <- c(1,0)
compare$atrisk <- as.numeric(paste(compare$atrisk))
compare$age <- apply(compare, 1, function(x) { age_years(x["birthdate"],
                                                         x["date"])})
compare$agegroup <- cut(compare$age, breaks=c(0,18,45,65, max(compare$age)), include.lowest=T)
compare$vaccine <- (compare$vaccine.this.year == 0)

ds <- compare[!duplicated(compare$global_id_number)]
ds$vaccinated <- with(compare, aggregate(vaccine,
                                         list(global_id_number=global_id_number),
                                         sum))$x > 0
ds$date.vaccine <- with(compare, aggregate(vaccine,
                                           list(global_id_number=global_id_number),
                                           sum))$x > 0

keen <- (compare[nReports>15])
keen[global_id_number %in%
     ds2[date.vaccine=="" & vaccine==T]$global_id_number]$date.vaccine <-
  "2011-11-01"

subsample <- keen[date > "2012-02-19" & date < "2012-03-05" & !is.na(ili) & 
                  !is.na(birthdate) & birthdate < as.Date("2012-03-01"),]
ds2 <- subsample[!duplicated(subsample$global_id_number)]
ds2$vaccinated <- with(subsample,
                       aggregate(vaccine,
                                 list(global_id_number=global_id_number),
                                 sum))$x > 0
ds2$not.vaccinated <- with(subsample,
                           aggregate(vaccine.this.year,
                                     list(global_id_number=global_id_number),
                                     sum))$x
length(unique(subsample[age<65 & atrisk==1 & as.Date(date.vaccine) <
                   as.Date("2011-11-07")]$global_id_number)) / 
  length(unique(subsample[age<65 & atrisk==1]$global_id_number))
length(unique(subsample[age>=65 & as.Date(date.vaccine) <
                   as.Date("2011-11-07")]$global_id_number)) / 
  length(unique(subsample[age>=65]$global_id_number))
length(unique(subsample[pregnant==T & as.Date(date.vaccine) <
                   as.Date("2011-11-07")]$global_id_number)) / 
  length(unique(subsample[pregnant==T]$global_id_number))

length(unique(subsample[age<65 & atrisk==1 & as.Date(date.vaccine) <
                   as.Date("2012-03-05")]$global_id_number)) / 
  length(unique(subsample[age<65 & atrisk==1]$global_id_number))
length(unique(subsample[age>=65 & as.Date(date.vaccine) <
                   as.Date("2012-03-05")]$global_id_number)) / 
  length(unique(subsample[age>=65]$global_id_number))
length(unique(subsample[pregnant==T & as.Date(date.vaccine) <
                   as.Date("2012-03-05")]$global_id_number)) / 
  length(unique(subsample[pregnant==T]$global_id_number))



age_years <- function(from, to)
{
     lt <- as.POSIXlt(c(from, to))
     age <- lt$year[2] - lt$year[1]
     mons <- lt$mon + lt$mday/50
     if(mons[2] < mons[1]) age <- age -1
     age
}

ds$age <-  0
ds$ili <- FALSE

ds$nbili <- with(compare, aggregate(ili,
                                    list(global_id_number=global_id_number),
                                    sum))$x
ds$ili <- (ds$nbili > 0)
ds$age <- apply(ds, 1, function(x) { age_years(as.Date(x["birthdate"]),
                                    as.Date("2012-02-19"))})

ds[is.na(nb.household.0.4)]$nb.household.0.4 <- 0
ds[is.na(nb.household.5.18)]$nb.household.5.18 <- 0
ds[is.na(nb.household.19.64)]$nb.household.19.64 <- 0
ds[is.na(nb.household.65.)]$nb.household.65. <- 0

nrow(ds) # 1350
nrow(ds[ili>0]) # 103

nrow(ds[age < 65 & atrisk > 0 & ili > 0]) # 18
nrow(ds[age < 65 & atrisk > 0 & ili > 0]) / nrow(ds[age < 65 & atrisk > 0]) # 0.1028571
nrow(ds[age < 65 & atrisk > 0 & ili > 0 & ds$vaccine.this.year == 0]) # 14
nrow(ds[age < 65 & atrisk > 0 & ili > 0 & ds$vaccine.this.year == 0]) /
  nrow(ds[age < 65 & atrisk > 0 & ds$vaccine.this.year == 0]) # 0.1060606
nrow(ds[age < 65 & atrisk > 0 & ili > 0 & ds$vaccine.this.year == 1]) # 4
nrow(ds[age < 65 & atrisk > 0 & ili > 0 & ds$vaccine.this.year == 1]) /
  nrow(ds[age < 65 & atrisk > 0 & ds$vaccine.this.year == 1]) # 0.1060606

nrow(ds[age >= 65 & ili > 0]) # 9
nrow(ds[age >= 65 & ili > 0]) / nrow(ds[age >= 65]) # 0.0483871
nrow(ds[age >= 65 & ili > 0 & ds$vaccine.this.year == 0]) # 6
nrow(ds[age >= 65 & ili > 0 & ds$vaccine.this.year == 0]) /
  nrow(ds[age >= 65 & ds$vaccine.this.year == 0]) # 0.04195804
nrow(ds[age >= 65 & ili > 0 & ds$vaccine.this.year == 1]) # 3
nrow(ds[age >= 65 & ili > 0 & ds$vaccine.this.year == 1]) /
  nrow(ds[age >= 65 & ds$vaccine.this.year == 1]) # 0.06976744

nrow(ds[pregnant == 0 & ili > 0]) # 0
nrow(ds[pregnant == 0 & ili > 0]) / nrow(ds[pregnant == 0]) # 0
nrow(ds[pregnant == 0 & ili > 0 & ds$vaccine.this.year == 0]) # 0
nrow(ds[pregnant == 0 & ili > 0 & ds$vaccine.this.year == 0]) /
  nrow(ds[pregnant == 0 & ds$vaccine.this.year == 0]) # 0
nrow(ds[pregnant == 0 & ili > 0 & ds$vaccine.this.year == 1]) # 0
nrow(ds[pregnant == 0 & ili > 0 & ds$vaccine.this.year == 1]) /
  nrow(ds[pregnant == 0 & ds$vaccine.this.year == 1]) # 0

nrow(ds[age >= 18 & age <= 24 & ili > 0]) # 5
nrow(ds[age >= 18 & age <= 24 & ili > 0]) / nrow(ds[age >= 18 & age <= 24]) # 0.1
nrow(ds[age >= 25 & age <= 34 & ili > 0]) # 24
nrow(ds[age >= 25 & age <= 34 & ili > 0]) / nrow(ds[age >= 25 & age <= 34]) # 0.09160305
nrow(ds[age >= 35 & age <= 44 & ili > 0]) # 9
nrow(ds[age >= 35 & age <= 44 & ili > 0]) / nrow(ds[age >= 35 & age <= 44]) # 0.03930131
nrow(ds[age >= 45 & age <= 54 & ili > 0]) # 24
nrow(ds[age >= 45 & age <= 54 & ili > 0]) / nrow(ds[age >= 45 & age <= 54]) # 0.08988764
nrow(ds[age >= 55 & age <= 64 & ili > 0]) # 25
nrow(ds[age >= 55 & age <= 64 & ili > 0]) / nrow(ds[age >= 55 & age <= 64]) # 0.08532423
nrow(ds[age >= 65 & age <= 74 & ili > 0]) # 8
nrow(ds[age >= 65 & age <= 74 & ili > 0]) / nrow(ds[age >= 65 & age <= 74]) # 0.05063291
nrow(ds[age >= 75 & ili > 0]) # 1
nrow(ds[age >= 75 & ili > 0]) / nrow(ds[age >= 75]) # 0.03571429

nrow(ds[gender == 1 & ili > 0]) # 80
nrow(ds[gender == 1 & ili > 0]) / nrow(ds[gender == 1]) # 0.09937888
nrow(ds[gender == 0 & ili > 0]) # 23
nrow(ds[gender == 0 & ili > 0]) / nrow(ds[gender == 0]) # 0.02857143

nrow(ds[cough == "t" & ili > 0]) # 65
nrow(ds[cough == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.631068
nrow(ds[sore.throat == "t" & ili > 0]) # 63
nrow(ds[sore.throat == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.6116505
nrow(ds[muscle.and.or.joint.pain == "t" & ili > 0]) # 44
nrow(ds[muscle.and.or.joint.pain == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.4271845
nrow(ds[fever.suddenly == 0 & ili > 0]) # 34
nrow(ds[fever.suddenly == 0 & ili > 0]) / nrow(ds[ili > 0]) # 0.3300971
nrow(ds[shortness.breath == "t" & ili > 0]) # 31
nrow(ds[shortness.breath == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.3009709
nrow(ds[(chills == "t" | blocked.runny.nose == "t" | sneezing == "t" |
         headache == "t" | chest.pain == "t" | tired == "t" |
         loss.appetite == "t" | phlegm == "t" | watery.eyes == "t" |
         nausea == "t" | vomiting == "t" | diarrhoea == "t" |
         stomach.ache == "t" | other == "t") & ili > 0]) # 87
nrow(ds[(chills == "t" | blocked.runny.nose == "t" | sneezing == "t" |
         headache == "t" | chest.pain == "t" | tired == "t" |
         loss.appetite == "t" | phlegm == "t" | watery.eyes == "t" |
         nausea == "t" | vomiting == "t" | diarrhoea == "t" |
         stomach.ache == "t" | other == "t") & ili > 0]) / nrow(ds[ili > 0]) # 0.8446602

nrow(ds[(Q9_1 == "t" | Q9_2 == "t") & ili > 0]) # 77
nrow(ds[(Q9_1 == "t" | Q9_2 == "t") & ili > 0]) / nrow(ds[ili > 0]) # 0.7475728
nrow(ds[visit.medical.service.gp == "t" & ili > 0]) # 9
nrow(ds[visit.medical.service.gp == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.08737864
nrow(ds[visit.medical.service.appointment == "t" & ili > 0]) # 2
nrow(ds[visit.medical.service.appointment == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.01941748
nrow(ds[visit.medical.service.ae == "t" & ili > 0]) # 1
nrow(ds[visit.medical.service.ae == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.009708738
nrow(ds[Q8_3 == "t" & ili > 0]) # 9
nrow(ds[Q8_3 == "t" & ili > 0]) / nrow(ds[ili > 0]) # 0.08737864

nrow(ds[gender==0]) / nrow(ds[gender==1]) # 0.6770186

nrow(ds[age >= 18 & age <= 24]) / nrow(ds[age >= 18]) # 0.03885004
nrow(ds[age >= 25 & age <= 44]) / nrow(ds[age >= 18]) # 0.3815074
nrow(ds[age >= 45 & age <= 64]) / nrow(ds[age >= 18]) # 0.4351204
nrow(ds[age >= 65]) / nrow(ds[age >= 18]) # 0.1445221

nrow(ds[education.bsc == "t" | education.msc == "t"]) /
  nrow(ds[education.bsc=="t" | education.msc == "t" |
          education.alevels == "t" |  education.gcse == "t" |
          no.education == "t" | education.stillin == "t"]) # 0.7557707
nrow(ds[education.alevels == "t"]) /
  nrow(ds[education.bsc=="t" | education.msc == "t" |
          education.alevels == "t" |  education.gcse == "t" |
          no.education == "t" | education.stillin == "t"]) # 0.1355175
nrow(ds[education.gcse == "t"]) /
  nrow(ds[education.bsc=="t" | education.msc == "t" |
          education.alevels == "t" |  education.gcse == "t" |
          no.education == "t" | education.stillin == "t"]) # 0.07371556

nrow(ds[no.education == "t"]) /
  nrow(ds[education.bsc=="t" | education.msc == "t" |
          education.alevels == "t" |  education.gcse == "t" |
          no.education == "t" | education.stillin == "t"]) # 0.02457185

nrow(ds[occupation == 0  | occupation == 2]) / nrow(ds[!is.na(occupation)]) # 0.7391874
nrow(ds[occupation == 1]) / nrow(ds[!is.na(occupation)]) # 0.1887287
nrow(ds[occupation == 5]) / nrow(ds[!is.na(occupation)]) # 0.0327654

nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. == 0)]) / nrow(ds) # 0.202963
nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. == 1)]) / nrow(ds) # 0.2192593
nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. == 2)]) / nrow(ds) # 0.2437037
nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. == 3)]) / nrow(ds) # 0.142963
nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. == 4)]) / nrow(ds) # 0.1325926
nrow(ds[(nb.household.0.4 + nb.household.5.18 + nb.household.19.64 +
         nb.household.65. > 4)]) / nrow(ds) # 0.05851852

nrow(ds[age < 65]) # 1164
nrow(ds[risk.heart =="t" & age < 65]) / nrow(ds[age < 65])
nrow(ds[risk.asthma =="t" & age < 65]) / nrow(ds[age < 65])
nrow(ds[risk.kidney =="t" & age < 65]) / nrow(ds[age < 65])
nrow(ds[risk.diabetes =="t" & age < 65]) / nrow(ds[age < 65])
nrow(ds[risk.immune =="t" & age < 65]) / nrow(ds[age < 65])
nrow(ds[(risk.heart == "t" | risk.asthma == "t" |
         risk.kidney == "t" | risk.diabetes == "t" | risk.immune == "t") &
        age < 65]) / nrow(ds[age < 65])

nrow(ds[age < 65 & atrisk > 0 & vaccine.this.year == 0]) /
  nrow(ds[age < 65 & atrisk > 0]) # 0.7542857
nrow(ds[age >= 65 & vaccine.this.year == 0]) /
  nrow(ds[age >=65]) # 0.7688172
nrow(ds[pregnant == 0 & vaccine.this.year == 0]) /
  nrow(ds[pregnant == 0]) # 0.7272727

m <- melt(ds, id.vars=c("agegroups", "vaccine.this.year", "smoke",
  "gender"), measure.vars=c("ili", "nonili"))
data <- cast(m, agegroups+gender+smoke+vaccine.this.year~variable, sum)
attach(data)
fluglm <- glm(cbind(ili, nonili) ~ agegroups+gender+smoke+vaccine.this.year, family=binomial)
