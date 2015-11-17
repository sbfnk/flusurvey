library(data.table)
library(ggplot2)
library(reshape)

symptoms <- c("fever","chills","blocked.runny.nose","sneezing","sore.throat","cough","shortness.breath","headache","muscle.and.or.joint.pain","chest.pain","tired","loss.appetite","phlegm","watery.eyes","nausea","vomiting","diarrhoea","stomach.ache","other")

ds <- dt[!duplicated(dt$bid)]

ds$ili <- FALSE
ds$nbili <- with(dt, aggregate(ili,
                                    ## list(global.id.number=global.id.number),
                                    list(bid=bid),
                                    sum))$x
ds$ili <- (ds$nbili > 0)

ds$vaccinated <- with(dt, aggregate(vaccine,
                                         ## list(global.id.number=global.id.number),
                                         list(bid=bid),
                                         sum))$x > 0
ds$nonili <- 1-ds$ili
ds$smoking <- ds$smoke %in% c(1,2,3)
ds$weight <- 0
for (i in 1:length(levels(factor(ds$country)))) {
  ds[country==levels(factor(ds$country))[i]]$weight <-
    1/nrow(ds[country==levels(factor(ds$country))[i]])
}

png("attack_rate.png")
ggplot(ds[ili==T], aes(x=country, fill=country, weight=weight))+
  geom_bar(color="black")+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("attack rate", limits=c(0,1.01))+
  opts(legend.position="none")
dev.off()

png("vaccination_coverage.png")
ggplot(ds[vaccinated==T], aes(x=country, fill=country, weight=weight))+
  geom_bar(color="black")+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))+
  opts(legend.position="none")
dev.off()

ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  for (j in levels(factor(ds$agegroup))) {
    ds[country==i & agegroup == j]$reweight <-
      1/nrow(ds[country==i & agegroup==j])
  }
}
png("vaccination_coverage_by_age.png")
ggplot(ds[vaccinated==T], aes(x=agegroup, fill=agegroup, weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+  
  facet_grid(.~country)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(),
       axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.title.x =
       theme_blank())+ 
  scale_fill_brewer(name="age group", palette="Set1")+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))
dev.off()

ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  for (j in levels(factor(ds$atrisk))) {
    ds[country==i & atrisk == j & agegroup %in% levels(agegroup)[1:3]]$reweight <-
      1/nrow(ds[country==i & atrisk == j & agegroup %in% levels(agegroup)[1:3]])
  }
}
png("vaccination_coverage_by_risk.png")
ggplot(ds[vaccinated==T & agegroup %in% levels(agegroup)[1:3]],
       aes(x=factor(atrisk), fill=factor(atrisk), weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+  
  facet_grid(.~country)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(),
       axis.ticks = theme_blank(), axis.text.x = theme_blank(), axis.title.x =
       theme_blank())+
  scale_fill_brewer(name="Risk group", palette="Set1", labels=c("no", "yes"))+
  scale_y_continuous("vaccination coverage", limits=c(0,1.01))
dev.off()

png("age_dist.png")
ggplot(ds, aes(x=country, fill=agegroup, weight=weight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(), panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(name="age group", palette="Set1")+
  scale_y_continuous("age distribution", limits=c(0,1.01))
dev.off()

vaccine_time <- data.frame()
for (country in levels(factor(ds$country))) {
  vaccine_country <- data.frame(week=as.character(levels(factor(compare$week))),
                                elderly=0, risk=0, all=0, country=country)
  for (i in 1:nrow(vaccine_country)) {
    vaccine_week <- compare[week <= vaccine_country[i,]$week]
    vaccine_country[i,]$all <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0]) /
      nrow(compare[week == vaccine_country[i,]$week & country == country])
    vaccine_country[i,]$elderly <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0 &
                   agegroup == levels(compare$agegroup)[4]]) / 
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   agegroup == levels(compare$agegroup)[4]])
    vaccine_country[i,]$risk <-
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   vaccine.this.year == 0 & atrisk == 1]) /
      nrow(compare[week == vaccine_country[i,]$week & country == country &
                   atrisk == 1])
  }
  vaccine_time <- rbind(vaccine_time, vaccine_country)
}

ds$education <- ""
ds[no.education=="t"]$education <- "None"
ds[education.gcse=="t"]$education <- "Intermediate"
ds[education.alevels=="t"]$education <- "High school"
ds[education.bsc=="t"]$education <- "Bachelor"
ds[education.msc=="t"]$education <- "Higher"
ds[education.stillin=="t"]$education <- "Student"
ds$education <- factor(ds$education,
                       levels=levels(factor(ds$education))[c(1,3,2,4,5,6,7)])
ds$reweight <- 0
for (i in levels(factor(ds$country))) {
  ds[country==i]$reweight <-
    1/nrow(ds[education!="" & country==i])
}

png("education_dist.png")
ggplot(ds[education!=""], aes(x=country, fill=education, weight=reweight))+
  geom_bar()+
  geom_bar(color="black", show_guide=F)+
  theme_bw(20)+
  opts(panel.grid.major=theme_blank(),
  panel.grid.minor=theme_blank(), title)+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("education distribution")
dev.off()

countries <- data.frame(country=levels(factor(ds$country)), aru = 0, arv = 0,
                        ar = 0, efficacy = 0)
for (i in 1:nrow(countries)) {
  countries[i,]$ar <-
    nrow(ds[country == countries[i,]$country & ili == T]) / 
    nrow(ds[country == countries[i,]$country])
  countries[i,]$aru <-
    nrow(ds[country == countries[i,]$country & ili == T & vaccinated == F]) /
    nrow(ds[country == countries[i,]$country & vaccinated == F])
  countries[i,]$arv <-
    nrow(ds[country == countries[i,]$country & ili == T & vaccinated == T]) /
    nrow(ds[country == countries[i,]$country & vaccinated == T])
  countries[i,]$efficacy <-
    (countries[i,]$aru - countries[i,]$arv) / countries[i,]$aru * 100
}


