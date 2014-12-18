library(ggplot2)
library(scales)

whole.users <- whole.users[country == "uk", "country" := "UK", with=F]
whole.users <- whole.users[country == "fr", "country" := "FR", with=F]
whole.users <- whole.users[agegroup=="[0,18)","group" :=
                           "children",with=F] 
whole.users <- whole.users[agegroup %in% c("[18,45)",
                           "[45,65)") & children == 1,"group" :=
                           "adults.children",with=F] 
whole.users <- whole.users[agegroup %in% c("[18,45)",
                           "[45,65)") & children == 0,"group" :=
                           "adults.no.children",with=F] 
whole.users <- whole.users[agegroup == "[65,109]","group" :=
                           "elderly", with=F] 

whole.users <- whole.users[,"group.weight":=0,with=F]
whole.users$group <- factor(whole.users$group,
                            levels=c("children", "adults.children",
                            "adults.no.children", "elderly"))

for (i in levels(factor(whole.users$group))) {
  whole.users[country == "UK" & group==i, "group.weight.uk" :=
    1/nrow(whole.users[country == "UK" & group==i,]), with=F]
}

png("attack_rate_uk.png")
ggplot(whole.users[country == "UK" & !is.na(group) & ili==T],
       aes(x=group, fill=group, weight=group.weight.uk))+ 
  geom_bar(color="black")+
  theme_bw(20)+
  theme(
           axis.text.x=element_text(angle=45,hjust=1,vjust=1),
           axis.title.x=element_blank()
           )+
  ggtitle("UK")+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("Cumulative incidence", labels=percent)+
  scale_x_discrete(labels=c("Children", "Adults living with children",
                     "Adults not living with children", "Elderly"))
dev.off()

for (i in levels(factor(whole.users$group))) {
  whole.users[group==i, "group.weight" :=
              1/nrow(whole.users[group==i,]), with=F]
}

png("attack_rate_europe.png")
ggplot(whole.users[!is.na(group) & ili==T],
       aes(x=group, fill=group, weight=group.weight))+ 
  geom_bar(color="black")+
  theme_bw(20)+
  theme(
           axis.text.x=element_text(angle=45,hjust=1,vjust=1),
           axis.title.x=element_blank()
           )+
  ggtitle("Europe")+
  theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), legend.position="none")+
  scale_fill_brewer(palette="Set1")+
  scale_y_continuous("Cumulative incidence", labels=percent)+
  scale_x_discrete(labels=c("Children", "Adults living with children",
                     "Adults not living with children", "Elderly"))
dev.off()

for (i in levels(factor(whole.users$group))) {
  for (j in levels(factor(whole.users$country))) {
    whole.users[country==j & group == i, "group.country.weight" :=
                1/nrow(whole.users[country==j & group == i,]), with=F] 
  }
}

png("attack_rate_by_country.png", width=960)
ggplot(whole.users[!is.na(group) & ili==T],
       aes(x=group, fill=group, weight=group.country.weight))+ 
  geom_bar()+
  geom_bar(color="black", show_guide=F)+
  facet_grid(.~country)+
  theme_bw(20)+
  theme(
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        legend.title=element_blank()
        )+ 
  scale_fill_brewer(palette="Set1",
                    labels=c("Children", "Adults living with children",
                      "Adults not living with children", "Elderly"))+
  scale_y_continuous("Cumulative incidence", labels=percent)
dev.off()
