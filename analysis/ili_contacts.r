library('flusurvey')
library('magrittr')
library('tidyr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')

dt <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2013)

bouts <- bouts_of_illness(dt)
bouts <- bouts[!is.na(ili)]

bouts %>%
  gather(type, contacts, conversational, physical)

bouts %>%
  filter(!is.na(ili)) %>%
  group_by(ili, type) %>%
  summarise(contacts=mean(contacts))

ggplot(bouts %>% filter(!is.na(ili)),
       aes(x=factor(ili), color=type, fill=type, y=contacts))+
  geom_boxplot(alpha=0.5)+
  scale_x_discrete("", labels=c("no ILI", "ILI")) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_color_brewer("", palette="Set1") +
  scale_fill_brewer("", palette="Set1")


