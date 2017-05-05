library('flusurvey')
library('magrittr')
library('dplyr')
library('cowplot')
library('tidyr')

dt_back <- extract_data("flusurvey_raw_2010_2017.rds", years=2015, surveys=c("background"))
dt_back %<>%
  filter(!duplicated(participant_id))

dt_back %>%
  .$gender %>%
  table

p <- ggplot(dt_back, aes(x=gender)) + 
  geom_bar() +
  xlab("") +
  ylab("Number of participants")

save_plot("demo_gender.pdf", p)

dt_back %>%
  .$agegroup %>%
  table

p <- ggplot(dt_back, aes(x=agegroup)) + 
  geom_bar() +
  xlab("Age") +
  ylab("Number of participants")

save_plot("demo_age.pdf", p)

dt_back %>%
  .$region %>%
  table

p <- ggplot(dt_back, aes(x=region))+ 
  geom_bar() +
  xlab("Region") +
  ylab("Number of participants") +
  theme(axis.text.x=element_text(angle=45, hjust=1))

save_plot("demo_region.pdf", p)

dt_back %>%
  .$country %>%
  table

p <- ggplot(dt_back, aes(x=country))+ 
  geom_bar() +
  xlab("Country") +
  ylab("Number of participants")

save_plot("demo_country.pdf", p)
