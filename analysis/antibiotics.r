library('flusurvey')
library('cowplot')
library('binom')
library('magrittr')

dt <- extract_data("flusurvey_raw_2010_2017.rds", surveys=c("background", "symptom"))

antibiotics <- dt %>%
  dplyr::filter(!is.na(medication.antibiotic)) %>%
  group_by(season, agegroup) %>%
  summarise(prescribed=sum(medication.antibiotic == "t"), n=n()) %>%
  ungroup %>%
  dplyr::filter(!is.na(agegroup)) %>%
  mutate(type="By season")

antibiotics_all <- antibiotics %>%
  group_by(agegroup) %>%
  summarise(prescribed=sum(prescribed),
            n=sum(n)) %>%
  ungroup %>%
  mutate(season="Overall",
         type="Overall")

antibiotics %<>%
  rbind(antibiotics_all)

anti_binom <-
  binom.confint(antibiotics$prescribed, antibiotics$n,
                method="wilson")

antibiotics %<>%
  left_join(anti_binom)

p <- ggplot(antibiotics %>%
            mutate(lower=ifelse(season == "Overall", lower, NA_real_),
                   upper=ifelse(season == "Overall", upper, NA_real_)),
            aes(x=agegroup, y=mean, ymin=lower, ymax=upper,
                             color=season, group=season)) +
  geom_point()+
  geom_errorbar()+
  geom_line()+
  expand_limits(y=0)+
  scale_y_continuous("Prescription rate", label=percent) +
  scale_x_discrete("Age group") +
  facet_wrap(~type) +
  scale_color_brewer(palette="Dark2")

ggsave("antibiotic_prescription_rate.pdf", p)

bouts <- bouts_of_illness(dt, symptomatic.only=TRUE)
saveRDS(bouts, "bouts_20180131.rds")
