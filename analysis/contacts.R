library('flusurvey')
library('cowplot')

dt <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013)
dt_back_contacts <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013, surveys=c("background", "contact"))
dt_contacts <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013, surveys="contact")

## describe contact data
max_people <- dt_contacts %>%
  group_by(participant_id) %>%
  mutate(n=n()) %>%
  dplyr::filter(n >30)

p <- ggplot(max_people, aes(x=date, y=conversational, group=season,
                            color=factor(participant_id)))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette="Set1")+
  facet_wrap(~participant_id, scales="free_y")+
  scale_x_date(breaks=pretty_breaks())+
  theme(legend.position="none")

means <- dt_contacts %>%
  group_by(participant_id) %>%
  mutate(mean=mean(conversational),
         sd=sd(conversational)) %>%
  ungroup

sd_pop <- means %>%
  summarise(sd=sd(mean),
            mean=mean(mean))

p <- ggplot(means, aes(x=mean))+
  geom_histogram(binwidth=1)

p <- ggplot(means, aes(x=sd))+
  geom_histogram(binwidth=1)

p <- ggplot(means, aes(x=mean, y=sd)) +
  geom_jitter() +
  geom_smooth(method=lm) +
  coord_cartesian(xlim=c(0, 100), ylim=c(1, 100)) +
  geom_line(data=data.frame(mean=1:100, sd=1:100), linetype="dashed") +
  geom_point(data=data.frame(sd_pop), color="red") +
  scale_x_continuous("Mean") +
  scale_y_continuous("Standard deviation")
ggsave("contacts_mean_sd.pdf", p)

## contact with respect to background
