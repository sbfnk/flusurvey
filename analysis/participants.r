library('cowplot')

dt <- extract_data(file.path("data", "flusurvey_raw_2010_2018.rds"), years=2010:2018, surveys=c("background", "symptom"))

part <- dt %>%
    group_by(season, participant_id) %>%
    summarise() %>%
    group_by(season) %>%
    summarise(n=n())

p <- ggplot(part, aes(x=season, y=n)) +
    geom_bar(stat="identity") +
    ylab("Number of participants") +
    xlab("") +
    theme(axis.text.x=element_text(angle = 45, vjust = 1, hjust=1))

save_plot(file.path("res", "participants.pdf"), p)
