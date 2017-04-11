library('flusurvey')
library('cowplot')

dt <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013)

dt_back_contacts <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013, surveys=c("background", "contact"))

dt_contacts <- extract_data("flusurvey_raw_2010_2015.rds", years=2012:2013, surveys="contact")

for (type in c("conversational", "physical"))
{
  ## describe contact data
  max_people <- dt_contacts %>%
    group_by(participant_id, season) %>%
    summarise(n=n()) %>%
    group_by(participant_id) %>%
    summarise(seasons=length(unique(season)), min=min(n)) %>%
    ungroup %>%
    dplyr::filter(seasons == 2) %>%
    arrange(min) %>%
    tail(n=6) %>%
    .$participant_id

  p <- ggplot(dt_contacts %>% dplyr::filter(participant_id %in% max_people),
              aes_string(x="date", y=type, group="season"))+
    geom_point()+
    geom_line()+
    scale_color_brewer(palette="Set1")+
    facet_wrap(~participant_id)+
    scale_x_date("Season",
                 breaks=date_breaks("1 year"), labels=date_format("%Y"))+
    scale_y_continuous(paste0("Number of ", type, " contacts"))+
    theme(legend.position="none")
  ggsave(paste0("example_users_", type, ".pdf"), p, width=6.5, height=3.5)

  means <- dt_contacts %>%
    group_by(participant_id) %>%
    mutate_(mean=lazyeval::interp(~mean(type), type=as.name(type)),
            var=lazyeval::interp(~var(type), type=as.name(type))) %>%
    ungroup

  var_pop <- means %>%
    summarise(var=var(mean),
              mean=mean(mean))

  p <- ggplot(means, aes(x=mean))+
    geom_histogram(binwidth=1) +
    coord_cartesian(xlim=c(0, 25)) +
    scale_y_continuous("Number of surveys") +
    scale_x_continuous("Mean number of contacts")
  save_plot(paste0(type, "_dist.pdf"), p)

  max_contacts <-
    min(100, max(means %>%
                 dplyr::filter(mean <= 100, !is.na(var)) %>%
                 .$mean))

  max_var <-
    max(means %>%
        dplyr::filter(mean <= max_contacts) %>%
        .$var, na.rm=TRUE)

  p <- ggplot(means %>% filter(mean>0, var>0), aes(x=log10(mean), y=log10(var))) +
    geom_jitter() +
    geom_smooth(method=lm) +
    ## coord_cartesian(xlim=c(1, max_contacts), ylim=c(1, max_var)) +
    geom_line(data=data.frame(mean=seq_len(max_contacts),
                              var=seq_len(max_contacts)), linetype="dashed") +
    geom_point(data=data.frame(var_pop), color="red") +
    scale_x_continuous("Mean (log-scale)") +
    scale_y_continuous("Variance (log-scale)")

  save_plot(paste0(type, "_contacts_mean_var.pdf"), p)

  ## contact with respect to background
  bg_means <- dt_back_contacts %>%
    group_by(participant_id) %>%
    mutate_(mean=lazyeval::interp(~mean(type), type=as.name(type)),
            var=lazyeval::interp(~var(type), type=as.name(type))) %>%
    ungroup

  ## age
  max_y <- bg_means %>% .$age %>% quantile(probs=0.95, na.rm=TRUE)
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(age)),
              aes(x=agegroup, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Age group")
  save_plot(paste0(type, "_age.pdf"), p)

  ## education

  ## rural/urban? postcode?
  ## household size?
  ## profession?
  ## others?





}
