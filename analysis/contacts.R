library('flusurvey')
library('magrittr')
library('tidyr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')
library('lubridate')

## https://en.wikipedia.org/wiki/Climate_of_the_United_Kingdom
sunshine_hours <- c(54.2, 74.3, 107.6, 155.2, 190.6, 182.6, 193.5, 182.5, 137.2, 103.1, 64.5, 47.3)

dt <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013)

dt_back_contacts <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013, surveys=c("background", "contact"))

dt_contacts <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013, surveys="contact")

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
  save_plot(paste0("example_users_", type, ".pdf"), p, base_aspect_ratio = 2)
  save_plot(paste0("example_users_", type, ".png"), p, base_aspect_ratio = 2)

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
  save_plot(paste0(type, "_dist.png"), p)

  max_contacts <-
    min(100, max(means %>%
                 dplyr::filter(mean <= 100, !is.na(var)) %>%
                 .$mean))

  max_meanvar <- max(log10(means$mean), log10(means$var), na.rm=TRUE)
  min_meanvar <- min(means %>% filter(mean > 0) %>% .$mean %>% log10,
                     means %>% filter(var > 0) %>% .$var %>% log10)

  p <- ggplot(means %>% filter(mean>0, var>0),
              aes(x=log10(mean), y=log10(var))) +
    geom_jitter() +
    geom_smooth(method=lm) +
    expand_limits(x=c(min_meanvar, max_meanvar * 1.1),
                  y=c(min_meanvar, max_meanvar * 1.1)) +
    ## coord_cartesian(xlim=c(1, max_contacts), ylim=c(1, max_var)) +
    geom_line(data=data.frame(mean=10**c(min_meanvar, max_meanvar),
                              var=10**c(min_meanvar, max_meanvar)),
              linetype="dashed") +
    geom_point(data=data.frame(var_pop), color="red") +
    scale_x_continuous("Mean (log-scale)") +
    scale_y_continuous("Variance (log-scale)") +
    ggtitle(paste0("Number of ", type, " contacts"))
  save_plot(paste0(type, "_contacts_mean_var.pdf"), p)
  save_plot(paste0(type, "_contacts_mean_var.png"), p)

  ## contact with respect to background
  bg_means <- dt_back_contacts %>%
    group_by(participant_id) %>%
    mutate_(mean=lazyeval::interp(~mean(type), type=as.name(type)),
            var=lazyeval::interp(~var(type), type=as.name(type))) %>%
    ungroup
  max_y <- bg_means %>% .$mean %>% quantile(probs=0.95, na.rm=TRUE)

  type_settings <- colnames(dt_back_contacts) %>%
    grep(paste0("^", type, "."), ., value=TRUE) %>%
    grep("^[^0-9]*$", ., value=TRUE)

  ## setting
  dots <- paste0("~mean(", type_settings, ")") %>%
    lapply(as.formula)
  bg_type_means <- dt_back_contacts %>%
    group_by(participant_id) %>%
    mutate_(.dots=setNames(dots, paste0("mean.", type_settings))) %>%
    ungroup %>%
    gather(setting, mean, starts_with("mean.")) %>%
    mutate(setting=sub(".*\\.", "", setting)) %>%
    mutate(setting=ifelse(setting == "work", "work/school", setting))
  max_type_y <- bg_type_means %>% .$mean %>% quantile(probs=0.95, na.rm=TRUE)

  ## age
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(age)),
              aes(x=agegroup, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Age group")
  save_plot(paste0(type, "_age.pdf"), p)
  save_plot(paste0(type, "_age.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(age)),
              aes(x=agegroup, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Age group") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_age_setting.pdf"), p)
  save_plot(paste0(type, "_age_setting.png"), p)

  ## education
  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(highest.education)),
              aes(x=highest.education, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Highest education level",
                     labels=c("None", "GCSE", "A-levels", "BSc", "MSc", "Student"))
  save_plot(paste0(type, "_education.pdf"), p)
  save_plot(paste0(type, "_education.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(highest.education)),
              aes(x=highest.education, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Highest education level",
                     labels=c("None", "GCSE", "A-levels", "BSc", "MSc", "Student")) +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_education_setting.pdf"), p)
  save_plot(paste0(type, "_education_setting.png"), p)

  ## students
  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(education.stillin)),
              aes(x=education.stillin, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("In education", labels=c("No", "Yes"))
  save_plot(paste0(type, "_students.pdf"), p)
  save_plot(paste0(type, "_students.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(education.stillin)),
              aes(x=education.stillin, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("In education", labels=c("No", "Yes")) +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_students_setting.pdf"), p)
  save_plot(paste0(type, "_students_setting.png"), p)

  ## rural/urban? postcode?
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(urban.rural)),
              aes(x=urban.rural, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Settlement type")
  save_plot(paste0(type, "_settlement.pdf"), p)
  save_plot(paste0(type, "_settlement.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(urban.rural)),
              aes(x=urban.rural, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Settlement type") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_settlement_setting.pdf"), p)
  save_plot(paste0(type, "_settlement_setting.png"), p)

  p <- ggplot(bg_means %>% dplyr::filter(!is.na(work.urban.rural)),
              aes(x=work.urban.rural, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Work settlement type")
  save_plot(paste0(type, "_work_settlement.pdf"), p)
  save_plot(paste0(type, "_work_settlement.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(work.urban.rural)),
              aes(x=work.urban.rural, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Work settlement type") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_work_settlement_setting.pdf"), p)
  save_plot(paste0(type, "_work_settlement_setting.png"), p)

  ## main activity
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(main.activity)),
              aes(x=main.activity, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Main activity",
                     labels=c("Full time employed",
                              "Part-time employed",
                              "Self-employed",
                              "School",
                              "Home-maker",
                              "Unemployed",
                              "Long-term leave",
                              "Retired",
                              "Other")) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
  save_plot(paste0(type, "_main_activity.pdf"), p)
  save_plot(paste0(type, "_main_activity.png"), p)

  p <- ggplot(bg_type_means %>% dplyr::filter(!is.na(main.activity)),
              aes(x=main.activity, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Main activity",
                     labels=c("Full time employed",
                              "Part-time employed",
                              "Self-employed",
                              "School",
                              "Home-maker",
                              "Unemployed",
                              "Long-term leave",
                              "Retired",
                              "Other")) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1)) +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_main_activity_setting.pdf"), p)
  save_plot(paste0(type, "_main_activity_setting.png"), p)

  ## occupation
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(occupation)),
              aes(x=occupation, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Occupation",
                     labels=c("Professional", "Office worker",
                              "Retail", "Skilled manual",
                              "Other manual", "Other")) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
  save_plot(paste0(type, "_occupation.pdf"), p)
  save_plot(paste0(type, "_occupation.png"), p)

  ## household size?
  bg_means %<>%
    mutate(hh_group =
             factor(ifelse(nb.household<6, as.character(nb.household), "6+"),
                    levels=c(as.character(seq_len(6)-1), "6+")))
  ## household size?
  bg_type_means %<>%
    mutate(hh_group =
             factor(ifelse(nb.household<6, as.character(nb.household), "6+"),
                    levels=c(as.character(seq_len(6)-1), "6+")))


  p <- ggplot(bg_means, aes(x=hh_group, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Household size")
  save_plot(paste0(type, "_hh_size.pdf"), p)
  save_plot(paste0(type, "_hh_size.png"), p)

  p <- ggplot(bg_type_means, aes(x=hh_group, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Household size") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_hh_size_setting.pdf"), p)
  save_plot(paste0(type, "_hh_size_setting.png"), p)

  bg_means %<>%
    mutate(hh_children_group =
             factor(ifelse(nb.household.children<6,
                           as.character(nb.household.children), "6+"),
                    levels=c(as.character(seq_len(6)-1), "6+")))
  bg_type_means %<>%
    mutate(hh_children_group =
             factor(ifelse(nb.household.children<6,
                           as.character(nb.household.children), "6+"),
                    levels=c(as.character(seq_len(6)-1), "6+")))

  p <- ggplot(bg_means, aes(x=hh_children_group, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Number of children in the household")
  save_plot(paste0(type, "_hh_size_children.pdf"), p)
  save_plot(paste0(type, "_hh_size_children.png"), p)

  p <- ggplot(bg_type_means,
              aes(x=hh_children_group, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Number of children in the household") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_hh_size_children_setting.pdf"), p)
  save_plot(paste0(type, "_hh_size_children_setting.png"), p)

  ## regional differences
  p <- ggplot(bg_means %>% filter(!is.na(country)), aes(x=country, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Country",
                     labels=stri_trans_totitle(gsub("_", " ",
                                                    levels(bg_means$country)))) +
  save_plot(paste0(type, "_country.pdf"), p)
  save_plot(paste0(type, "_country.png"), p)

  p <- ggplot(bg_type_means %>% filter(!is.na(country)),
              aes(x=country, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Country",
                     labels=stri_trans_totitle(gsub("_", " ",
                                                    levels(bg_means$country)))) +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position = "top")
  save_plot(paste0(type, "_country_setting.pdf"), p)
  save_plot(paste0(type, "_country_setting.png"), p)

  p <- ggplot(bg_means %>% filter(!is.na(region)),
              aes(x=region, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Region",
                     labels=stri_trans_totitle(gsub("_", " ",
                                                    levels(bg_means$region)))) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1))
  save_plot(paste0(type, "_region.pdf"), p)
  save_plot(paste0(type, "_region.png"), p)

  p <- ggplot(bg_type_means %>% filter(!is.na(region)),
              aes(x=region, y=mean, color=setting)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Region",
                     labels=stri_trans_totitle(gsub("_", " ",
                                                    levels(bg_means$region)))) +
    theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1),
          legend.position="top") +
    scale_color_brewer("", palette="Set1") +
  save_plot(paste0(type, "_region_setting.pdf"), p)
  save_plot(paste0(type, "_region_setting.png"), p)
}

means <- dt_contacts %>%
    group_by(participant_id) %>%
    mutate(mean.physical=mean(physical),
           mean.conversational=mean(conversational)) %>%
    ungroup %>%
    gather(type, contacts, starts_with("mean.")) %>%
    mutate(type=sub("mean.", "", type))

p <- ggplot(means, aes(x=contacts, fill=type))+
    geom_histogram(binwidth=1, position="stack") +
    scale_fill_brewer("", palette="Set1") +
    coord_cartesian(xlim=c(0, 25)) +
    scale_y_continuous("Number of surveys") +
    scale_x_continuous("Mean number of contacts") +
    theme(legend.position="bottom")
save_plot(paste0("contact_dist.pdf"), p)
save_plot(paste0("contact_dist.png"), p)

weekly_means <- dt_contacts %>%
    group_by(participant_id) %>%
    mutate(mean.physical=mean(physical),
           mean.conversational=mean(conversational)) %>%
    ungroup %>%
    gather(type, contacts, starts_with("mean.")) %>%
    mutate(type=sub("mean.", "", type))

dt_symptom_contacts <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013, surveys=c("background", "symptom", "contact"))
# of contacts
weekly_contacts <- dt_symptom_contacts %>%
    mutate(health_status=recode_factor(no.symptoms, t="healthy", f="symptomatic"),
           health_score=cut(health.score, breaks=seq(0, 100, by=25)),
           week=floor_date(date, "week")) %>% 
    filter(contact.id != "2013.1801") %>% ## remove spurious entry
    group_by(background.id) %>%
    mutate(n=n()) %>%
    ungroup %>%
    filter(n>1)

nb_sick <- weekly_contacts %>%
    group_by(season, week) %>%
    summarise(incidence=mean(health_status=="symptomatic")) %>%
    ungroup()

individual_contacts_by_health_status <- weekly_contacts %>%
    group_by(season, week, participant_id, health_status) %>%
    summarise(conversational=mean(conversational)) %>%
    group_by(season, participant_id, health_status) %>%
    summarise(conversational=mean(conversational)) %>%
    ungroup %>%
    spread(health_status, conversational) %>%
    filter(!(is.na(healthy) | is.na(symptomatic)))

contacts_by_health_status <- weekly_contacts %>%
    group_by(season, week, health_status) %>%
    summarise(mean=mean(conversational),
              sd=sd(conversational),
              n=n()) %>%
    ungroup

contacts_by_health_score <- weekly_contacts %>%
    filter(!is.na(health_score)) %>% 
    group_by(season, week, health_score) %>%
    summarise(mean=mean(conversational),
              sd=sd(conversational),
              n=n()) %>%
    ungroup

p <- ggplot(contacts_by_health_score, aes(x=week, y=mean, colour=health_score, group=interaction(season, health_score))) +
    facet_wrap(~season, scales="free_x") +
    geom_line() +
    scale_color_brewer("Health score (100 best)", palette="Set1") +
    xlab("Week") + ylab("Mean conversational contacts") +
    theme(legend.position="bottom")
ggsave("contacts_by_health_score.pdf", p)

write_csv(contacts_by_health_status, "contacts_by_health_status.csv")
write_csv(contacts_by_health_score, "contacts_by_health_score.csv")

healthy_medians <- weekly_contacts %>%
    filter(health_status=="healthy") %>%
    group_by(participant_id, season) %>%
    summarise(healthy_median=median(conversational)) %>%
    ungroup() %>%
    filter(healthy_median>0) %>%
    select(participant_id, season, healthy_median)

weekly_relative_contacts <- weekly_contacts %>%
    inner_join(healthy_medians, by=c("participant_id", "season")) %>%
    mutate(dc=(conversational-healthy_median)/healthy_median)

hvs <- weekly_relative_contacts %>%
    group_by(week, region, health_status) %>%
    summarise(dc=median(dc)) %>%
    ungroup() %>%
    spread(health_status, dc)

p <- ggplot(hvs, aes(x=healthy, y=symptomatic)) +
    geom_jitter() +
    geom_smooth(method="lm") +
    ylim(c(-1, 1)) +
    xlim(c(-1, 1)) +
    geom_hline(yintercept=0, linetype="dashed") +
    geom_vline(xintercept=0, linetype="dashed")
ggsave("dc_vs_dc_median_regional.pdf", p)

contacts_vs_sick <- weekly_relative_contacts %>%
    inner_join(healthy_medians, by=c("participant_id", "season")) %>%
    group_by(season, week, health_status) %>%
    summarise(median_dc=median(dc),
              median=median(conversational),
              n=n()) %>%
    ungroup %>%
    left_join(nb_sick)

p <- ggplot(contacts_vs_sick %>% filter(incidence>0.1),
            aes(x=incidence, y=median, colour=health_status)) +
    geom_point() +
    xlab("Incidence of sickness") +
    ylab("Median number of contacts") +
    geom_smooth(method="lm") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position="top")
ggsave("dc_vs_sickness.pdf", p)

p <- ggplot(contacts_vs_sick %>% filter(incidence>0.1),
            aes(x=incidence, y=median_dc, colour=health_status)) +
    geom_point() +
    xlab("Incidence of sickness") +
    ylab("Relative change in contacts") +
    geom_smooth(method="lm") +
    scale_color_brewer("", palette="Set1") +
    theme(legend.position="top")
ggsave("contacts_vs_sickness.pdf", p)

