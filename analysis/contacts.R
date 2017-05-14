library('flusurvey')
library('magrittr')
library('tidyr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')

## https://en.wikipedia.org/wiki/Climate_of_the_United_Kingdom
sunshine_hours <- c(54.2, 74.3, 107.6, 155.2, 190.6, 182.6, 193.5, 182.5, 137.2, 103.1, 64.5, 47.3)

dt <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2013)

dt_back_contacts <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2013, surveys=c("background", "contact"))

dt_contacts <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2013, surveys="contact")

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

  p <- ggplot(bg_means, aes(x=hh_group, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Household size")
  save_plot(paste0(type, "_hh_size.pdf"), p)
  save_plot(paste0(type, "_hh_size.png"), p)

  p <- ggplot(bg_means, aes(x=hh_group, y=mean, color=setting)) +
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

  p <- ggplot(bg_means, aes(x=hh_children_group, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Number of children in the household")
  save_plot(paste0(type, "_hh_size_children.pdf"), p)
  save_plot(paste0(type, "_hh_size_children.png"), p)

  p <- ggplot(bg_means, aes(x=hh_children_group, y=mean, color=setting)) +
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

  p <- ggplot(bg_means %>% filter(!is.na(country)),
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

  p <- ggplot(bg_means %>% filter(!is.na(region)),
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
