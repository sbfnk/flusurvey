library('flusurvey')
library('magrittr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')

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
  max_y <- bg_means %>% .$mean %>% quantile(probs=0.99, na.rm=TRUE)

  ## age
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(age)),
              aes(x=agegroup, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Age group")
  save_plot(paste0(type, "_age.pdf"), p)

  ## education
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(highest.education)),
              aes(x=highest.education, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Highest education level",
                     labels=c("None", "GCSE", "A-levels", "BSc", "MSc"))
  save_plot(paste0(type, "_education.pdf"), p)

  ## students
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(education.stillin)),
              aes(x=education.stillin, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("In education", labels="No", "Yes")
  save_plot(paste0(type, "_students.pdf"), p)

  ## rural/urban? postcode?
  p <- ggplot(bg_means %>% dplyr::filter(!is.na(urban.rural)),
              aes(x=urban.rural, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Settlement type")
  save_plot(paste0(type, "_settlement.pdf"), p)

  p <- ggplot(bg_means %>% dplyr::filter(!is.na(work.urban.rural)),
              aes(x=work.urban.rural, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Work settlement type")
  save_plot(paste0(type, "_work_settlement.pdf"), p)

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

  ## regional differences
  p <- ggplot(bg_means %>% filter(!is.na(country)), aes(x=country, y=mean)) +
    geom_boxplot() +
    coord_cartesian(ylim=c(0, max_y)) +
    scale_y_continuous(paste0("Number of ", type, " contacts")) +
    scale_x_discrete("Country",
                     labels=stri_trans_totitle(gsub("_", " ",
                                                    levels(bg_means$country)))) +
  save_plot(paste0(type, "_country.pdf"), p)

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
}
