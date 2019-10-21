
library('dplyr')
library('magrittr')
library('lubridate')
library('brms')

contacts <- readRDS("res/contacts_health.rds")

dt_back_contacts <- contacts %>%
    mutate(season_month = month(date) - 6,
           month_name = as.character(month(date, label=TRUE, abbr=FALSE))) %>%
    mutate(season_month = if_else(season_month > 0, season_month, season_month + 12)) %>%
    mutate(weekday=(day.of.week %in% 1:5))

colnames(dt_back_contacts) <- gsub("\\.", "_", colnames(dt_back_contacts))

for (type in c("conversational", "physical"))
{
    participants <-
        data.frame(global_id=unique(dt_back_contacts$global_id)) %>%
        mutate(new_participant_id=1:n())

    contacts <- dt_back_contacts %>%
        left_join(participants, by="global_id") %>%
        select(-global_id, contacts=!!sym(type)) %>%
        rename(global_id=new_participant_id) %>%
        filter(!is.na(contacts))

    mean_contacts <- mean(contacts$contacts, na.rm=TRUE)

    random_poisson_model <-
        brm(formula=contacts ~ 1, data=contacts, family=poisson,
            cores=2, chains=2, iter=200)

    random_negbin_model <-
      brm(formula=contacts ~ 1, data=contacts, family=negbinomial,
          cores=2, chains=2, iter=200)

    variate_model <- brm(
        formula=contacts ~ 1 + agegroup + weekday + season_month + month_name + gender + nb_household + nb_household_children + main_activity + occupation + highest_education + incidence + urban_rural + work_urban_rural + enclosed_indoor_space + public_transport + health_status, data=contacts, family=negbinomial, cores=2, chains=2, iter=200, prior = set_prior(horseshoe())
    )

    individual_model <- brm(
        formula=contacts ~ (1 | global_id) + agegroup + weekday + season_month + month_name + gender + nb_household + nb_household_children + main_activity + occupation + highest_education + incidence + health_status + incidence:health_status + urban_rural + work_urban_rural + enclosed_indoor_space + public_transport, data=contacts, family=negbinomial, cores=2, chains=2, iter=4000, prior = set_prior(horseshoe())
    )

    saveRDS(list(random=random_model,
                 variate=variate_model,
                 individual_mu=individual_mu_model,
                 individual_sigma=individual_sigma_model,
                 individual=individual_model,
                 variate_individual_mu=variate_individual_mu_model),
            paste0(type, "_contact_models.rds"))
}
