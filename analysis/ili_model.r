library('flusurvey')
library('magrittr')
library('tidyr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')

categorical_to_single <- function(dt, var) {
    categories <- levels(dt[, get(var)])[-1]
    for (category in categories) {
        dt[, paste(var, category, sep=".") :=
                 as.integer(get(var) == category)]
    }
    return(dt)
}

dt <- extract_data("flusurvey_raw_2010_2017.rds", years=2012:2013)

bouts <- bouts_of_illness(dt) %>%
    filter(!is.na(ili)) %>%
    data.table

seasonal_nb <-
    table(bouts[, list(bouts=sum(ili == 1)),
                by=list(participant_id, season)][, list(season, bouts)])

ili <- copy(bouts) %>% tbl_df %>%
    categorical_to_single("main.activity") %>%
    categorical_to_single("occupation") %>%
    categorical_to_single("highest.education") %>%
    select(participant_id, contacts=get(type), age,
           urban.rural, work.urban.rural,
           nb.household, nb.household.children,
           gender, day.of.week, month,
           enclosed.indoor.space,
           public.transport, ili,
           conversational, physical=physical,
           starts_with("main.activity."),
           starts_with("occupation."),
           starts_with("highest.education.")
           ) %>%
    mutate(participant_id=as.integer(participant_id)) %>%
    dplyr::filter(!is.na(age)) %>%
    mutate(age=(age-mean(age))/sd(age), ## regularise
           gender=as.integer(gender) - 1,
           urban=as.integer(urban.rural) - 1,
           work.urban=as.integer(work.urban.rural) - 1,
           enclosed.indoor.space=as.integer(enclosed.indoor.space) - 1,
           public.transport=as.integer(public.transport) - 1,
           physical=(physical-mean(physical))/sd(physical),
           conversational=(conversational-mean(conversational))/sd(conversational))

complete_ili <- ili[complete.cases(ili), ]

complete_participants <-
    data.frame(participant_id=unique(complete_ili$participant_id)) %>%
    mutate(new_participant_id=1:n())

complete_ili %<>%
    left_join(complete_participants) %>%
    select(-participant_id) %>%
    rename(participant_id=new_participant_id)

nb_complete_participants <- complete_ili %>%
    group_by(participant_id) %>%
    summarise %>%
    nrow


random_model <- map2stan(
    alist(
        ili ~ dbinom(1, p),
        logit(p) <- a
        a ~ dnorm(0, 1)
    ), data=complete_ili %>% data.frame
)

random_contact_model <- map2stan(
    alist(
        ili ~ dbinom(1, p),
        logit(p) <- a +
            bc * conversational +
            bp * physical,
        a ~ dnorm(2.5, 1),
        bc ~ dnorm(0, 1),
        bp ~ dnorm(0, 1)
    ), data=complete_ili %>% data.frame, constraints=list(b="lower=0"),
    start=list(a=2.5),
)

variate_model <- map2stan(
    alist(
        ili ~ dbinom(1, p),
        logit(p) <- a +
            ba * age +
            bm * month +
            bg * gender +
            bh * nb.household +
            bhc * nb.household.children +
            bm1 * main.activity.paid_employment_part_time +
            bm2 * main.activity.self_employed +
            bm3 * main.activity.school +
            bm4 * main.activity.home_maker +
            bm5 * main.activity.unemployed +
            bm6 * main.activity.long_term_leave +
            bm7 * main.activity.retired +
            bm8 * main.activity.other +
            bo1 * occupation.office_worker +
            bo2 * occupation.retail +
            bo3 * occupation.skilled_manual +
            bo4 * occupation.other_manual +
            bo5 * occupation.other +
            bhe1 * highest.education.education.gcse +
            bhe2 * highest.education.education.alevels +
            bhe3 * highest.education.education.bsc +
            bhe4 * highest.education.education.msc +
            bhe5 * highest.education.education.stillin +
            bu * urban +
            bwu * work.urban +
            be * enclosed.indoor.space +
            bt * public.transport +
            bc * conversational +
            bp * physical,
        a ~ dnorm(2.5, 1),
        ba ~ dnorm(0, 1),
        bd ~ dnorm(0, 1),
        bg ~ dnorm(0, 1),
        bh ~ dnorm(0, 1),
        bhc ~ dnorm(0, 1),
        bm ~ dnorm(0, 1),
        bm1 ~ dnorm(0, 1),
        bm2 ~ dnorm(0, 1),
        bm3 ~ dnorm(0, 1),
        bm4 ~ dnorm(0, 1),
        bm5 ~ dnorm(0, 1),
        bm6 ~ dnorm(0, 1),
        bm7 ~ dnorm(0, 1),
        bm8 ~ dnorm(0, 1),
        bo1 ~ dnorm(0, 1),
        bo2 ~ dnorm(0, 1),
        bo3 ~ dnorm(0, 1),
        bo4 ~ dnorm(0, 1),
        bo5 ~ dnorm(0, 1),
        bhe1 ~ dnorm(0, 1),
        bhe2 ~ dnorm(0, 1),
        bhe3 ~ dnorm(0, 1),
        bhe4 ~ dnorm(0, 1),
        bhe5 ~ dnorm(0, 1),
        bu ~ dnorm(0, 1),
        bwu ~ dnorm(0, 1),
        be ~ dnorm(0, 1),
        bt ~ dnorm(0, 1),
        bc ~ dnorm(0, 1),
        bp ~ dnorm(0, 1),
        b ~ dexp(1)
    ), data=complete_ili %>% data.frame, constraints=list(b="lower=0"),
    start=list(a=2.5),
)

variate_individual_model <- map2stan(
    alist(
        ili ~ dbinom(1, p),
        logit(p) <- a[participant_id] +
            ba * age +
            bd * day.of.week +
            bm * month +
            bg * gender +
            bh * nb.household +
            bhc * nb.household.children +
            bm1 * main.activity.paid_employment_part_time +
            bm2 * main.activity.self_employed +
            bm3 * main.activity.school +
            bm4 * main.activity.home_maker +
            bm5 * main.activity.unemployed +
            bm6 * main.activity.long_term_leave +
            bm7 * main.activity.retired +
            bm8 * main.activity.other +
            bo1 * occupation.office_worker +
            bo2 * occupation.retail +
            bo3 * occupation.skilled_manual +
            bo4 * occupation.other_manual +
            bo5 * occupation.other +
            bhe1 * highest.education.education.gcse +
            bhe2 * highest.education.education.alevels +
            bhe3 * highest.education.education.bsc +
            bhe4 * highest.education.education.msc +
            bhe5 * highest.education.education.stillin +
            bu * urban +
            bwu * work.urban +
            be * enclosed.indoor.space +
            bt * public.transport +
            bc * conversational +
            bp * physical,
        a[participant_id] ~ dnorm(2.5, 1),
        ba ~ dnorm(0, 1),
        bd ~ dnorm(0, 1),
        bg ~ dnorm(0, 1),
        bh ~ dnorm(0, 1),
        bhc ~ dnorm(0, 1),
        bm ~ dnorm(0, 1),
        bm1 ~ dnorm(0, 1),
        bm2 ~ dnorm(0, 1),
        bm3 ~ dnorm(0, 1),
        bm4 ~ dnorm(0, 1),
        bm5 ~ dnorm(0, 1),
        bm6 ~ dnorm(0, 1),
        bm7 ~ dnorm(0, 1),
        bm8 ~ dnorm(0, 1),
        bo1 ~ dnorm(0, 1),
        bo2 ~ dnorm(0, 1),
        bo3 ~ dnorm(0, 1),
        bo4 ~ dnorm(0, 1),
        bo5 ~ dnorm(0, 1),
        bhe1 ~ dnorm(0, 1),
        bhe2 ~ dnorm(0, 1),
        bhe3 ~ dnorm(0, 1),
        bhe4 ~ dnorm(0, 1),
        bhe5 ~ dnorm(0, 1),
        bu ~ dnorm(0, 1),
        bwu ~ dnorm(0, 1),
        be ~ dnorm(0, 1),
        bt ~ dnorm(0, 1),
        bc ~ dnorm(0, 1),
        bp ~ dnorm(0, 1),
        b ~ dexp(1)
    ), data=complete_ili %>% data.frame, constraints=list(b="lower=0"),
    start=list(a=rep(2.5, nb_complete_participants)),
)

saveRDS(list(random=random_model,
             variate=variate_model,
             individual_mu=individual_mu_model,
             individual_sigma=individual_sigma_model,
             individual=individual_model,
             variate_individual_mu=variate_individual_mu_model),
        paste0(type, "_contact_models.rds"))
