library('rethinking')
library('flusurvey')
library('dplyr')

dt_back_contacts <-
  extract_data("flusurvey_raw_2010_2017.rds",
               years=2012:2013, surveys=c("background", "contact"))

just_contacts <- dt_back_contacts %>%
  select(participant_id, contacts=conversational) %>%
  mutate(participant_id=as.integer(participant_id),
         contacts=as.integer(contacts))

nb_participants <- just_contacts %>%
    group_by(participant_id) %>%
    summarise %>%
    nrow

random_model <- map2stan(
  alist(
    contacts ~ dgampois(mu, k),
    log(mu) <- a,
    a ~ dnorm(2.5, 1),
    k ~ dexp(1)
  ), data=just_contacts %>% data.frame, constraints=list(b="lower=0"), start=list(a=2.5), iter=100
)

individual_mu_model <- map2stan(
    alist(
        contacts ~ dgampois(mu, k),
        log(mu) <- a[participant_id],
        a[participant_id] ~ dnorm(2.5, 1),
        k ~ dexp(1)
    ), data=just_contacts %>% data.frame, iter=100, constraints=list(b="lower=0"), start=list(a=rep(2.5, nb_participants), b=1)
)

individual_sigma_model <- map2stan(
    alist(
        contacts ~ dgampois(mu, k),
        log(mu) <- a,
        k <- b[participant_id],
        a ~ dnorm(2.5, 1),
        b[participant_id] ~ dexp(1)
    ), data=just_contacts %>% data.frame, iter=100, constraints=list(b="lower=0"), start=list(a=2.5, b=rep(1, nb_participants))
)

individual_model <- map2stan(
    alist(
        contacts ~ dgampois(mu, k),
        log(mu) <- a[participant_id],
        k <- b[participant_id],
        a[participant_id] ~ dnorm(2.5, 1),
        b[participant_id] ~ dexp(1)
    ), data=just_contacts %>% data.frame, iter=100, constraints=list(b="lower=0"), start=list(a=rep(2.5, nb_participants), b=rep(1, nb_participants))
)

contacts_data <- list(
  N = just_contacts %>%
    nrow,
  N_participant_id = just_contacts %>%
    group_by(participant_id) %>%
    summarise %>%
    nrow,
  contacts = just_contacts$contacts,
  participant_id = just_contacts$participant_id
)

fit <- stan(file=contacts_stan_file,
            data=contacts_data,
            chains=1,
            warmup=100,
            iter=200,
            refresh=10,
            init=list(list(a=2.5, inv_k=1)),
            verbose=TRUE)
