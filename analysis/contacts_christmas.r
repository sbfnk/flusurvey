library('flusurvey')
library('magrittr')
library('tidyr')
library('dplyr')
library('cowplot')
library('stringi')
library('scales')
library('lubridate')
library('ggplot2')
library('janitor')

## https://en.wikipedia.org/wiki/Climate_of_the_United_Kingdom
sunshine_hours <- c(54.2, 74.3, 107.6, 155.2, 190.6, 182.6, 193.5, 182.5, 137.2, 103.1, 64.5, 47.3)

dt <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013)

interesting_weeks <- as.Date(c("2012-12-17", "2012-12-24", "2012-12-31", "2013-01-07", "2013-01-14"))

dt_back_contacts <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013, surveys=c("background", "contact")) %>%
  mutate(date_week = floor_date(date, "week", 1)) %>%
  filter(between(date_week, as.Date(interesting_weeks[1]), as.Date(interesting_weeks[5]))) %>%
  janitor::clean_names()

dt_contacts <- extract_data("data/flusurvey_raw_2010_2018.rds", years=2012:2013, surveys="contact") %>%
  mutate(date_week = floor_date(date, "week", 1)) %>%
  filter(between(date_week, as.Date(interesting_weeks[1]), as.Date(interesting_weeks[5])))

type <- "conversational"

for (week in interesting_weeks) {
  for (addon in c("", "_65")) {
    means_age <- dt_back_contacts %>%
      filter(!is.na(agegroup)) %>%
      select(participant_id, date_week, agegroup, starts_with(type)) %>%
      pivot_longer(starts_with(type)) %>%
      group_by(participant_id, date_week, agegroup, name) %>%
      summarise(value = mean(value), .groups = "drop")

    means_where <- means_age %>%
      filter(name %in% paste0(type, "_", c("home", "other", "work"), addon)) %>%
      mutate(location = sub(paste0(type, "_(.+)", addon), "\\1", name))

    p <- ggplot(means_where,
                aes(x = as.character(date_week), y = value, colour = location)) +
      geom_boxplot(outlier.shape = NA) +
      stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75)) +
      ylab("Number of contacts") +
      ylim(c(0, 10)) +
      xlab("Start of week") +
      facet_wrap(~ agegroup) +
      theme_minimal() +
      scale_colour_brewer("Location", palette = "Set1")

    ggsave(paste0(type, "_christmas_byage", addon, ".pdf"), p,
           width = 10, height = 5)

    bootstraps <- 100

    ratios <- means_where %>%
      filter(date_week %in% interesting_weeks[1:2]) %>%
      group_by(date_week, agegroup, location) %>%
      summarise(mean =
                  list(replicate(bootstraps,
                                 mean(sample(value, n(), replace = TRUE)))),
                .groups = "drop") %>%
      pivot_wider(names_from = "date_week", values_from = "mean") %>%
      rowwise() %>%
      mutate(ratio =
               if_else(addon == "",
                       list(unlist(`2012-12-24`) / unlist(`2012-12-17`)),
                       list(unlist(`2012-12-24`) - unlist(`2012-12-17`)))) %>%
      ungroup() %>%
      select(agegroup, location, ratio) %>%
      unnest(ratio) %>%
      group_by(agegroup, location) %>%
      summarise(ratio = quantile(ratio[!is.na(ratio)],
                                 c(0.05, 0.25, 0.5, 0.75, 0.95)),
                q = c(0.05, 0.25, 0.5, 0.75, 0.95),
                .groups = "drop") %>%
      pivot_wider(names_from = "q", values_from = "ratio")

    p <- ggplot(ratios, aes(x = agegroup, colour = location, y = `0.5`)) +
      geom_point(position = position_dodge(width = 0.5)) +
      geom_linerange(aes(ymin = `0.25`, ymax = `0.75`),
                     position = position_dodge(width = 0.5)) +
      geom_linerange(aes(ymin = `0.05`, ymax = `0.95`), alpha = 0.5,
                     position = position_dodge(width = 0.5)) +
      scale_colour_brewer("Location", palette = "Set1") +
      theme_minimal() +
      xlab("Age")

    if (addon == "") {
      p <- p +
        coord_flip(ylim = c(0, 2)) +
        ylab("Ratio of contacts in Christmas week to previous week" )
    } else {
      p <- p +
        coord_flip(ylim = c(-1, 1)) +
        ylab("Difference between contacts in Christmas week and previous week" ) +
        geom_hline(linetype = "dashed", yintercept = 0)
    }

    ggsave(paste0(type, "_christmas_ratios", addon, ".pdf"), p, width = 6,
           heigh = 4)
}
