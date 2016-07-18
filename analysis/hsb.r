library('readr')
library('tidyr')
library('dplyr')
library('binom')
library('cowplot')
library('scales')

dt <- read_csv("~/Research/Flusurvey/hsb.csv")

colnames(dt)[1] <- "severity"

mdt <- dt %>% gather(season, proportion, 2:6)

mdt <- mdt %>%
  mutate(severity = ifelse(severity == "04-Jul", "4-7", severity),
         severity = ifelse(severity == "Aug-14", "8-14", severity),
         severity = ifelse(grepl("15$", severity), "15+", severity),
         severity = ifelse(grepl("50.1%", severity), "50+%", severity),
         n = as.integer(gsub("^.*\\((.*)\\/.*$", "\\1", proportion)),
         N = as.integer(gsub(",", "", gsub("^.*\\(.*/(.*)\\)$", "\\1", proportion)))) %>%
  select(-proportion)

severity_order <- c("ARI", "ILI-No fever", "ILI-Fever, no phlegm", "ILI-Fever & Phlegm")

mdt <- mdt %>%
  filter(severity != "ARI or ILI") %>%
  mutate(severity = ifelse(severity == "ILI-Fever", "ILI-Fever, no phlegm", severity),
         severity = factor(severity, c(severity_order, setdiff(unique(severity), severity_order))))

mdt <- mdt %>%
  mutate(type = ifelse(grepl("%", severity), "Health-score decrease", NA_character_),
         type = ifelse(is.na(type) & grepl("^[0-9]", severity), "Illness duration (in days)", type), 
         type = ifelse(is.na(type), "Symptom", type))

clean <- mdt %>% filter(!is.na(n) & !is.na(N))
clean_confint <- binom.confint(clean$n, clean$N, method = "wilson")
clean <- cbind(clean %>% select(-n, -N), clean_confint) %>% select(-method)

mdt <- mdt %>%
  select(-n, -N) %>%
  left_join(clean)

mdts <- lapply(unique(mdt$type), function(this.type)
{
  mdt %>%
    filter(type == this.type)
})

p <- list()
for (i in seq_along(mdts))
{
  p[[i]] <- ggplot(mdts[[i]],
                   aes(x = season, y = mean, ymin = lower,
                       ymax = upper, color = severity, group = severity)) +
    geom_errorbar(position = position_dodge(width = 0.3)) +
    geom_point(position = position_dodge(width = 0.3))+
    geom_line(position = position_dodge(width = 0.3)) +
    scale_color_brewer(unique(mdts[[i]]$type), palette = "Set1") +
    coord_cartesian(ylim = c(0, 0.4))+
    scale_y_continuous("Proportion", labels = percent) + 
    scale_x_discrete("") +
    theme(axis.text.x=element_text(angle = 45, vjust = 0.5),
          text = element_text(size = 12))
}

g <- plot_grid(p[[1]], p[[2]], p[[3]], ncol = 1, nrow = 3, labels = c(" A", " B", " C"), align = "v")
save_plot("hsb.pdf", g, base_width = 5, base_height = 7)
