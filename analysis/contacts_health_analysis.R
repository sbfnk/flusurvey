library("flusurvey")
library("tidyr")
library("dplyr")
library("cowplot")
library("lubridate")
library("ggplot2")
library("data.table")
library("here")

dt_symptoms <- extract_data(
  "data/flusurvey_raw_2012_2020.rds",
  years = 2012:2013,
  surveys = "symptom"
)
dt_back_symptoms <- extract_data(
  "data/flusurvey_raw_2012_2020.rds",
  years = 2012:2013,
  surveys = c("background", "symptom")
)
dt_contacts <- extract_data(
  "data/flusurvey_raw_2012_2020.rds",
  years = 2012:2013,
  surveys = c("background", "contact")
)

bouts <- bouts_of_illness(dt_symptoms, symptomatic.only = FALSE)

bouts[, health.status := ifelse(is.na(bout), "healthy", "ill")]
bouts[health.status == "ill", date := symptoms.start.date]
bouts[, order := 1]

## check all contact reports for symptoms
bouts_end <- copy(bouts)
bouts_end <- bouts_end[!is.na(bout)]
bouts_end[is.na(symptoms.end.date), symptoms.end.date := date]
bouts_end[, date := symptoms.end.date]
bouts_end[, health.status := "healthy"]
bouts_end[, order := 2]

db <- rbindlist(list(bouts, bouts_end))
db[, health.status := factor(health.status, levels = c("ill", "healthy"))]

setkey(db, season, global_id, date, order)
setkey(dt_contacts, season, global_id, date)

joined <- db[dt_contacts, roll = TRUE]
## ill is if any ill status on the day
setkey(joined, season, global_id, date, health.status)
joined <- unique(joined, by = c("season", "global_id", "date"))
joined <- joined[!is.na(health.status)]
joined[, order := NULL]

dt_inc <- get_incidence(dt_back_symptoms, incidence.columns = "ili.symptoms")
setnames(dt_inc, "week", "date")

inc <- dt_inc[, list(date, season, incidence = ili.symptoms / N)]
setkey(inc, season, date)
setkey(joined, season, date)

dc <- inc[joined, roll = TRUE]
dc[, week := floor_date(date, "week")]
dc[, month := floor_date(date, "month")]

contacts <- dc |>
  mutate(
    week = floor_date(date, "week"),
    month = floor_date(date, "month")
  )

dir.create(here("res"), showWarnings = FALSE)
saveRDS(contacts, here("res", "contacts_health.rds"))

contacts <- readRDS(here("res", "contacts_health.rds"))

hvsw <- contacts |>
  group_by(season, week, health.status) |>
  summarise(
    mc = median(conversational, na.rm = TRUE),
    incidence = unique(incidence),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 15) |>
  select(-n) |>
  pivot_wider(names_from = health.status, values_from = mc) |>
  filter(!is.na(ill), !is.na(healthy))

hvsm <- dc |>
  group_by(season, month, health.status) |>
  summarise(
    mc = median(conversational, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  filter(n >= 15) |>
  select(-n) |>
  pivot_wider(names_from = health.status, values_from = mc) |>
  filter(!is.na(ill), !is.na(healthy))

dir.create(here("figures"), showWarnings = FALSE)

p <- ggplot(hvsw, aes(x = healthy, y = ill)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_cowplot()
save_plot(here("figures", "dc_vs_dc_mean.pdf"), p)

s <- stats::lm(formula = ill ~ healthy, data = hvsw)
print(confint(s))
print(summary(s))

hdcw <- hvsw |>
  pivot_longer(
    cols = c(ill, healthy),
    names_to = "health.status",
    values_to = "contacts"
  ) |>
  unite(status_season, health.status, season, remove = FALSE)

hdcm <- hvsm |>
  pivot_longer(
    cols = c(ill, healthy),
    names_to = "health.status",
    values_to = "contacts"
  ) |>
  unite(status_season, health.status, season, remove = FALSE)

p <- ggplot(
  hdcm,
  aes(x = month, group = status_season, y = contacts, colour = health.status)
) +
  geom_line()
save_plot(here("figures", "contacts_by_month_health.pdf"), p)

hvsl <- hvsw |>
  pivot_longer(
    cols = c(ill, healthy),
    names_to = "health.status",
    values_to = "mc"
  )

p <- ggplot(hvsl, aes(x = incidence, y = mc, color = health.status)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_colour_brewer("Health status", palette = "Set1") +
  xlab("Incidence of ILI symptoms") +
  ylab("Median number of contacts") +
  theme_cowplot() +
  theme(legend.position = "top")
save_plot(here("figures", "contacts_vs_incidence.pdf"), p)

s <- stats::lm(
  formula = mc ~ incidence,
  data = hvsl |> filter(health.status == "healthy")
)
print("Model for healthy:")
print(confint(s))
print(summary(s))

s <- stats::lm(
  formula = mc ~ incidence,
  data = hvsl |> filter(health.status == "ill")
)
print("Model for ill:")
print(confint(s))
print(summary(s))
