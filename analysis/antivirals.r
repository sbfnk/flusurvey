library('flusurvey')
library('data.table')
library('lubridate')
library('cowplot')
library('binom')

raw <- read_data(list(background = "~/Research/FluSurvey/Data/200910/background_200910.csv",
                      symptom = "~/Research/FluSurvey/Data/200910/symptoms_200910.csv"), 2010, sep = ";")

dt <- merge_data(raw)

dt[, symptoms.month := floor_date(symptoms.start.date, "month")]
dt[, antivirals := as.integer((medication.tamiflu + medication.relenza > 0))]

dt.ili <- dt[ili == 1]

ili_month <- dt.ili[, list(antivirals = sum(antivirals > 0), .N), by = list(symptoms.month)]
ili_month <- ili_month[!is.na(symptoms.month) & symptoms.month > "2009-01-01"]

n_month <- data.table(binom.confint(n_month$antivirals, n_month$N, method = "wilson"))
n_month[, proportion.ili.antivirals := x / n]
n_month[, symptoms.month := ili_month$symptoms.month]

p <- ggplot(n_month, aes(x = symptoms.month, y = mean, ymin = lower, ymax = upper)) +
  geom_errorbar() +
  scale_y_continuous("antivir / ILI") +
  coord_cartesian(ylim = c(0, 0.2)) +
  scale_x_date("")

ggsave("antivi_2009.pdf", p)
