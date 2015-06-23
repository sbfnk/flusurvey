dt <- readRDS("flusurvey_201314.rds")
dt <- dt[, c(1, 8, 9, 36, 37, 39, seq(66, 72), 123, 124, 127, 129, 130, 132, 133, 135, seq(136, 187)), with = FALSE]
