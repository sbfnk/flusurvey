dt <- readRDS("flusurvey_201314.rds")
write.table(dt, "flusurvey_201314.csv", quote = FALSE, sep = ",", row.names = FALSE)

dt <- dt[, c(1, 122, 8, 9, 36, 37, 39, seq(66, 72), 123, 124, 127, 129, 130, 132, 133, 135, seq(136, 187)), with = FALSE]

setnames(dt, "i.timestamp", "timestamp")

write.table(dt, "flusurvey_201314_reduced.csv", quote = FALSE, sep = ",", row.names = FALSE)
