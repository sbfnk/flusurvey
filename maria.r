library(data.table)
library(plyr)

dt <- readRDS("flusurvey_201314.rds")

dt[, global_id.bg := NULL]
dt[, global_id := NULL]
dt[, channel := NULL]
dt[, timestamp := NULL]
dt[, Qcountry := NULL]
dt[, global.id.number := NULL]
dt[, bid := NULL]
dt[, country := NULL]
dt[, ur := NULL]
dt[, uk.country := NULL]
dt[, urban := NULL]
dt[, work.ur := NULL]
dt[, work.uk.country := NULL]
dt[, work.urban := NULL]
dt[, i.id := NULL]
dt[, user := NULL]
dt[, i.global_id := NULL]
dt[, i.channel := NULL]
dt[, N1 := NULL]
dt[, ili.notired := NULL]
dt[, global_id.contacts := NULL]
dt[, i.timestamp := NULL]

for (name in colnames(dt))
{
    if (all(dt[, get(name)] %in% c("f", "t")))
    {
        dt[, c(name) := as.integer(as.character(revalue(get(name), c(f = 0, t = 1))))]        
    }
}


write.table(dt, "flusurvey_201314.csv", quote = TRUE, sep = ",", row.names = FALSE)


dt <- dt[, c(1, 122, 8, 9, 36, 37, 39, seq(66, 72), 123, 124, 127, 129, 130, 132, 133, 135, seq(136, 187)), with = FALSE]

setnames(dt, "i.timestamp", "timestamp")

write.table(dt, "flusurvey_201314_reduced.csv", quote = FALSE, sep = ",", row.names = FALSE)

dt[, ili.notired := NULL]

convert_01 <- function(x)
{
    
}
