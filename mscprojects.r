library(data.table)
library(plyr)
dt <- list()

for (file in c("flusurvey_201415.rds", "flusurvey_201314.rds",  "flusurvey_201213.rds", "flusurvey_201112.rds"))
{
    dt[[file]] <- readRDS(file)
    dt[[file]] <- dt[[file]][!is.na(id)]

    dt[[file]][, global_id.bg := NULL]
    dt[[file]][, global_id := NULL]
    dt[[file]][, channel := NULL]
    dt[[file]][, timestamp := NULL]
    global.id.numbers <-
        data.table(global.id.number = unique(dt[[file]][, global.id.number]),
                   user_id = as.integer(0))
    global.id.numbers[, user_id := 1:nrow(global.id.numbers)]
    dt[[file]] <- merge(dt[[file]], global.id.numbers, by = "global.id.number",
                        all.x = TRUE)
    dt[[file]][, global.id.number := NULL]
    dt[[file]][, bid := NULL]
    dt[[file]][, cid := NULL]
    dt[[file]][, country := NULL]
    dt[[file]][, ur := NULL]
    dt[[file]][, uk.country := NULL]
    dt[[file]][, urban := NULL]
    dt[[file]][, work.ur := NULL]
    dt[[file]][, work.uk.country := NULL]
    dt[[file]][, work.urban := NULL]
    dt[[file]][, user := NULL]
    dt[[file]][, ili.notired := NULL]
    dt[[file]][, global_id.contacts := NULL]
    dt[[file]][, weekweight := NULL]
    dt[[file]][, ili.self := as.integer(ili.self)]
    
    inames <- c(grep("^i\\.", colnames(dt[[file]]), value = TRUE),
                grep("^conversational\\.", colnames(dt[[file]]), value = TRUE),
                grep("^physical\\.", colnames(dt[[file]]), value = TRUE),
                grep("^Q", colnames(dt[[file]]), value = TRUE),
                grep("^N", colnames(dt[[file]]), value = TRUE))
    if (length(inames) > 0) dt[[file]][, paste(inames) := NULL]
    
    for (name in colnames(dt[[file]]))
    {
        if (all(dt[[file]][, get(name)] %in% c("f", "t"), na.rm = TRUE))
        {
            dt[[file]][, c(name) := as.integer(as.character(revalue(get(name), c(f = 0, t = 1))))]
        }
    }
    setkey(dt[[file]], user_id, date)
    write.table(dt[[file]], sub("rds", "csv", file), quote = TRUE, sep = ",", row.names = FALSE)
}
