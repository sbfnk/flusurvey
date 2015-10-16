library(data.table)
library(plyr)
dt <- list()

for (file in c("flusurvey_201315.rds", "flusurvey_201213.rds", "flusurvey_201112.rds"))
{
    dt[[file]] <- readRDS(file)
    dt[[file]] <- dt[[file]][!is.na(id)]

    dt[[file]][, global_id.bg := NULL]
    dt[[file]][, global_id := NULL]
    dt[[file]][, channel := NULL]
    dt[[file]][, timestamp := NULL]
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

    setkey(dt[[file]], id, date)
    write.table(dt[[file]], sub("rds", "csv", file), quote = TRUE, sep = ",", row.names = FALSE)
}
