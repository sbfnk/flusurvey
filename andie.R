library('data.table')

dt10 <- readRDS("flusurvey_200910_raw.rds")
dt11 <- readRDS("flusurvey_201011_raw.rds")
dt12 <- readRDS("flusurvey_201112_raw.rds")
dt13 <- readRDS("flusurvey_201213_raw.rds")

## merge what we can merge
join.vertical <- function (...) {
    x <- list(...)
    for (i in 1:(length(x) - 1)) {
        for (j in 2:(i + 1)) {
            names.diff <- setdiff(names(x[[j - 1]]), names(x[[i + 1]]))
            if (length(names.diff) > 0) {
                x[[j - 1]] <- x[[j - 1]][, !names.diff, with = F]
            }
        }
        names.diff.reverse <- setdiff(names(x[[i + 1]]), names(x[[i]]))
        if (length(names.diff.reverse) > 0) {
            x[[i + 1]] <- x[[i + 1]][, !names.diff.reverse, with = F]
        }
    }
    res <- do.call("rbind", c(x, list(use.names = T)))
}

setkey(dt10$background, global.id.number)
setkey(dt10$vaccination, global.id.number)
dt10$background <- merge(dt10$background, dt10$vaccination)
## in the other seasons,  vaccine.this.year == 0 means vaccinated,  here its' vaccine.this.year == 1
dt10$background[, vaccine.this.year := as.integer(as.character(vaccine.this.year)) - 1]
dt10$background[, vaccine.this.year.swineflu := as.integer(as.character(swineflu.vaccine.this.year)) - 1]
setnames(dt10$background, "date.y", "date")

dt11$background <- dt11$background[, vaccine.this.year.swineflu := NA_real_]
dt12$background <- dt12$background[, vaccine.this.year.swineflu := NA_real_]
dt13$background <- dt13$background[, vaccine.this.year.swineflu := NA_real_]

dt11[["contact"]][, "physical.19-64" := get("physical.19-44") + get("physical.45-64")]
dt12[["contact"]][, "physical.19-64" := get("physical.19-44") + get("physical.45-64")]
dt13[["contact"]][, "physical.19-64" := get("physical.19-44") + get("physical.45-64")]

dt11[["contact"]][, "conversational.19-64" := get("conversational.19-44") + get("conversational.45-64")]
dt12[["contact"]][, "conversational.19-64" := get("conversational.19-44") + get("conversational.45-64")]
dt13[["contact"]][, "conversational.19-64" := get("conversational.19-44") + get("conversational.45-64")]

st <- join.vertical(dt13$symptoms, dt12$symptoms, dt11$symptoms, dt10$symptoms)
bt <- join.vertical(dt13$background, dt12$background, dt11$background, dt10$background)
ct <- join.vertical(dt13$contact, dt12$contact, dt11$contact, dt10$contact)

# second half of season
st <- st[, month := as.numeric(format(date, "%m"))]
st <- st[, shos := ifelse(month < 7, -1, 0)]
st <- st[, season := as.numeric(format(date, "%Y")) + shos]
st <- st[season %in% as.numeric(names(which(table(st$season) > 100)))]
bt <- bt[, month := as.numeric(format(date, "%m"))]
bt <- bt[, shos := ifelse(month < 7, -1, 0)]
bt <- bt[, season := as.numeric(format(date, "%Y")) + shos]
bt <- bt[season %in% as.numeric(names(which(table(bt$season) > 100)))]
ct <- ct[, month := as.numeric(format(date, "%m"))]
ct <- ct[, shos := ifelse(month < 7, -1, 0)]
ct <- ct[, season := as.numeric(format(date, "%Y")) + shos]
ct <- ct[season %in% as.numeric(names(which(table(ct$season) > 100)))]

bt[, agegroup := cut(age, c(0, 5, 19, 65, 100), right = FALSE, labels = c("0-4", "5-18", "19-64", "65+"))]
bt <- bt[!is.na(agegroup)]

st <- st[, list(ili = as.numeric(any(ili.fever ==1, na.rm = TRUE))), by = list(global.id.number, season)]
bt <- bt[, list(vaccinated = as.numeric(any(vaccine.this.year == 0)), vaccinated.swineflu = as.numeric(any(vaccine.this.year.swineflu == 0))), by = list(global.id.number, agegroup, season)]
## ct <- ct[, list(physical = mean(physical),
##                 conversational = mean(conversational),
##                 reports = length(physical)),
##          by = list(global.id.number, season)]

ct <- ct[, list("physical.0-4" = mean(get("physical.0-4")),
                "physical.5-18" = mean(get("physical.5-18")),
                "physical.19-64" = mean(get("physical.19-64")),
                "physical.65+" = mean(get("physical.65+")),
                "conversational.0-4" = mean(get("conversational.0-4")),
                "conversational.5-18" = mean(get("conversational.5-18")),
                "conversational.19-64" = mean(get("conversational.19-64")),
                "conversational.65+" = mean(get("conversational.65+"))), 
         by = list(global.id.number, season)]

setkey(st, global.id.number, season)
setkey(bt, global.id.number, season)
setkey(ct, global.id.number, season)

dt <- merge(merge(st, bt), ct)

sample.ids <- data.table(global.id.number = unique(dt[, global.id.number]))
sample.ids[, id := seq_len(nrow(sample.ids))]

dt <- merge(dt, sample.ids, by = "global.id.number")

dt <- dt[, c("id", setdiff(colnames(dt), c("global.id.number", "id"))), with = FALSE]

write.table(dt, "contacts_vacc_ili.csv", quote = F, row.names = F, sep = ",")
