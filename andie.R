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
dt10$background[, vaccine.this.year := as.numeric(had.seasonal.vaccine == 1)]
dt10$background[, vaccine.this.year.swineflu := as.numeric(had.swineflu.vaccine == 1)]
setnames(dt10$background, "date.y", "date")

dt11$background <- dt11$background[, vaccine.this.year.swineflu := NA_real_]
dt12$background <- dt12$background[, vaccine.this.year.swineflu := NA_real_]
dt13$background <- dt13$background[, vaccine.this.year.swineflu := NA_real_]

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

st <- st[, list(ili = as.numeric(any(ili.fever ==1))), by = list(global.id.number, season)]
bt <- bt[, list(vaccinated = as.numeric(any(vaccine.this.year == 0)), vaccinated.swineflu = as.numeric(any(vaccine.this.year.swineflu == 0))), by = list(global.id.number, season)]
ct <- ct[, list(physical = mean(physical),
                conversational = mean(conversational),
                reports = length(physical)),
         by = list(global.id.number, season)]

setkey(st, global.id.number, season)
setkey(bt, global.id.number, season)
setkey(ct, global.id.number, season)

dt <- merge(merge(st, bt), ct)
dt[is.na(ili), ili := 0]

write.table(dt, "contacts_vacc_ili.csv", quote = F, row.names = F, sep = ",")
