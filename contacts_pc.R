ur <- read.csv("urban_rural.csv", header=F, sep=",")

dt$postcode <- sub("[[:blank:]]+$", "", dt$postcode)
dt$postcode <- toupper(dt$postcode)

dt$ur <- ur$V3[match(dt$postcode, ur$V1)]
dt$country <- ur$V2[match(dt$postcode, ur$V1)]
dt$urban <- rep(0, length(dt$postcode))

dt[is.na(dt$ur),]$urban <- 2

dt[dt$country %in% c("E","W") & !(dt$ur %in% c(2,3,4,6,7,8)),]$urban <- 0
dt[dt$country %in% c("E","W") & dt$ur %in% c(1,5),]$urban <- 1

dt[dt$country == "S" & dt$ur %in% c(1,2),]$urban <- 1
dt[dt$country == "S" & dt$ur %in% c(3,4,5,6,7),]$urban <- 0

dt[dt$country == "N" & dt$ur %in% c(1,2,3,4),]$urban <- 1
dt[dt$country == "N" & !(dt$ur %in% c(5,6,7)),]$urban <- 0

dt$urban <- as.factor(dt$urban)
