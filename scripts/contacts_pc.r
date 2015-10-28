library('data.table')

urban_rural <- data.table(read.csv("urban_rural.csv", header=F, sep=","))
setnames(urban_rural, 1:3, c("postcode", "country", "settlement.type"))
save(urban_rural, file = "urban_rural.rda")

regions <- data.table(read.csv("postcodes.csv", header = FALSE, sep=","))
setnames(regions, 1:2, c("postcode", "region"))
regions[region=="W99999999"]$region <- "Wales"
regions[region=="E12000001"]$region <- "North East England"
regions[region=="E12000002"]$region <- "North West England"
regions[region=="E12000003"]$region <- "Yorkshire and the Humber"
regions[region=="E12000004"]$region <- "East Midlands"
regions[region=="E12000005"]$region <- "West Midlands"
regions[region=="E12000006"]$region <- "East of England"
regions[region=="E12000007"]$region <- "London"
regions[region=="E12000008"]$region <- "South East England"
regions[region=="E12000009"]$region <- "South West England"
regions[region=="L99999999"]$region <- "Channel Islands"
regions[region=="N99999999"]$region <- "Northern Ireland"
regions[region=="S99999999"]$region <- "Scotland"

regions$postcode <- as.character(regions$postcode)
save(regions, file = "regions.rda")

