ur <- read.csv("urban_rural.csv", header=F, sep=",")

contacts <- bt[ct, roll=TRUE]
setnames(contacts, "Q3", "postcode")
contacts$postcode <- toupper(contacts$postcode)

contacts_pc <- contacts[contacts$postcode!="",]
contacts_pc$ur <- rep(0, length(contacts_pc$postcode))
contacts_pc$country <- rep("X", length(contacts_pc$postcode))
contacts_pc$postcode <- sub("[[:blank:]]+$", "", contacts_pc$postcode)

#contacts_pc <- contacts_pc[-(171:173),]
todel <- c()

for (i in 1:length(contacts_pc$country)) {
  if (length(ur[ur==contacts_pc[i,]$postcode,2]) > 0 &&
      length(ur[ur==contacts_pc[i,]$postcode,3]) > 0) {
    contacts_pc[i,]$country <- as.character(ur[ur==contacts_pc[i,]$postcode,2])
    contacts_pc[i,]$ur <- ur[ur==contacts_pc[i,]$postcode,3]
  } else {
    todel <- c(todel, i)
  }
}

contacts_pc <- contacts_pc[-todel]

contacts_pc$urban <- rep(0, length(contacts_pc$postcode))

contacts_pc[contacts_pc$country %in% c("E","W") & contacts_pc$ur %in% c(1,5),]$urban <- 1
contacts_pc[contacts_pc$country %in% c("E","W") & !(contacts_pc$ur %in% c(1,5)),]$urban <- 0

contacts_pc[contacts_pc$country == "S" & contacts_pc$ur %in% c(1,2),]$urban <- 1
contacts_pc[contacts_pc$country == "S" & !(contacts_pc$ur %in% c(1,2)),]$urban <- 0

contacts_pc[contacts_pc$country == "N" & contacts_pc$ur %in% c(1,2,3,4),]$urban <- 1
contacts_pc[contacts_pc$country == "N" & !(contacts_pc$ur %in% c(1,2,3,4)),]$urban <- 0
