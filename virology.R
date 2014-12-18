virology <- data.table(read.csv("virology.csv", header=T, sep=","))

gv <- c()
for (i in 40:77) {
  gv <- c(gv, which(abs(virology$x-i) == min(abs(virology$x-i)))[1])
}

virology <- virology[gv]
virology$x <- 40:77

virology[Influenza.A < 0]$Influenza.A <- 0
virology[Influenza.B < 0]$Influenza.B <- 0
virology[Inf.A.H1.pdm < 0]$Inf.A.H1.pdm <- 0
virology[Inf.A.H3. < 0]$Inf.A.H3. <- 0
virology$Influenza <- virology$Influenza.A + virology$Influenza.B

virology <- data.table(melt(virology, id.vars="x"))

setnames(virology, "x", "week")
setnames(virology, "variable", "strain")
setnames(virology, "value", "samples")

virology$year <- 0
virology[week>52]$year <- 2012
virology[week<=52]$year <- 2011
virology[week>52]$week <- virology[week>52]$week-52

virology$date <- as.Date(strptime(paste(virology$year, virology$week*7-1,sep=" "),format="%Y %j"))
write.csv(virology, file="virology_clean.csv", row.names=F)

