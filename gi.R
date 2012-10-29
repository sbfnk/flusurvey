library(data.table)

                                        # GI stuff
dt$gi.or <- as.numeric(dt$diarrhoea == "t" | dt$vomiting == "t" | dt$nausea == "t")
dt$gi.and <- as.numeric(dt$diarrhoea == "t" & dt$vomiting == "t" & dt$nausea == "t")
dt$gi.or.novom <- as.numeric(dt$diarrhoea == "t" | dt$nausea == "t")
dt$gi.and.novom <- as.numeric(dt$diarrhoea == "t" & dt$nausea == "t")

dt$newgi.or <- dt$gi.or
dt$newgi.and <- dt$gi.and
dt$newgi.or.novom <- dt$gi.or.novom
dt$newgi.and.novom <- dt$gi.and.novom

dt[same==0, newgi.or := 0]
dt[same==0, newgi.and := 0]
dt[same==0, newgi.or.novom := 0]
dt[same==0, newgi.and.novom := 0]

r.or <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or,
            row.vars=1)
r.and <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and,
            row.vars=1)
r.or.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.or.novom,
            row.vars=1)
r.and.novom <- ftable(dt[country == "uk"]$week, dt[country=="uk"]$newgi.and.novom,
            row.vars=1)

gi.or.raw.data <- data.frame(expand.grid(rev(attr(r.or, "row.vars"))),
                                   unclass(r.or))
gi.and.raw.data <- data.frame(expand.grid(rev(attr(r.and, "row.vars"))),
                                   unclass(r.and))
gi.or.novom.raw.data <- data.frame(expand.grid(rev(attr(r.or.novom, "row.vars"))),
                                   unclass(r.or.novom))
gi.and.novom.raw.data <- data.frame(expand.grid(rev(attr(r.and.novom, "row.vars"))),
                                   unclass(r.and.novom))

names(gi.or.raw.data) <- c("Week", "nongi", "gi")
names(gi.and.raw.data) <- c("Week", "nongi", "gi")
names(gi.or.novom.raw.data) <- c("Week", "nongi", "gi")
names(gi.and.novom.raw.data) <- c("Week", "nongi", "gi")

gi.or.raw.data$gi.incidence <-
  gi.or.raw.data$gi / (gi.or.raw.data$nongi + gi.or.raw.data$nongi)
gi.and.raw.data$gi.incidence <-
  gi.and.raw.data$gi / (gi.and.raw.data$nongi + gi.and.raw.data$nongi)
gi.or.novom.raw.data$gi.incidence <-
  gi.or.novom.raw.data$gi / (gi.or.novom.raw.data$nongi + gi.or.novom.raw.data$nongi)
gi.and.novom.raw.data$gi.incidence <-
  gi.and.novom.raw.data$gi / (gi.and.novom.raw.data$nongi + gi.and.novom.raw.data$nongi)

gi.or.12 <- gi.or.raw.data[-c(1:3, 22:26),]
gi.and.12 <- gi.and.raw.data[-c(1:3, 22:26),]
gi.or.novom.12 <- gi.or.novom.raw.data[-c(1:3, 22:26),]
gi.and.novom.12 <- gi.and.novom.raw.data[-c(1:3, 22:26),]

write.csv(gi.or.12, "gi_or_201112.csv", quote=F, row.names=F)
write.csv(gi.and.12, "gi_and_201112.csv", quote=F, row.names=F)
write.csv(gi.or.novom.12, "gi_or_novom_201112.csv", quote=F, row.names=F)
write.csv(gi.and.novom.12, "gi_and_novom_201112.csv", quote=F, row.names=F)
