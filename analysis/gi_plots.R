library(data.table)
library(ggplot2)

gi.or.12 <- read.csv("gi_or_201112.csv", sep=",", header=T)
gi.and.12 <- read.csv("gi_and_201112.csv", sep=",", header=T)
gi.or.novom.12 <- read.csv("gi_or_novom_201112.csv", sep=",", header=T)
gi.and.novom.12 <- read.csv("gi_and_novom_201112.csv", sep=",", header=T)

tmp <- strsplit(as.character(gi.or.12$Week), "-")
cols <- t(sapply(tmp,c))
gi.or.12[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.and.12$Week), "-")
cols <- t(sapply(tmp,c))
gi.and.12[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.or.novom.12$Week), "-")
cols <- t(sapply(tmp,c))
gi.or.novom.12[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.and.novom.12$Week), "-")
cols <- t(sapply(tmp,c))
gi.and.novom.12[,c("year","week")] <- as.numeric(cols)

gi.or.12$date <- as.Date(strptime(paste(gi.or.12$year, gi.or.12$week*7-1,sep=" "),format="%Y %j"))
gi.and.12$date <- as.Date(strptime(paste(gi.and.12$year, gi.and.12$week*7-1,sep=" "),format="%Y %j"))
gi.or.novom.12$date <- as.Date(strptime(paste(gi.or.novom.12$year, gi.or.novom.12$week*7-1,sep=" "),format="%Y %j"))
gi.and.novom.12$date <- as.Date(strptime(paste(gi.and.novom.12$year, gi.and.novom.12$week*7-1,sep=" "),format="%Y %j"))

gi.or.11 <- read.csv("gi_or_201011.csv", sep=",", header=T)
gi.and.11 <- read.csv("gi_and_201011.csv", sep=",", header=T)
gi.or.novom.11 <- read.csv("gi_or_novom_201011.csv", sep=",", header=T)
gi.and.novom.11 <- read.csv("gi_and_novom_201011.csv", sep=",", header=T)

tmp <- strsplit(as.character(gi.or.11$Week), "-")
cols <- t(sapply(tmp,c))
gi.or.11[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.and.11$Week), "-")
cols <- t(sapply(tmp,c))
gi.and.11[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.or.novom.11$Week), "-")
cols <- t(sapply(tmp,c))
gi.or.novom.11[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.and.novom.11$Week), "-")
cols <- t(sapply(tmp,c))
gi.and.novom.11[,c("year","week")] <- as.numeric(cols)

gi.or.11$date <- as.Date(strptime(paste(gi.or.11$year, gi.or.11$week*7-1,sep=" "),format="%Y %j"))
gi.and.11$date <- as.Date(strptime(paste(gi.and.11$year, gi.and.11$week*7-1,sep=" "),format="%Y %j"))
gi.or.novom.11$date <- as.Date(strptime(paste(gi.or.novom.11$year, gi.or.novom.11$week*7-1,sep=" "),format="%Y %j"))
gi.and.novom.11$date <- as.Date(strptime(paste(gi.and.novom.11$year, gi.and.novom.11$week*7-1,sep=" "),format="%Y %j"))

gi.or.novom.10 <- read.csv("gi_or_novom_200910.csv", sep=",", header=T)
gi.and.novom.10 <- read.csv("gi_and_novom_200910.csv", sep=",", header=T)

tmp <- strsplit(as.character(gi.or.novom.10$Week), "-")
cols <- t(sapply(tmp,c))
gi.or.novom.10[,c("year","week")] <- as.numeric(cols)

tmp <- strsplit(as.character(gi.and.novom.10$Week), "-")
cols <- t(sapply(tmp,c))
gi.and.novom.10[,c("year","week")] <- as.numeric(cols)

gi.or.novom.10$date <- as.Date(strptime(paste(gi.or.novom.10$year, gi.or.novom.10$week*7-1,sep=" "),format="%Y %j"))
gi.and.novom.10$date <- as.Date(strptime(paste(gi.and.novom.10$year, gi.and.novom.10$week*7-1,sep=" "),format="%Y %j"))

gi.or <- rbind(gi.or.12, gi.or.11)
gi.and <- rbind(gi.and.12, gi.and.11)
gi.or.novom <- rbind(gi.or.novom.12, gi.or.novom.11, gi.or.novom.10)
gi.and.novom <- rbind(gi.and.novom.12, gi.and.novom.11, gi.and.novom.10)

png(filename="gi_or.png", width=960)
ggplot(gi.or, aes(x=date, y=gi.incidence))+ geom_point(lwd=3.5)+
  scale_y_continuous(name="Weekly incidence (in %)")+ theme_bw(30)+
  scale_x_date(name="")+ opts(panel.grid.major=theme_blank(),
                              panel.grid.minor=theme_blank())
dev.off()

png(filename="gi_and.png", width=960)
ggplot(gi.and, aes(x=date, y=gi.incidence))+ geom_point(lwd=3.5)+
  scale_y_continuous(name="Weekly incidence (in %)")+ theme_bw(30)+
  scale_x_date(name="")+ opts(panel.grid.major=theme_blank(),
                              panel.grid.minor=theme_blank())
dev.off()

png(filename="gi_or_novom.png", width=960)
ggplot(gi.or.novom, aes(x=date, y=gi.incidence))+ geom_point(lwd=3.5)+
  scale_y_continuous(name="Weekly incidence (in %)")+ theme_bw(30)+
  scale_x_date(name="")+ opts(panel.grid.major=theme_blank(),
                              panel.grid.minor=theme_blank())
dev.off()

png(filename="gi_and_novom.png", width=960)
ggplot(gi.and.novom, aes(x=date, y=gi.incidence))+ geom_point(lwd=3.5)+
  scale_y_continuous(name="Weekly incidence (in %)")+ theme_bw(30)+
  scale_x_date(name="")+ opts(panel.grid.major=theme_blank(),
                              panel.grid.minor=theme_blank())
dev.off()

