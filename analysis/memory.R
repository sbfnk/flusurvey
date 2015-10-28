library(data.table)

dt.short <-
  dt[,c("week", "bid", "vaccine", "fever", "chills", "blocked.runny.nose",
  "sneezing", "sore.throat", "cough", "shortness.breath", "headache",
  "muscle.and.or.joint.pain", "chest.pain", "tired", "loss.appetite", "phlegm",
  "watery.eyes", "nausea", "vomiting", "diarrhoea", "stomach.ache",
  "other", "fever.suddenly", "visit.medical.service.no",
  "contact.medical.service.no", "no.medication", "alter.routine",
  "ili", "ili.fever", "ili.self", "age", "agegroup", "Q6b",
  "frequent.contact.children", "frequent.contact.elderly",
  "frequent.contact.people", "smoke", "atrisk", "date",
  "symptoms.start.week", "same", "no.symptoms"),with=F]   

dt.short$added.2week <- F
dt.short$added.3week <- F

lastrow <- nrow(dt.short)
dt2 <- dt.short
dt3 <- dt.short
counter <- 1

## dt2$date <- dt$date - 7
## dt2$week <- format(dt2$date, format="%G-%W")

for (i in 1:lastrow) {
  ## if (nrow(dt[bid == dt2[i]$bid & week == dt2[i]$week]) == 0) {
  ##    if (is.na(dt[i]$symptoms.start.week) |
  ##        dt[i]$symptoms.start.week != dt2[i]$week) {
  ##      ## dt <- rbind(dt, dt[i])
  ##      dt2 <- dt2[i, same:=0]
  ##      dt2 <- dt2[i, date := dt[i]$date]
  ##      dt2[newrow]$date <- as.Date(date.before)
  ##      dt2[newrow]$week <- week.before
  ##      dt2[newrow]$added.2week <- T
  ##      counter <- counter + 1
  ##    }

  date.before <- dt.short[i]$date - 7
  week.before <- format(date.before, format="%G-%W")
  date.before.before <- dt.short[i]$date - 14
  week.before.before <- format(date.before.before, format="%G-%W")
  if (nrow(dt.short[bid == dt.short[i]$bid & week == week.before]) == 0) {
    if (is.na(dt.short[i]$symptoms.start.week) |
        dt.short[i]$symptoms.start.week != week.before) {
      ## dt <- rbind(dt, dt.short[i])
      newrow <- counter
      dt2[newrow] <- dt.short[i]
      dt2 <- dt2[newrow, same := as.integer(0)]
      dt2 <- dt2[newrow, date := as.integer(date.before)]
      dt2 <- dt2[newrow, week := week.before]
      dt2 <- dt2[newrow, added.2week := T]
      counter <- counter + 1
    }
    if (nrow(dt.short[bid == dt.short[i]$bid & week == week.before.before]) == 0) {
      if (is.na(dt.short[i]$symptoms.start.week) |
          dt.short[i]$symptoms.start.week != week.before.before) {
        if (is.na(dt.short[i]$symptoms.start.week) |
            dt.short[i]$symptoms.start.week != week.before) {
          ## dt <- rbind(dt, dt.short[i])
          ## newrow <- nrow(dt)
          newrow <- counter
          dt2[newrow] <- dt.short[i]
          dt2 <- dt2[newrow, same := as.integer(0)]
          dt2 <- dt2[newrow, date := as.integer(date.before.before)]
          dt2 <- dt2[newrow, week := week.before.before]
          dt2 <- dt2[newrow, added.3week := T]
          counter <- counter + 1
        } else {
          ## dt <- rbind(dt, dt.short[i])
          ## newrow <- nrow(dt)
          newrow <- counter
          dt2[newrow] <- dt.short[i]
          dt2 <- dt2[newrow, same := as.integer(0)]
          dt2 <- dt2[newrow, date := as.integer(date.before.before)]
          dt2 <- dt2[newrow, week := week.before.before]
          dt2 <- dt2[newrow, no.symptoms := "t"]
          dt2 <- dt2[newrow, symptoms := "f", with=F]
          dt2 <- dt2[newrow, added.3week := T]
          counter <- counter + 1
        }
      }
    }
  }
}

dt.added <- rbind(dt.short, dt2[1:(counter-1),])
dt.added[dt.added$week=="2011-00"]$week <- "2011-52"

incidence <-
  data.table(expand.grid(
             yearweek = levels(factor(dt.added$week)),
             measure = c("realtime.report", "realtime.symptoms", "retrospective.symptoms"),
             weeks = as.factor(seq(0, 3)),
             def = c("ili", "self", "fever"),
             value = 0.0
             ))

for (thisweek in levels(factor(dt.added$week))) {
  week.all <- dt.added[week == thisweek]
  week.all <- week.all[added.3week == F & added.2week == F]
  week.users <- week.all[!duplicated(week.all$bid)]
  incidence[yearweek == thisweek & weeks == 0 & def == "ili" & measure == "realtime.report"]$value <-
    nrow(week.users[ili==1 & same != 0])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 0 & def == "self" & measure == "realtime.report"]$value <-
    nrow(week.users[ili.self==1 & same!=0])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 0 & def == "fever" & measure == "realtime.report"]$value <-
    nrow(week.users[ili.fever==1 & same!=0])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 1 & def == "ili" & measure == "realtime.symptoms"]$value <-
    nrow(week.users[ili==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 1 & def == "self" & measure == "realtime.symptoms"]$value <-
    nrow(week.users[ili.self==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 1 & def == "fever" & measure == "realtime.symptoms"]$value <-
    nrow(week.users[ili.fever==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)

  week.all <- dt.added[(week == thisweek & (is.na(symptoms.start.week) | same == 0)) |
                       (symptoms.start.week == thisweek & same != 0)]
  week.all <- week.all[added.3week == F & added.2week == F]
  week.users <- week.all[!duplicated(week.all$bid)]
  incidence[yearweek == thisweek & weeks == 1 & def == "ili" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 1 & def == "self" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.self==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 1 & def == "fever" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.fever==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)

  week.all <- dt.added[(week == thisweek & is.na(symptoms.start.week)) |
                       symptoms.start.week == thisweek]
  week.all <- week.all[added.3week==F]
  week.users <- week.all[!duplicated(week.all$bid)]
  incidence[yearweek == thisweek & weeks == 2 & def == "ili" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 2 & def == "self" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.self==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 2 & def == "fever" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.fever==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)

  week.all <- dt.added[(week == thisweek & is.na(symptoms.start.week)) |
                       symptoms.start.week == thisweek]
  week.users <- week.all[!duplicated(week.all$bid)]
  incidence[yearweek == thisweek & weeks == 3 & def == "ili" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 3 & def == "self" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.self==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
  incidence[yearweek == thisweek & weeks == 3 & def == "fever" & measure == "retrospective.symptoms"]$value <-
    nrow(week.users[ili.fever==1 & same != 0 & symptoms.start.week == thisweek])/nrow(week.users)
}

incidence <- incidence[as.character(yearweek) > "2011-45" & as.character(yearweek) < "2012-19"]
incidence$year <- as.numeric(substr(incidence$yearweek, 1, 4))
incidence$week <- as.numeric(substr(incidence$yearweek, 6, 7))
incidence$date <- as.Date(strptime(paste(incidence$year, incidence$week*7,sep=" "),format="%Y %j"))+5
incidence <- incidence[value > 0]

for (definition in unique(incidence$def)) {
  pdf(paste("memory_", definition, ".pdf", sep=""), width=10)
  print(
        ggplot(droplevels(incidence[as.numeric(weeks) > 1 & measure == "retrospective.symptoms" & def==definition]), aes(x=date, y=value, group=weeks, color=weeks))+
        geom_line()+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Weekly incidence")+
        theme_bw(30)+
        scale_x_date(name="")+
        scale_color_brewer(palette="Set1")
        )
  dev.off()
  pdf(paste("realtime_", definition, ".pdf", sep=""), width=10)
  print(
        ggplot(incidence[as.numeric(weeks) < 3 & def==definition], aes(x=date, y=value, group=measure, color=measure))+
        geom_line()+
        geom_point(size=3, shape=21, fill="white")+
        scale_y_continuous(name="Weekly incidence")+
        theme_bw(30)+
        scale_x_date(name="")+
        scale_color_brewer(palette="Set1")
        )
  dev.off()
}
