## contacts <- read.csv('contact.csv', sep=',', header=T)
## intake <- read.csv('intake.csv', sep=',', header=T)

age <- c(108,113,118)
setting <- 108:112

contacts_pc$age1_conversational <- rowSums(contacts_pc[,age,with=F])
contacts_pc$age2_conversational <- rowSums(contacts_pc[,age+1,with=F])
contacts_pc$age3_conversational <- rowSums(contacts_pc[,age+2,with=F])
contacts_pc$age4_conversational <- rowSums(contacts_pc[,age+3,with=F])
contacts_pc$age5_conversational <- rowSums(contacts_pc[,age+4,with=F])

contacts_pc$age1_physical <- rowSums(contacts_pc[,age+15,with=F])
contacts_pc$age2_physical <- rowSums(contacts_pc[,age+15+1,with=F])
contacts_pc$age3_physical <- rowSums(contacts_pc[,age+15+2,with=F])
contacts_pc$age4_physical <- rowSums(contacts_pc[,age+15+3,with=F])
contacts_pc$age5_physical <- rowSums(contacts_pc[,age+15+4,with=F])

contacts_pc$age1 <- contacts_pc$age1_conversational + contacts_pc$age1_physical
contacts_pc$age2 <- contacts_pc$age2_conversational + contacts_pc$age2_physical
contacts_pc$age3 <- contacts_pc$age3_conversational + contacts_pc$age3_physical
contacts_pc$age4 <- contacts_pc$age4_conversational + contacts_pc$age4_physical
contacts_pc$age5 <- contacts_pc$age5_conversational + contacts_pc$age5_physical

contacts_pc$setting1_conversational <- rowSums(contacts_pc[,setting,with=F])
contacts_pc$setting2_conversational <- rowSums(contacts_pc[,setting+5,with=F])
contacts_pc$setting3_conversational <- rowSums(contacts_pc[,setting+10,with=F])

contacts_pc$setting1_physical <- rowSums(contacts_pc[,setting+15,with=F])
contacts_pc$setting2_physical <- rowSums(contacts_pc[,setting+20,with=F])
contacts_pc$setting3_physical <- rowSums(contacts_pc[,setting+25,with=F])

contacts_pc$setting1 <- contacts_pc$setting1_conversational + contacts_pc$setting1_physical
contacts_pc$setting2 <- contacts_pc$setting2_conversational + contacts_pc$setting2_physical
contacts_pc$setting3 <- contacts_pc$setting3_conversational + contacts_pc$setting3_physical

contacts_pc$conversational <-
  contacts_pc$setting1_conversational +
  contacts_pc$setting2_conversational +
  contacts_pc$setting3_conversational

contacts_pc$physical <-
  contacts_pc$setting1_physical +
  contacts_pc$setting2_physical +
  contacts_pc$setting3_physical

contacts_pc$contacts <- 
  contacts_pc$physical +
  contacts_pc$conversational

#intake_df <- melt(intake,id=c("id","user","channel","timestamp","NOTE","global_id"))
contact_df <-
  melt(contacts,id=c("id","user","channel","timestamp", "NOTE","global_id",
                  "Q3", "Q4", "Q5"))
contact_df <- melt(contacts_pc,
                   measure.vars=c("Q1_multi_row1_col1", "Q1_multi_row1_col2",
                     "Q1_multi_row1_col3", "Q1_multi_row1_col4",
                     "Q1_multi_row1_col5", "Q1_multi_row2_col1",
                     "Q1_multi_row2_col2", "Q1_multi_row2_col3",
                     "Q1_multi_row2_col4", "Q1_multi_row2_col5",
                     "Q1_multi_row3_col1", "Q1_multi_row3_col2",
                     "Q1_multi_row3_col3", "Q1_multi_row3_col4",
                     "Q1_multi_row3_col5", "Q2_multi_row1_col1",
                     "Q2_multi_row1_col2", "Q2_multi_row1_col3",
                     "Q2_multi_row1_col4", "Q2_multi_row1_col5",
                     "Q2_multi_row2_col1", "Q2_multi_row2_col2",
                     "Q2_multi_row2_col3", "Q2_multi_row2_col4",
                     "Q2_multi_row2_col5", "Q2_multi_row3_col1",
                     "Q2_multi_row3_col2", "Q2_multi_row3_col3",
                     "Q2_multi_row3_col4", "Q2_multi_row3_col5"))

#df <- rbind(contact_df, intake_df)

df <- contact_df
df$type <- "Unknown"
df$age <- "Unknown"
df$setting <- "Unknown"

df[df$variable=="Q1_multi_row1_col1",]$setting <- "Home"
df[df$variable=="Q1_multi_row1_col2",]$setting <- "Home"
df[df$variable=="Q1_multi_row1_col3",]$setting <- "Home"
df[df$variable=="Q1_multi_row1_col4",]$setting <- "Home"
df[df$variable=="Q1_multi_row1_col5",]$setting <- "Home"
df[df$variable=="Q2_multi_row1_col1",]$setting <- "Home"
df[df$variable=="Q2_multi_row1_col2",]$setting <- "Home"
df[df$variable=="Q2_multi_row1_col3",]$setting <- "Home"
df[df$variable=="Q2_multi_row1_col4",]$setting <- "Home"
df[df$variable=="Q2_multi_row1_col5",]$setting <- "Home"

df[df$variable=="Q1_multi_row2_col1",]$setting <- "Work/School"
df[df$variable=="Q1_multi_row2_col2",]$setting <- "Work/School"
df[df$variable=="Q1_multi_row2_col3",]$setting <- "Work/School"
df[df$variable=="Q1_multi_row2_col4",]$setting <- "Work/School"
df[df$variable=="Q1_multi_row2_col5",]$setting <- "Work/School"
df[df$variable=="Q2_multi_row2_col1",]$setting <- "Work/School"
df[df$variable=="Q2_multi_row2_col2",]$setting <- "Work/School"
df[df$variable=="Q2_multi_row2_col3",]$setting <- "Work/School"
df[df$variable=="Q2_multi_row2_col4",]$setting <- "Work/School"
df[df$variable=="Q2_multi_row2_col5",]$setting <- "Work/School"

df[df$variable=="Q1_multi_row3_col1",]$setting <- "Other"
df[df$variable=="Q1_multi_row3_col2",]$setting <- "Other"
df[df$variable=="Q1_multi_row3_col3",]$setting <- "Other"
df[df$variable=="Q1_multi_row3_col4",]$setting <- "Other"
df[df$variable=="Q1_multi_row3_col5",]$setting <- "Other"
df[df$variable=="Q2_multi_row3_col1",]$setting <- "Other"
df[df$variable=="Q2_multi_row3_col2",]$setting <- "Other"
df[df$variable=="Q2_multi_row3_col3",]$setting <- "Other"
df[df$variable=="Q2_multi_row3_col4",]$setting <- "Other"
df[df$variable=="Q2_multi_row3_col5",]$setting <- "Other"

df[df$variable=="Q1_multi_row1_col1",]$age <- "0-4 years"
df[df$variable=="Q1_multi_row2_col1",]$age <- "0-4 years"
df[df$variable=="Q1_multi_row3_col1",]$age <- "0-4 years"
df[df$variable=="Q2_multi_row1_col1",]$age <- "0-4 years"
df[df$variable=="Q2_multi_row2_col1",]$age <- "0-4 years"
df[df$variable=="Q2_multi_row3_col1",]$age <- "0-4 years"

df[df$variable=="Q1_multi_row1_col2",]$age <- "5-18 years"
df[df$variable=="Q1_multi_row2_col2",]$age <- "5-18 years"
df[df$variable=="Q1_multi_row3_col2",]$age <- "5-18 years"
df[df$variable=="Q2_multi_row1_col2",]$age <- "5-18 years"
df[df$variable=="Q2_multi_row2_col2",]$age <- "5-18 years"
df[df$variable=="Q2_multi_row3_col2",]$age <- "5-18 years"

df[df$variable=="Q1_multi_row1_col3",]$age <- "19-44 years"
df[df$variable=="Q1_multi_row2_col3",]$age <- "19-44 years"
df[df$variable=="Q1_multi_row3_col3",]$age <- "19-44 years"
df[df$variable=="Q2_multi_row1_col3",]$age <- "19-44 years"
df[df$variable=="Q2_multi_row2_col3",]$age <- "19-44 years"
df[df$variable=="Q2_multi_row3_col3",]$age <- "19-44 years"

df[df$variable=="Q1_multi_row1_col4",]$age <- "45-64 years"
df[df$variable=="Q1_multi_row2_col4",]$age <- "45-64 years"
df[df$variable=="Q1_multi_row3_col4",]$age <- "45-64 years"
df[df$variable=="Q2_multi_row1_col4",]$age <- "45-64 years"
df[df$variable=="Q2_multi_row2_col4",]$age <- "45-64 years"
df[df$variable=="Q2_multi_row3_col4",]$age <- "45-64 years"

df[df$variable=="Q1_multi_row1_col5",]$age <- "65+ years"
df[df$variable=="Q1_multi_row2_col5",]$age <- "65+ years"
df[df$variable=="Q1_multi_row3_col5",]$age <- "65+ years"
df[df$variable=="Q2_multi_row1_col5",]$age <- "65+ years"
df[df$variable=="Q2_multi_row2_col5",]$age <- "65+ years"
df[df$variable=="Q2_multi_row3_col5",]$age <- "65+ years"

df[df$variable=="Q1_multi_row1_col1",]$type <- "conversational"
df[df$variable=="Q1_multi_row1_col2",]$type <- "conversational"
df[df$variable=="Q1_multi_row1_col3",]$type <- "conversational"
df[df$variable=="Q1_multi_row1_col4",]$type <- "conversational"
df[df$variable=="Q1_multi_row1_col5",]$type <- "conversational"
df[df$variable=="Q1_multi_row2_col1",]$type <- "conversational"
df[df$variable=="Q1_multi_row2_col2",]$type <- "conversational"
df[df$variable=="Q1_multi_row2_col3",]$type <- "conversational"
df[df$variable=="Q1_multi_row2_col4",]$type <- "conversational"
df[df$variable=="Q1_multi_row2_col5",]$type <- "conversational"
df[df$variable=="Q1_multi_row3_col1",]$type <- "conversational"
df[df$variable=="Q1_multi_row3_col2",]$type <- "conversational"
df[df$variable=="Q1_multi_row3_col3",]$type <- "conversational"
df[df$variable=="Q1_multi_row3_col4",]$type <- "conversational"
df[df$variable=="Q1_multi_row3_col5",]$type <- "conversational"

df[df$variable=="Q2_multi_row1_col1",]$type <- "physical"
df[df$variable=="Q2_multi_row1_col2",]$type <- "physical"
df[df$variable=="Q2_multi_row1_col3",]$type <- "physical"
df[df$variable=="Q2_multi_row1_col4",]$type <- "physical"
df[df$variable=="Q2_multi_row1_col5",]$type <- "physical"
df[df$variable=="Q2_multi_row2_col1",]$type <- "physical"
df[df$variable=="Q2_multi_row2_col2",]$type <- "physical"
df[df$variable=="Q2_multi_row2_col3",]$type <- "physical"
df[df$variable=="Q2_multi_row2_col4",]$type <- "physical"
df[df$variable=="Q2_multi_row2_col5",]$type <- "physical"
df[df$variable=="Q2_multi_row3_col1",]$type <- "physical"
df[df$variable=="Q2_multi_row3_col2",]$type <- "physical"
df[df$variable=="Q2_multi_row3_col3",]$type <- "physical"
df[df$variable=="Q2_multi_row3_col4",]$type <- "physical"
df[df$variable=="Q2_multi_row3_col5",]$type <- "physical"

cdf <- subset(df, setting!="Unknown")

cdf$age <- factor(cdf$age)
cdf$setting <- factor(cdf$setting)
cdf$type <- factor(cdf$type)


"Q1_multi_row1_col1","Q1_multi_row1_col2","Q1_multi_row1_col3","Q1_multi_row1_col4","Q1_multi_row1_col5","Q1_multi_row2_col1","Q1_multi_row2_col2","Q1_multi_row2_col3","Q1_multi_row2_col4","Q1_multi_row2_col5","Q1_multi_row3_col1","Q1_multi_row3_col2","Q1_multi_row3_col3","Q1_multi_row3_col4","Q1_multi_row3_col5","Q2_multi_row1_col1","Q2_multi_row1_col2","Q2_multi_row1_col3","Q2_multi_row1_col4","Q2_multi_row1_col5","Q2_multi_row2_col1","Q2_multi_row2_col2","Q2_multi_row2_col3","Q2_multi_row2_col4","Q2_multi_row2_col5","Q2_multi_row3_col1","Q2_multi_row3_col2","Q2_multi_row3_col3","Q2_multi_row3_col4","Q2_multi_row3_col5"
