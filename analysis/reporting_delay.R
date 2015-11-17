library(data.table)

# reporting delay distributions
dt$diffdays.onset <- as.numeric(as.Date(dt$timestamp.1)-as.Date(dt$symptoms.start.date))

pdf("onset_all_symptoms.pdf")
ggplot(dt[same != 0 & diffdays.onset>=0 & diffdays.onset < 57], aes(x = diffdays.onset, y=..density..)) + geom_histogram(binwidth=1) + scale_x_continuous("days between symptom onset and report")+ scale_y_continuous("proportion of all symptom reports")+ theme_bw()
dev.off()

pdf("onset_ili.pdf")
ggplot(dt[diffdays.onset>=0 & diffdays.onset < 57 & ili==T], aes(x = diffdays.onset, y=..density..)) + geom_histogram(binwidth=1) + scale_x_continuous("days between symptom onset and report")+ scale_y_continuous("proportion of symptom reports with ILI")+ theme_bw()
dev.off()

nrow(dt[diffdays.onset>7])/nrow(dt[diffdays.onset>=0])
nrow(dt[diffdays.onset>14])/nrow(dt[diffdays.onset>=0])
nrow(dt[diffdays.onset>21])/nrow(dt[diffdays.onset>=0])
nrow(dt[diffdays.onset>28])/nrow(dt[diffdays.onset>=0])
nrow(dt[diffdays.onset<0])/nrow(dt[!is.na(diffdays.onset)])

nrow(dt[same != 0 & diffdays.onset>7])/nrow(dt[same != 0 & diffdays.onset>=0])
nrow(dt[same != 0 & diffdays.onset>14])/nrow(dt[same != 0 & diffdays.onset>=0])
nrow(dt[same != 0 & diffdays.onset>21])/nrow(dt[same != 0 & diffdays.onset>=0])
nrow(dt[same != 0 & diffdays.onset>28])/nrow(dt[same != 0 & diffdays.onset>=0])
nrow(dt[same != 0 & diffdays.onset<0])/nrow(dt[same != 0 & !is.na(diffdays.onset)])

nrow(dt[ili==T & diffdays.onset>7])/nrow(dt[ili==T & diffdays.onset>=0])
nrow(dt[ili==T & diffdays.onset>14])/nrow(dt[ili==T & diffdays.onset>=0])
nrow(dt[ili==T & diffdays.onset>21])/nrow(dt[ili==T & diffdays.onset>=0])
nrow(dt[ili==T & diffdays.onset>28])/nrow(dt[ili==T & diffdays.onset>=0])
nrow(dt[ili==T & diffdays.onset<0])/nrow(dt[ili==T & !is.na(diffdays.onset)])

nrow(dt[ili.fever==T & diffdays.onset>7])/nrow(dt[ili.fever==T & diffdays.onset>=0])
nrow(dt[ili.fever==T & diffdays.onset>14])/nrow(dt[ili.fever==T & diffdays.onset>=0])
nrow(dt[ili.fever==T & diffdays.onset>21])/nrow(dt[ili.fever==T & diffdays.onset>=0])
nrow(dt[ili.fever==T & diffdays.onset>28])/nrow(dt[ili.fever==T & diffdays.onset>=0])
nrow(dt[ili.fever==T & diffdays.onset<0])/nrow(dt[ili.fever==T & !is.na(diffdays.onset)])

dt$diffdays.end <- as.numeric(as.Date(dt$timestamp.1)-as.Date(dt$symptoms.end.date))

pdf("end_all_symptoms.pdf")
ggplot(dt[diffdays.end>=0 & diffdays.end < 57], aes(x = diffdays.end, y=..density..)) + geom_histogram(binwidth=1) + scale_x_continuous("days between symptom end and report")+ scale_y_continuous("proportion of all symptom reports")+ theme_bw()
dev.off()

pdf("end_ili.pdf")
ggplot(dt[diffdays.end>=0 & diffdays.end < 57 & ili==T], aes(x = diffdays.end, y=..density..)) + geom_histogram(binwidth=1) + scale_x_continuous("days between symptom end and report")+ scale_y_continuous("proportion of symptom reports with ILI")+ theme_bw()
dev.off()

nrow(dt[diffdays.end>7])/nrow(dt[diffdays.end>=0])
nrow(dt[diffdays.end>14])/nrow(dt[diffdays.end>=0])
nrow(dt[diffdays.end>21])/nrow(dt[diffdays.end>=0])
nrow(dt[diffdays.end>28])/nrow(dt[diffdays.end>=0])
nrow(dt[diffdays.end<0])/nrow(dt[!is.na(diffdays.end)])

nrow(dt[ili==T & diffdays.end>7])/nrow(dt[ili==T & diffdays.end>=0])
nrow(dt[ili==T & diffdays.end>14])/nrow(dt[ili==T & diffdays.end>=0])
nrow(dt[ili==T & diffdays.end>21])/nrow(dt[ili==T & diffdays.end>=0])
nrow(dt[ili==T & diffdays.end>28])/nrow(dt[ili==T & diffdays.end>=0])
nrow(dt[ili==T & diffdays.end<0])/nrow(dt[ili==T & !is.na(diffdays.end)])

nrow(dt[ili.fever==T & diffdays.end>7])/nrow(dt[ili.fever==T & diffdays.end>=0])
nrow(dt[ili.fever==T & diffdays.end>14])/nrow(dt[ili.fever==T & diffdays.end>=0])
nrow(dt[ili.fever==T & diffdays.end>21])/nrow(dt[ili.fever==T & diffdays.end>=0])
nrow(dt[ili.fever==T & diffdays.end>28])/nrow(dt[ili.fever==T & diffdays.end>=0])
nrow(dt[ili.fever==T & diffdays.end<0])/nrow(dt[ili.fever==T & !is.na(diffdays.end)])

