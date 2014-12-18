library(ggplot2)

data <- read.csv('swabbing.csv', sep=",", header=T)
data <- data[!is.na(data$swabbing),]

png("swabbing_question")
ggplot(data, aes(swabbing, fill=swabbing))+ geom_histogram()+
  opts(legend.position="none")+ scale_fill_brewer(palette="Set1")
dev.off()
