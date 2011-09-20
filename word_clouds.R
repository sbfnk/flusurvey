library(RColorBrewer)

x <- runif(19)
y <- runif(19)

lwords =
  c("Fever", "Watery eyes", "Runny nose", "Sneezing", "Sore throat", "Cough",
  "Coloured sputum", "Headache", "Muscle pain", "Chest pain", "Feeling tired",
  "Loss of appetite", "Nausea", "Vomiting", "Diarrhoea", "Other", "Chills",
  "Shortness of breath", "Stomach ache")

df <- read.csv('ill.csv', header=T, sep=',')
df$date <- as.Date(df$date, "%Y/%m/%d")
m <- data.matrix(df)

m_fever <- data.matrix(subset(df, Fever==1))

counts <- x
counts_fever <- x

for (i in 1:19) {
  counts[i] <- sum(m[,i+2])
}

for (i in 1:19) {
  counts_fever[i] <- sum(m_fever[,i+2])
}

pdf('word_cloud.pdf')
plot(x, y, type = "n", axes = FALSE)
tagcloudspiral(lwords, counts_fever, col = rev(brewer.pal(9, "PuBu"))[1:5], maxcex=5)
dev.off()

png('word_cloud.png')
plot(x, y, type = "n", axes = FALSE)
tagcloudspiral(lwords, counts_fever, col = rev(brewer.pal(9, "PuBu"))[1:5], maxcex=5)
dev.off()

pdf('word_cloud_all.pdf')
plot(x, y, type = "n", axes = FALSE)
tagcloudspiral(lwords, counts, col = rev(brewer.pal(9, "PuBu"))[1:5], maxcex=5)
dev.off()

png('word_cloud_all.png')
plot(x, y, type = "n", axes = FALSE)
tagcloudspiral(lwords, counts, col = rev(brewer.pal(9, "PuBu"))[1:5], maxcex=5)
dev.off()

slices <- 5

df_sorted$date_cut <- cut(df_sorted$date, slices)
m_counts <- matrix(0,ncol=length(lwords),nrow=slices)
m_counts_fever<- matrix(0,ncol=length(lwords),nrow=slices)

for (i in 1:slices) {
  for (j in 1:length(lwords)) {
    m_counts[i,j] <- sum(subset(df_sorted, date_cut==levels(df_sorted$date_cut)[i])[,j+2])
    m_counts_fever[i,j] <- sum(subset(df_sorted,
                                      date_cut==levels(df_sorted$date_cut)[i] &
                                      Fever == 1)[,j+2])
  }
}

for (i in 1:slices) {
  pdf(paste("word_cloud_all_", i, ".pdf", sep=""))
  plot(x, y, type = "n", axes = FALSE)
  tagcloudspiral(lwords, m_counts[i,], col = rev(brewer.pal(9, "PuBu"))[1:5],
                 maxcex=max(m_counts[i,])/max(m_counts)*5)
  dev.off()
  png(paste("word_cloud_all_", i, ".png", sep=""))
  plot(x, y, type = "n", axes = FALSE)
  tagcloudspiral(lwords, m_counts[i,], col = rev(brewer.pal(9, "PuBu"))[1:5],
                 maxcex=max(m_counts[i,])/max(m_counts)*5)
  dev.off()
  pdf(paste("word_cloud_", i, ".pdf", sep=""))
  plot(x, y, type = "n", axes = FALSE)
  tagcloudspiral(lwords, m_counts_fever[i,], col = rev(brewer.pal(9, "PuBu"))[1:5],
                 maxcex=max(m_counts_fever[i,])/max(m_counts_fever)*5)
  dev.off()
  png(paste("word_cloud_", i, ".png", sep=""))
  plot(x, y, type = "n", axes = FALSE)
  tagcloudspiral(lwords, m_counts_fever[i,], col = rev(brewer.pal(9, "PuBu"))[1:5],
                 maxcex=max(m_counts_fever[i,])/max(m_counts_fever)*5)
  dev.off()
}
