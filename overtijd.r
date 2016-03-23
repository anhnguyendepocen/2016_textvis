library(corpustools)
load("data.rda")
head(cmp)

with(head(cmp, 100), plotWords(date, topic, words=as.character(term), wordfreq = freq, col=topic.col, axes=F))

years = 2000:2016
dates = paste(years, "-01-01", sep = "")

axis(1, at = as.Date(dates), labels = years)
