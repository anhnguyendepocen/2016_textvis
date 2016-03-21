source("functions.R")
library(corpustools)
data(sotu)
load("model.rda")
table(sotu.meta$headline)
dtm.obama = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "Barack Obama"],]
dtm.bush = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "George W. Bush"],]

cmp = corpustools::corpora.compare(dtm.obama, dtm.bush)
cmp$freq = cmp$termfreq.x + cmp$termfreq.y

wordfreqs = dtm.to.df(dtm)
wordfreqs = merge(sotu.meta, wordfreqs, by.x="id", by.y="doc")
dates = aggregate(wordfreqs["date"], by=wordfreqs["term"], FUN=mmode)
cmp = merge(cmp, dates)

adates = aggregate(list(avgdate=wordfreqs$date), by=wordfreqs["term"], FUN=mean)
cmp = merge(cmp, adates)

# define word color based on over and chi
h = rescale(log(cmp$over), c(1, .6666))
s = rescale(sqrt(cmp$chi), c(0,1))
cmp$col = hsv(h, s, .33 + .67*s)
cmp = arrange(cmp, -freq)
save(cmp, file="word_contrast.rda")

# contrast plot
cmp = arrange(cmp, -freq)
with(head(cmp, 130), plotWords2(x=log(over), words=term, wordfreq=freq, random.y = T, col=col))

text(-2, 0, "Bush", srt=90, col="red", cex=2)
text(2, 0, "Obama", srt=90, col="blue", cex=2)
title(xlab="Overrepresentation")

# over time
with(head(cmp, 150), 
     plotWords2(x=date, words=term, wordfreq=freq, random.y = T, col=col))


# over time
cmp = arrange(cmp, -freq)
with(head(cmp, 130), 
     plotWords2(x=date, words=term, wordfreq=freq, random.y = T, col=col))
with(head(cmp, 150), 
     plotWords2(x=avgdate, words=term, wordfreq=freq, random.y = T, col=col))


cmp = arrange(cmp, -chi)
with(head(cmp, 130), 
     plotWords2(x=date, words=term, wordfreq=freq, random.y = T, col=col))



