library(corpustools)
load("data.rda")

cmp = arrange(cmp, -freq)

with(head(cmp, 130), plotWords(x=log(over), words=term, wordfreq=freq, random.y = T, col=col))

text(-2, 0, "Bush", srt=90, col="red", cex=2)
text(2, 0, "Obama", srt=90, col="blue", cex=2)
title(xlab="Overrepresentation")

# over time
with(head(cmp, 150), 
     plotWords(x=date, words=term, wordfreq=freq, random.y = T, col=col))

cmp = arrange(cmp, -chi)
with(head(cmp, 130), 
     plotWords2(x=date, words=term, wordfreq=freq, random.y = T, col=col))

