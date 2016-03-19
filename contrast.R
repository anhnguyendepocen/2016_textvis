library(corpustools)
data(sotu)
load("model.rda")
table(sotu.meta$headline)
dtm.obama = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "Barack Obama"],]
dtm.bush = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "George W. Bush"],]

cmp = corpustools::corpora.compare(dtm.obama, dtm.bush)
cmp$freq = cmp$termfreq.x + cmp$termfreq.y
cmp = arrange(cmp, -freq)
cmp = head(cmp, 200)

col = hsv()

col = color.scale(log(cmp$over), c(.5, 1, 0), c(0, 1, 1), 0)

h = rescale(log(cmp$over), c(1, .6666))
s = rescale(sqrt(cmp$chi), c(0,1))
col = hsv(h, s, .5)
col = hsv(h, 1, .5)
plotWords2(x=log(cmp$over), words=cmp$term, wordfreq=cmp$freq, random.y = T, col=col)

plot(log(sample(cmp$over, 100)), ylim=c(-2,2))

wordsize = rescale(log(cmp$freq), c(0.75, 2))

x = exp(cmp$over)
plot(log(cmp$over))

  zif (is.null(y) & random.y) 
    y = sample(seq(-1, 1, by = 0.01), length(x))
  if (is.null(y) & !random.y) 
    y = wordsize
  xmargin = (max(x) - min(x)) * 0.2
  ymargin = (max(y) - min(y)) * 0.2
  xlim = c(min(x) - xmargin, max(x) + xmargin)
  ylim = c(min(y) - ymargin, max(y) + ymargin)
  
  plot(x, y, type = "n", xlim = xlim, ylim = ylim, frame.plot = F, 
       yaxt = yaxt, ylab = ylab, xlab = xlab, ...)
  wl <- as.data.frame(wordlayout(x, y, words, cex = wordsize))
  
  text(wl$x + 0.5 * wl$width, wl$y + 0.5 * wl$ht, words, cex = wordsize, 
       col = col)
}
