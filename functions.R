plotWords2 = function (x, y = NULL, words = NULL, wordfreq = NULL, xlab = "", 
                       ylab = "", yaxt = "n", random.y = F, col = color.scale(x, c(1, 2, 0), c(0, 1, 1), 0), ...) 
{
  if (is.null(wordfreq)) 
    wordfreq = rep(1, length(x))
  wordsize = rescale(log(wordfreq), c(0.75, 2))
  if (is.null(y) & random.y) 
    y = sample(seq(-1, 1, by = 0.001), length(x))
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

mmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
dtm.to.df <- function(dtm) {
  terms = factor(dtm$j, labels = colnames(dtm))
  docs = factor(dtm$i, labels = rownames(dtm))
  data.frame(doc=docs, term=terms, freq=dtm$v)
}