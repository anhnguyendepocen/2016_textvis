plotWords2 = function (x, y = NULL, words = NULL, wordfreq = NULL, xlab = "", 
                       ylab = "", yaxt = "n", random.y = F, col = color.scale(x, c(1, 2, 0), c(0, 1, 1), 0), ...) 
{
  if (is.null(wordfreq)) 
    wordfreq = rep(1, length(x))
  wordsize = rescale(log(wordfreq), c(0.75, 2))
  if (is.null(y) & random.y) 
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

library(corpustools)

data(sotu)

head(sotu.tokens)

dtm = with(sotu.tokens[sotu.tokens$pos1 %in% c("N", "A", "M"), ],
          dtm.create(aid, lemma))

set.seed(12345)
m = lda.fit(dtm, K=15, alpha=.5)


topics = c("energy", "congress", "people", "boast", "budget", "tax", "terror", "jobs", "nuclear", "social", "health", "human", "nation", "filler", "education")

save(m, dtm, topics, file="model.rda")

# selct words which occur in the topics
words = terms(m, threshold=.001)
words = unique(rle(unlist(words))$values)

# in which topic does each word occur 'most frequently'?
w = m@wordassignments
colnames(w) = m@terms
w = w[, m@terms %in% words]
mostfrequent = function(x) {t = names(sort(table(x[x!=0]), decreasing = T)[1]); if(is.null(t)) 0 else t}
topics = apply(w, MARGIN = 2, FUN = mostfrequent)


rows = as.numeric(rownames(dtm))
cols = as.factor(colnames(dtm))
triples = data.frame(document_id = rows[dtm$i], term=cols[dtm$j], val=dtm$v)
triples = merge(triples, sotu.meta, by.x="document_id", by.y="id", all.x=T)
head(triples)
counts = aggregate(triples["val"], by=triples["term"], FUN=sum)
head(counts)

mmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

dates = aggregate(triples["date"], by=triples["term"], FUN=mmode)
dates$date = as.Date(dates$date)



counts = merge(counts, dates)
counts = counts[order(-counts$val),]
#counts = head(counts, 150)
counts$topic = as.numeric(topics[as.character(counts$term)])

counts$topicname = topics[counts$topic]

head(counts)


.topics = sort(unique(counts$topic))
cols =  sample(rainbow(length(.topics)))
#cols =  rainbow(length(.topics))
counts$col = cols[match(counts$topic, .topics)]


with(head(counts, 150), plotWords2(date, topic-10, words=as.character(term), wordfreq = val, col=col, axes=F))

years = 2000:2016
dates = paste(years, "-01-01", sep = "")

axis(1, at = as.Date(dates), labels = years)
legend("bottom", legend = topics,  text.col = cols, ncol=5)
