library(corpustools)
data(sotu)

# Create dtm
dtm = with(sotu.tokens[sotu.tokens$pos1 %in% c("N", "A", "M"), ],
           dtm.create(aid, lemma))

# compare obama vs bush
dtm.obama = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "Barack Obama"],]
dtm.bush = dtm[rownames(dtm) %in% sotu.meta$id[sotu.meta$headline == "George W. Bush"],]

cmp = corpustools::corpora.compare(dtm.obama, dtm.bush)
cmp$freq = cmp$termfreq.x + cmp$termfreq.y

wordfreqs = dtm.to.df(dtm)
wordfreqs = merge(sotu.meta, wordfreqs, by.x="id", by.y="doc")

mmode <- function(v) {uniqv <- unique(v); uniqv[which.max(tabulate(match(v, uniqv)))]}
dates = aggregate(wordfreqs["date"], by=wordfreqs["term"], FUN=mmode)
cmp = merge(cmp, dates)

# define word color based on over and chi
h = rescale(log(cmp$over), c(1, .6666))
s = rescale(sqrt(cmp$chi), c(0,1))
cmp$col = hsv(h, s, .33 + .67*s)

# Create topic model
set.seed(12345)
m = lda.fit(dtm, K=15, alpha=.5)
topics = c("job", "education", "nuclear", "health", "terror", "americans", "boast", "budget", "future", "freedom", "reform", "energy", "tax", "congress", "US")


# in which topic does each word occur 'most frequently'?
words = terms(m, threshold=.001)
words = unique(rle(unlist(words))$values)
w = m@wordassignments
colnames(w) = m@terms
w = w[, m@terms %in% words]
mostfrequent = function(x) {t = names(sort(table(x[x!=0]), decreasing = T)[1]); if(is.null(t)) 0 else t}
word.topics = apply(w, MARGIN = 2, FUN = mostfrequent)
cmp$topic = as.numeric(word.topics[as.character(cmp$term)])
cmp$topic.name = factor(cmp$topic, labels = topics)
cols = sample(rainbow(length(topics)))
cols = sample(substr(rainbow(length(topics), s=0.6,alpha=0.5), 1,7))

cmp$topic.col = cols[cmp$topic]



save(m, cmp, dtm, topics, file="data.rda")
