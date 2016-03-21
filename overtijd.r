library(corpustools)
data(sotu)
load("model.rda")

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
