library(corpustools)

data(sotu)

head(sotu.tokens)

dtm = with(sotu.tokens[sotu.tokens$pos1 %in% c("N", "A", "M"), ],
           dtm.create(aid, lemma))

set.seed(12345)
m = lda.fit(dtm, K=15, alpha=.5)


topics = c("energy", "congress", "people", "boast", "budget", "tax", "terror", "jobs", "nuclear", "social", "health", "human", "nation", "filler", "education")
topics = c("job", "education", "nuclear", "health", "terror", "americans", "boast", "budget", "future", "freedom", "reform", "energy", "tax", "congress", "US")

save(m, dtm, topics, file="model.rda")
