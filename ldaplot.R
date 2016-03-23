library(corpustools)
load("data.rda")


compare.topics <- function(m, cmp_topics) {
  assignments = data.frame(doc = m@wordassignments$i, term = m@wordassignments$j, topic = m@wordassignments$v)
  tpw = acast(assignments, term ~ topic, value.var = "doc", fun.aggregate = length)
  rownames(tpw) = m@terms
  terms = as.data.frame(tpw[,cmp_topics])
  terms$freq = rowSums(terms)
  terms = terms[terms$freq > 0,]
  terms$prop = terms[[1]] / terms$freq
  terms$col = hsv(rescale(terms$prop, c(1, .6666)), .5, .5)
  terms$nn = rowSums(terms[1:2]>0)
  terms$term = rownames(terms)
  arrange(terms, -nn, -freq)
}


terms = compare.topics(m2, match(c("freedom", "war"), topics2))
with(head(terms, 100), plotWords(x=prop, wordfreq = freq, words = term, col=col, xaxt="none", random.y = T, main="Topic words for War on Terror and Nuclear Proliferation", sub="Color and position indicate topic"))
axis(1, at=c(0, 0.5, 1), labels = c("War on Terror", "", "Nuclear Proliferation"))




terms = compare.topics(m, match(c("nuclear", "terror"), topics))
with(head(terms, 100), plotWords(x=prop, wordfreq = freq, words = term, col=col, xaxt="none", random.y = T, main="Topic words for War on Terror and Nuclear Proliferation", sub="Color and position indicate topic"))
axis(1, at=c(0, 0.5, 1), labels = c("War on Terror", "", "Nuclear Proliferation"))


terms$col.speaker = cmp$col[match(terms$term, cmp$term)]
with(head(terms, 100), plotWords(x=prop, wordfreq = freq, words = term, col=col.speaker, xaxt="none", random.y = T, main="Topic words for War on Terror and Nuclear Proliferation", sub="Color indicates speaker (red=Bush, blue=Obama)"))
axis(1, at=c(0, 0.5, 1), labels = c("War on Terror", "", "Nuclear Proliferation"))

# now with three topics?

compare.topics3 <- function(m, cmp_topics) {
  assignments = dtm.to.df(m@wordassignments, term_labels=m@terms, doc_labels=m@documents)
  colnames(assignments)[3] = "topic"
  terms = dcast(assignments, term ~ topic, value.var = "doc", fun.aggregate = length)
  
  terms = terms[,c(1, cmp_topics+1)] # 'term is 1
  colnames(terms)[-1] = c("x", "y", "z")
  terms$freq = rowSums(terms[-1])
  terms = terms[terms$freq>0, ]
  terms$prop.x = terms$x / terms$freq
  terms$prop.y = terms$y / terms$freq
  terms$prop.z = terms$z / terms$freq
  tern = ggtern::tlr2xy(terms[c("x", "y", "z")], ggtern::coord_tern())
  terms$tx = tern$x
  terms$ty = tern$y
  terms$col = rgb(terms$prop.x, terms$prop.y, terms$prop.z)
  terms$nn = rowSums(terms[c("x","y","z")] > 0)
  arrange(terms, -nn, -freq)
}

terms = compare.topics3(m, match(c("tax", "budget", "health"), topics))

with(head(terms, 50), plotWords(tx, ty, term, freq, col=col, xaxt="none", main=c("Topic words for Health, Tax, and Budget"),  sub="Colour indicates topic"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)

terms$col.speaker = cmp$col[match(terms$term, cmp$term)]

with(head(terms, 50), plotWords(tx, ty, term, freq, col=col.speaker, xaxt="none", main="Topic words for Health, Tax, and Budget", sub="Colour indicates speaker, red=Bush, blue=Obama"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)



terms = compare.topics3(m2, match(c("terrorism", "war", "freedom"), topics2))

with(head(terms, 75), plotWords(tx, ty, term, freq, col=col, xaxt="none", main=c("Topic words for Health, Tax, and Budget"),  sub="Colour indicates topic"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)

terms$col.speaker = cmp$col[match(terms$term, cmp$term)]

with(head(terms, 50), plotWords(tx, ty, term, freq, col=col.speaker, xaxt="none", main="Topic words for Health, Tax, and Budget", sub="Colour indicates speaker, red=Bush, blue=Obama"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)


