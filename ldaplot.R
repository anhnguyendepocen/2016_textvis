library(corpustools)
data(sotu)
load("model.rda")
source("functions.R")
# let's plot words between nuclear and terror
x = t(m@beta)
colnames(x) = topics
rownames(x) = m@terms

assignments = data.frame(doc = m@wordassignments$i, term = m@wordassignments$j, topic = m@wordassignments$v)
tpw = acast(assignments, term ~ topic, value.var = "doc", fun.aggregate = length)
colnames(tpw) = topics
rownames(tpw) = m@terms
head(tpw)

a_terms = tpw[,"nuclear"]
a_terms = a_terms[a_terms>0]

b_terms = tpw[,"terror"]
b_terms = b_terms[b_terms>0]
terms[terms$prop > 0 & terms$prop < 1,]
terms = merge(a_terms, b_terms, by="row.names", all=T)
colnames(terms)[1] = "term"
terms[is.na(terms)] = 0
terms$freq = terms$x + terms$y
terms$prop = terms$x / terms$freq
h = rescale(terms$prop, c(1, .6666))
terms$col = hsv(h, .5, .5)

terms$stratum = "mixed"
terms$stratum[terms$x == 0] = "terror"
terms$stratum[terms$y == 0] = "nuclear"
load("word_contrast.rda")
terms$col.speaker = cmp$col[match(terms$term, cmp$term)]

terms = arrange(terms, -freq)

sub = rbind(terms[terms$stratum == "mixed",], head(terms[terms$stratum == "terror",], 15), head(terms[terms$stratum == "nuclear",], 15))

# color = topic
with(sub, plotWords2(x=prop, wordfreq = freq, words = term, col=col, xaxt="none", random.y = T, main="Topic words for War on Terror and Nuclear Proliferation", sub="Color and position indicate topic"))
axis(1, at=c(0, 0.5, 1), labels = c("War on Terror", "", "Nuclear Proliferation"))

# color = speaker
with(sub, plotWords2(x=prop, wordfreq = freq, words = term, col=col.speaker, xaxt="none", random.y = T, main="Topic words for War on Terror and Nuclear Proliferation", sub="Color indicates speaker (red=Bush, blue=Obama)"))
axis(1, at=c(0, 0.5, 1), labels = c("War on Terror", "", "Nuclear Proliferation"))

# now with three topics?

terms = c("tax", "budget", "health")

a_terms = tpw[,terms[1]]
a_terms = a_terms[a_terms>0]

b_terms = tpw[,terms[2]]
b_terms = b_terms[b_terms>0]

c_terms = tpw[,terms[3]]
c_terms = c_terms[c_terms>0]

terms = merge(a_terms, b_terms, by="row.names", all=T)
colnames(terms)[1] = "term"
terms = merge(terms, cbind(z=c_terms), by.x="term", by.y="row.names", all=T)
terms[is.na(terms)] = 0
terms$freq = terms$x + terms$y + terms$z

terms$prop.x = terms$x / terms$freq
terms$prop.y = terms$y / terms$freq
terms$prop.z = terms$z / terms$freq

tern = ggtern::tlr2xy(terms[c("x", "y", "z")], ggtern::coord_tern())
terms$tx = tern$x
terms$ty = tern$y

terms$col = rgb(terms$prop.x, terms$prop.y, terms$prop.z)

terms$nn = (terms$x > 0) + (terms$y > 0) + (terms$z > 0)

load("word_contrast.rda")
terms$col.speaker = cmp$col[match(terms$term, cmp$term)]

terms = arrange(terms, -nn, -freq)

with(head(terms, 50), plotWords2(tx, ty, term, freq, col=col, xaxt="none", main=c("Topic words for Health, Tax, and Budget"),  sub="Colour indicates topic"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)


with(head(terms, 50), plotWords2(tx, ty, term, freq, col=col.speaker, xaxt="none", main="Topic words for Health, Tax, and Budget", sub="Colour indicates speaker, red=Bush, blue=Obama"))
axis(1, at=c(0, 0.5, 1), labels = c("Tax", "", "Health"), line = 1)
axis(2, at = c(0,0.5, 1), labels = c("","", "Budget"), line = 1)

head(terms)

