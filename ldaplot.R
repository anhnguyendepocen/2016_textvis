library(corpustools)
data(sotu)
load("model.rda")

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

intersect(names(a_terms), names(b_terms))

"terror" %in% names(b_terms)
x = terms(m, 10)
colnames(x) = topics
x
topics
