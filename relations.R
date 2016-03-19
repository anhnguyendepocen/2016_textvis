library(corpustools)
data(sotu)
load("model.rda")
# loads a 15-k lda "m" based on "dtm" with topic labels "topics" 

# which topics have similar betas?
x = t(m@beta)
colnames(x) = topics
rownames(x) = m@terms

# this makes sense:
cm = cor(x)
for(i in 1:15) cm[i,i]=0
heatmap(cm, symm = T)

# now let's try via wordassignments (inspired by documentsums):
assignments = data.frame(doc = m@wordassignments$i, term = m@wordassignments$j, topic = m@wordassignments$v)
tpw = acast(assignments, term ~ topic, value.var = "doc", fun.aggregate = length)
colnames(tpw) = topics
rownames(tpw) = m@terms
head(tpw)

# thsi makes no sense, strongest link by far is nuclear - education:
cm = cor(tpw>0)
for(i in 1:15) cm[i,i]=0
heatmap(cm, symm = T)

# but this does makes sense: (so it's probably a weighting issue of sorts)
cm = cor(tpw>0)
for(i in 1:15) cm[i,i]=0
heatmap(cm, symm = T)

# does it help to weigh by original dtm?
x = dtm[m@documents, m@terms]
x = data.frame(doc = dtm$i, term = dtm$j, count = dtm$v)
assignments2 = merge(assignments, x, all.x = T)

tpw2 = acast(assignments2, term ~ topic, value.var = "count", fun.aggregate = sum)
colnames(tpw2) = topics
rownames(tpw2) = m@terms
head(tpw2)
head(tpw)

# makes no difference:
cm = cor(tpw2)
for(i in 1:15) cm[i,i]=0
heatmap(cm, symm = T)


heatmap(cm, symm=T)

library(igraph)
g = igraph::graph_from_adjacency_matrix(cm, weighted = T, mode = "undirected")
g = delete_edges(g, which(E(g)$weight<0))

g = delete.vertices(g, degree(g) == 0)

E(g)$width = E(g)$weight * 100
plot(g)

plot(delete.isolates(g))
edges(g)
as.data.frame(g, "edges")
igraph::as_data_frame(g, "edges")

