library(semnet)
data(sotu)
load("data.rda")

sotu.tokens = sotu.tokens[sotu.tokens$pos1 %in% c("N", "M", "A"), ]

g.all = windowedCoOccurenceNetwork(location = sotu.tokens$id, term = sotu.tokens$lemma, 
                               context = sotu.tokens$aid, window.size = 20)

g.bush = with(sotu.tokens[sotu.tokens$aid %in% sotu.meta$id[sotu.meta$headline == "George W. Bush"], ],
              windowedCoOccurenceNetwork(location = id, term = lemma, context = aid, window.size = 20))
g.obama = with(sotu.tokens[sotu.tokens$aid %in% sotu.meta$id[sotu.meta$headline == "Barack Obama"], ],
              windowedCoOccurenceNetwork(location = id, term = lemma, context = aid, window.size = 20))


d.obama = get.data.frame(g.obama, what=c("edges"))
d.bush = get.data.frame(g.bush, what=c("edges"))


d =merge(d.obama, d.bush, by=c("from", "to"), all.x=T)
d[is.na(d)] = 0
d = d[d$weight.x > d$weight.y, ]
colnames(d)[3] = "weight"
g.obama2 = graph.data.frame(d, vertices = get.data.frame(g.obama, "vertices"))

d =merge(d.bush, d.obama, by=c("from", "to"), all.x=T)
d[is.na(d)] = 0
d = d[d$weight.x > d$weight.y, ]
colnames(d)[3] = "weight"
g.bush2 = graph.data.frame(d, vertices = get.data.frame(g.bush, "vertices"))



par(mfrow=c(1,2))
g = g.bush2
g_backbone = getBackboneNetwork(g, alpha = 1e-03, max.vertices = 75)
g_backbone = decompose.graph(g_backbone, min.vertices = 5, max.comps = 1)[[1]]
V(g_backbone)$cluster = edge.betweenness.community(g_backbone)$membership
g_backbone = setNetworkAttributes(g_backbone, size_attribute = V(g_backbone)$freq, 
                                  cluster_attribute = V(g_backbone)$cluster)

plot(g_backbone, main="Bush (color=clusters)")
V(g_backbone)$color  = cmp$col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Bush (color=speaker)")
V(g_backbone)$color  = cmp$topic.col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Bush (color=speaker)")
V(g_backbone)$color  = cmp$topic2.col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Bush (color=speaker)")

g = g.obama2
g_backbone = getBackboneNetwork(g, alpha = 1e-03, max.vertices = 75)
g_backbone = decompose.graph(g_backbone, min.vertices = 5, max.comps = 1)[[1]]
V(g_backbone)$cluster = edge.betweenness.community(g_backbone)$membership
g_backbone = setNetworkAttributes(g_backbone, size_attribute = V(g_backbone)$freq, 
                                  cluster_attribute = V(g_backbone)$cluster)
plot(g_backbone, main="Obama (color=clusters)")
V(g_backbone)$color  = cmp$col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Obama (color=speaker)")
V(g_backbone)$color  = cmp$topic.col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Obama (color=topic)")
V(g_backbone)$color  = cmp$topic2.col[match(V(g_backbone)$name, cmp$term)]
V(g_backbone)$frame.color = V(g_backbone)$color 
plot(g_backbone, main="Obama (color=topic2)")



V(g)$name
E(g)[1]$x
class(E(g))

E(g)$color

g = g.mixed
g_backbone = getBackboneNetwork(g, alpha = 1e-03, max.vertices = 75)
g_backbone = decompose.graph(g_backbone, min.vertices = 5, max.comps = 1)[[1]]
V(g_backbone)$cluster = edge.betweenness.community(g_backbone)$membership
g_backbone = setNetworkAttributes(g_backbone, size_attribute = V(g_backbone)$freq, 
                                  cluster_attribute = V(g_backbone)$cluster)
plot(g_backbone, main="Mixed")
