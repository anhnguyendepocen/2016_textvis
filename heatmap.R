library(corpustools)
load("data.rda")

# which topics have similar betas?
x = t(m@beta)
colnames(x) = topics
cm = cor(x)
diag(cm) = 0
heatmap(cm, symm = T)
