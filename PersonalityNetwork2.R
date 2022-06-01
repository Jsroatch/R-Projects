library(car)
library(multcomp)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(caret)
library(stargazer)
library(psych)
library(tm)
library(topicmodels)
library(reshape2)
library(ggplot2)
library(ggthemes)
library(ggthemes)
library(wordcloud)
library(pals)
library(SnowballC)
library(tidytext)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(lavaan)
library(semPlot)
library(semTools)
library(rsm)
library(Hmisc)
library(psychonetrics)
library(qgraph)
library(sna)
library(igraph)
library(ggcorrplot)
library(qgraph)
library(stringr)
library(bootnet)

options(stringsAsFactors = F)         # prevent automatic data transformation
setwd("C:/Users/19134/Documents/R")

df <- read.csv("Generalized.csv")
key <- read.csv("PersonalityNetworkItems.csv")

big5 <- df %>% 
  select(-ResponseId)

node <- key 

edgecor <- cor(big5, use = "complete.obs")
edgelasso <- EBICglasso(edgecor, n = nrow(big5))
edgelist <- melt(edgelasso)
head(edgelist)

edgelist <- edgelist %>%
  select(From = Var1, To = Var2, weight = value) %>%
  filter(weight != 0) %>%
  filter(From != To)

edgelist$sign <- ifelse(edgelist$weight >= 0, "Positive", "Negative")
edgelist$weight <- abs(edgelist$weight)

edgelist <- edgelist %>%
  group_by(From, To) %>%
  mutate(edge_id = paste(sort(unique(c(From, To))), collapse=" ")) 

edgelist<- edgelist[!duplicated(edgelist$edge_id),]
head(edgelist)

net <- graph_from_data_frame(d = edgelist, directed = F, vertices = node)
net <- simplify(net)

V(net)$degree <- igraph::degree(net, mode = "total")
V(net)$clustering <- igraph::transitivity(net, type = "local")
V(net)$betweenness <- igraph::betweenness(net)
V(net)$closeness <- igraph::closeness(net)

clusters <- igraph::cluster_optimal(net)
clusters$vcount

vstats <- igraph::get.data.frame(net, what = "vertices")
vstats <- cbind(Item = rownames(vstats), vstats)

vscale <- vstats %>% 
  select_if(is.numeric)
vscale <- scale(vscale)

corr <- round(cor(vscale),2)
corr

ggcorrplot(corr, 
           outline.col = "white", 
           ggtheme =  ggplot2::theme_classic,
           lab = T,
           colors = c("#6D9EC1", "white", "#E46726"))

####### PLOTS ######

#color based on factors
V(net)$color[V(net)$Factor == "Conscientiousness"] <- "#66c2a5"
V(net)$color[V(net)$Factor == "Extraversion"] <- "#fc8d62"
V(net)$color[V(net)$Factor == "Neuroticism"] <- "#8da0cb"
V(net)$color[V(net)$Factor == "Agreeableness"] <- "#a6cee3"
V(net)$color[V(net)$Factor == "Openness"] <- "#a6d854"

E(net)$color <- edgelist$sign
E(net)$color <- ifelse(E(net)$color == "Positive", "green4", "red")

par(mar=c(2.1,2.1,2.1,8.1), xpd = T)

set.seed(100)

plot(net,
     layout = layout.fruchterman.reingold,
     vertex.color =  V(net)$color,
     vertex.size =  V(net)$degree^1.3,
     vertex.label = V(net)$Item,
     edge.color = E(net)$color,
     edge.curved = .5,
     edge.width = E(net)$value,
     edge.arrow.size = .08,
     edge.arrow.width = .25
)

legend("topright", 
       bty = "n",
       inset = c(-.3,0),
       legend = c("Conscientiousness", "Extraversion", "Neuroticism", "Agreeableness", "Openness"),
       fill = c("#66c2a5", "#fc8d62", "#8da0cb", "#a6cee3","#a6d854" ), 
       border = NA)