#### Preamble ####
# Purpose: POL304 Week 10 Network Analysis
# Author: Talia Fabregas
# Date: March 18 2024
# Contact: talia.fabregas@mail.utoronto.ca

#### Workplace Setup ####
# install.packages("sna")
# install.packages("igraph")
library(sna)
library(igraph)

#### Network Analysis Examples ####

# load the coleman data
# display rows 1 to 10, columns 1 to 10
# 10x10 adjacency matrix for friendships in the coleman data
# friendship network
data(coleman)
coleman[1,1:10,1:10]

# make a network graph
# convert adjacency matrix into igraph object
ggraph<-graph_from_adjacency_matrix(coleman[1,,], mode="undirected", diag=FALSE)
# plot it
plot(ggraph, vertex.color="black", vertex.size=5, vertex.label=NA)

# calculate degree centrality using the degree function from the igraph package
degree.cent <- igraph::degree(ggraph)
# this will show the degree centrality of every node in the network
degree.cent
# calculate closeness centrality
closeness_centrality <- igraph::closeness(ggraph)
closeness_centrality

## visualize information speed
V(ggraph)$color <- ifelse(V(ggraph)$name == 21, "red", "grey")
plot(ggraph)

## shortest path of 1
neighbors(ggraph,21)
V(ggraph)$color <- ifelse(V(ggraph)$name %in% neighbors(ggraph,21), "pink", "grey")
V(ggraph)$color[V(ggraph)$name == 21]<- "red"
plot(ggraph)

## shortest path of 2
# convert igraph object back into a matrix
mynet<-as.matrix(igraph::as_adj(ggraph))
# use this to square a matrix (do not try to square an igraph object)
mynet2<-mynet%*%mynet
diag(mynet2)<-0 #a node cannot be connected to itself
neighbors2<-names(mynet2[21,][mynet2[21,]>0])
neighbors2

V(ggraph)$color <- ifelse(V(ggraph)$name %in% neighbors2, "mistyrose", "grey")
V(ggraph)$color[V(ggraph)$name %in% neighbors(ggraph,21)]<- "pink"
V(ggraph)$color[V(ggraph)$name == 21]<- "red"
plot(ggraph, vertex.label=NA)

# Example (slide 35)
el <- matrix( c("1", "2", "1", "3","2","3","3","5","2","4","2","5","3","4","4","5","4","7","6","5","5","7","6","7","6","8", "8","9","9","10", "8","12", "11","12",  "12","13","12","14","12","15","12","16"), nc = 2, byrow = TRUE)
g<-graph_from_edgelist(el, directed=F)
# plot the new graph
plot(g, vertex.color="black", vertex.size=5, vertex.label=NA)

# model the spread of information using 12 as the seed
V(g)$color <- ifelse(V(g)$name %in% neighbors(g,12), "pink", "grey")
V(g)$color[V(g)$name == 12]<- "red"
plot(g)

# model the spread using 6 as the seed
V(g)$color <- ifelse(V(g)$name %in% neighbors(g,6), "pink", "grey")
V(g)$color[V(g)$name == 6]<- "red"
plot(g)

# model the spread using 8 as the seed
V(g)$color <- ifelse(V(g)$name %in% neighbors(g,8), "pink", "grey")
V(g)$color[V(g)$name == 8]<- "red"
plot(g)



