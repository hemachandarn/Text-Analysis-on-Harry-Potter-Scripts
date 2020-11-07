library(dplyr)
library(igraph) 
scripts <- read.csv(file.choose())
scripts <- scripts %>% select(Speaker,Receiver)
scripts <- scripts %>% filter(Receiver != "")
length(unique(scripts$Speaker))
unique(scripts$Speaker)
length(unique(scripts$Receiver))
unique(scripts$Receiver)
scripts <- scripts %>% select(Speaker,Receiver)

conversations <- scripts %>% group_by(Speaker,Receiver) %>% summarise(counts = n())

set.seed(42) 
conversations <- conversations[sample(nrow(conversations),192), ]

nodes <- c(as.character(conversations$Speaker), as.character(conversations$Receiver))
nodes <- unique(nodes)

my_graph <- graph_from_data_frame(d=conversations, vertices=nodes, directed=FALSE)
my_graph 

V(my_graph)$name

E(my_graph)
plot(my_graph, vertex.label.color = "black")

plot(my_graph, vertex.label.color = "black", layout = layout_in_circle(my_graph))
plot(my_graph, vertex.label.color = "black", layout = layout_with_fr(my_graph))
plot(my_graph, vertex.label.color = "black", layout = layout_as_tree(my_graph))
w1 <- E(my_graph)$counts

plot(my_graph, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(w1),  # put w1 in sqrt() so that the lines don't become too wide
     layout = layout_nicely(my_graph))

my_graph_2more_conv <- delete_edges(my_graph, E(my_graph)[counts < 2])
plot(my_graph_2more_conv, 
     vertex.label.color = "black", 
     edge.color = 'black',
     edge.width = sqrt(E(my_graph_2more_conv)$counts),
     layout = layout_nicely(my_graph_2more_conv))


g <- graph_from_data_frame(conversations, directed = TRUE)
g

is.directed(g)

plot(g, 
     vertex.label.color = "black", 
     edge.color = 'orange',
     vertex.size = 0,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))

neighbors(g, 'Harry', mode = c('all'))


neighbors(g, 'Harry', mode = c('in'))
neighbors(g, 'Harry', mode = c('out'))

n1 <- neighbors(g, 'Harry', mode = c('out'))
n2 <- neighbors(g, 'Hagrid', mode = c('in'))
intersection(n1, n2)

farthest_vertices(g) 
get_diameter(g)  

ego(g, 2, 'Snape', mode = c('out'))
ego(g, 2, 'Snape', mode = c('in'))

g.outd <- degree(g, mode = c("out"))
g.outd
which.max(g.outd)

g.b <- betweenness(g, directed = TRUE)
g.b

plot(g, 
     vertex.label.color = 'black',
     edge.color = 'black',
     vertex.size = sqrt(g.b) / 1.2,
     edge.arrow.size = 0.03,
     layout = layout_nicely(g))

g184 <- make_ego_graph(g, 2, nodes = 'Hagrid', mode = c("all"))[[1]]
g184

dists <- distances(g184, "Hagrid")

colors <- c("black", "blue", "orange", "red", "green")

V(g184)$color <- colors[dists+1]
plot(g184, 
     vertex.label = dists, 
     vertex.label.color = "white",
     vertex.label.cex = .6,
     edge.color = 'black',
     vertex.size = 7,
     edge.arrow.size = .05)