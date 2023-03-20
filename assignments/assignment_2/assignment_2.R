library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(visNetwork)


### CREATE DATA
nodes <- tibble(id = c('1', '2', '3', '4', '5', '6','A', 'B', 'C', 'D')) %>% mutate(label = id)
edges <- tibble(
  from = c('6','6','6', '5', '5', '4', '4', '3', '2', '2', 'D', 'D', 'B', 'B', 'C'),
  to = c('5','D','B', 'D', '3', '3', 'C', 'D', '1', 'A', 'B', 'C', 'C', 'A', 'A',)
)


### CREATE NETWORK
network <- tbl_graph(nodes = nodes, edges = edges, directed=FALSE)


### ESTIMATE METRICS
network <- network %>% 
  mutate(degree = centrality_degree(),
         closeness = centrality_closeness_harmonic(),
         betweenness = centrality_betweenness())
node_data <- network %>% data.frame() %>% tibble()


### PLOT NETWORK
visNetwork(nodes=nodes, edges=edges) %>% 
  visOptions(selectedBy = "id") %>%
  visIgraphLayout()

