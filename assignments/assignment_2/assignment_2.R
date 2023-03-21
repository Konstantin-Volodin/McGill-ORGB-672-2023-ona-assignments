  library(tidyverse)
  library(igraph)
  library(tidygraph)
  library(ggraph)
  
  
  ### CREATE DATA
  nodes <- tibble(name = c('1', '2', '3', '4', '5', '6','A', 'B', 'C', 'D'))
  edges <- tibble(
    from = c('6','6','6', '5', '5', '4', '4', '3', '2', '2', 'D', 'D', 'B', 'B', 'C'),
    to = c('5','D','B', 'D', '3', '3', 'C', 'D', '1', 'A', 'B', 'C', 'C', 'A', 'A')
  )
  
  
  ### CREATE NETWORK
  network <- tbl_graph(nodes = nodes, edges = edges, directed=FALSE)
  
  
  ### ESTIMATE METRICS
  network <- network %>% 
    mutate(degree = centrality_degree(),
           closeness = centrality_closeness_harmonic(),
           betweenness = centrality_betweenness()) %>%
    mutate(avg = (degree + closeness + betweenness)/3) %>%
    mutate(label = paste0(name, '\n',
                          'Degree: ',round(degree,2), '\n',
                          'Closeness: ',round(closeness,2), '\n',
                          'Betweenness: ',round(betweenness,2), '\n',
                          'Avg: ',round(avg,2)))
  node_data <- network %>% data.frame() %>% tibble()
  
  
  ### PLOT NETWORK
  network %>%
    ggraph(layout="graphopt") +
    geom_edge_link(edge_colour = "grey", alpha=0.5) +
    geom_node_point(aes(size=avg)) +
    geom_node_text(aes(label = label), repel=TRUE) +
    theme_graph(foreground=NA)
