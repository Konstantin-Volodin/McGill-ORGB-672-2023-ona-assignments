library(here)
library(arrow)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)
library(cowplot)
library(plotly)



### PREPARE DATA
##### LOAD
data_path <- here('assignments', 'final_project', '672_project_data')
results <- read_parquet(here(data_path, 'results.parquet'))
applications <- read_parquet(here(data_path, 'clean_applications.parquet'))
edges <- read_parquet(here(data_path, 'clean_edges.parquet'))
examiner_data <- read_parquet(here(data_path, 'clean_examiner.parquet')) %>% rename(name = examiner_id)
examiner_wg_data <- read_parquet(here(data_path, 'clean_examiner_wg.parquet')) %>% rename(name = examiner_id)

##### GET SOME WORKGROUPS ONLY
wgs <- c(216)
exm_wgs <- examiner_wg_data %>% filter(examiner_workgroup %in% wgs) %>% distinct(name) %>% pull(name)

##### NETWORK
edge_subset <- edges %>% select(to = ego_examiner_id, from = alter_examiner_id) %>%
  filter(to %in% exm_wgs & from %in% exm_wgs) %>% 
  mutate(to = as.character(to), from = as.character(from)) %>% drop_na()
node_subset <- edge_subset %>% pivot_longer(cols=c('from','to')) %>% distinct(examiner_id = value) %>% 
  rename(name = examiner_id) %>% mutate(name = as.character(name))
network <- tbl_graph(node_subset, edge_subset, directed=FALSE)
network <- network %>% left_join(examiner_data, on='name')
network <- network %>% mutate(examiner_race = case_when(examiner_race == 'white' ~ 'white', is.na(examiner_race) ~ examiner_race, TRUE ~ 'non-white'))

##### RESULTS
summary <- results %>%
  group_by(timestep, desc, thresh) %>%
  summarize(adoption = mean(value)) %>%
  ungroup()

##### DETAILED EVOLUTION
net_rnd_10 <- results %>% filter(desc == 'random_start' & thresh == 0.1 & repl == 1)
net_rnd_25 <- results %>% filter(desc == 'random_start' & thresh == '0.3' & repl == 1)
net_rnd_50 <- results %>% filter(desc == 'random_start' & thresh == 0.6 & repl == 1)
net_male_25 <- results %>% filter(desc == 'male_start' & thresh == '0.3' & repl == 1)
net_female_25 <- results %>% filter(desc == 'female_start' & thresh == '0.3' & repl == 1)
net_white_25 <- results %>% filter(desc == 'white_start' & thresh == '0.3' & repl == 1)
net_nonwhite_25 <- results %>% filter(desc == 'non_white_start' & thresh == '0.3' & repl == 1)
net_deg_25 <- results %>% filter(desc == 'top_10_degree_start' & thresh == '0.3' & repl == 1)
net_betw_25 <- results %>% filter(desc == 'top_10_betw_start' & thresh == '0.3' & repl == 1)

### VISUALIZATIONS
##### GENDER
set.seed(1)
p1a <- network %>% ggraph(layout="mds") +
  geom_edge_link(edge_colour = "#d3d3d3", alpha=0.3) +
  geom_node_point(aes(color=examiner_gender)) +
  theme_void()
set.seed(1)
p1b <- network %>% ggraph(layout="mds") +
  geom_edge_link(edge_colour = "#d3d3d3", alpha=0.3) +
  geom_node_point(aes(color=examiner_race)) +
  theme_void()
grid.arrange(p1a,p1b)

##### THRESHOLD VISUALIZATIONS
thresh_vis <- function(desc_filter, description, timestep_fl) {
  p <- ggplot(summary %>% filter(desc ==  desc_filter & timestep <= timestep_fl), 
              aes(x=timestep, y=adoption, color=thresh, group=thresh)) +
    geom_line() +
    labs(title = description, x = "Timestep", y = "Spread") +
    scale_color_gradient(low = "green", high = "red") +
    theme_minimal() +
    theme(text = element_text(size = 10))
}
p21a <- thresh_vis('random_start', 'No Bias Random Start', 20)
p31a <- thresh_vis('male_start', 'Male Random Start', 20)
p31b <- thresh_vis('female_start', 'Female Random Start', 20)
p32a <- thresh_vis('white_start', 'White Random Start', 20)
p32b <- thresh_vis('non_white_start', 'Non White Random Start', 20)
p33a <- thresh_vis('top_10_degree_start', 'Top 10 Degree Centrality', 20)
p33b <- thresh_vis('bot_10_degree_start', 'Bottom 10 Degree Centrality', 20)
p34a <- thresh_vis('top_10_betw_start', 'Top 10 Betweenness Centrality', 20)
p34b <- thresh_vis('bot_10_betw_start', 'Bottom 10 Betweenness Centrality', 20)

##### NETWORK EVOLUTION VISUALIZATION
evol_vis <- function(data, description, timesteps) {
  title <- ggdraw() + draw_label(description, x = 0, hjust = 0) + 
    theme(text = element_text(size = 6), plot.margin = margin(0, 0, 0, 7))
  p <- list()
  for (ii in 0:timesteps) {
    set.seed(1)
    nt <- data %>% filter(timestep == ii) %>% select(name, idea=value)
    nt <- network %>% left_join(nt, on='name')
    p[[ii+1]] <- nt %>% ggraph(layout="mds") +
      geom_edge_link(edge_colour = "#d3d3d3", alpha=0.3) +
      geom_node_point(aes(color=idea)) +
      ggtitle(paste0("Timestep: ", ii)) +
      theme_void() +
      theme(text = element_text(size = 8), legend.position = "none")
  }
  p <- plot_grid(plotlist=p)
  return(plot_grid(title, p, ncol=1, rel_heights = c(0.05, 1)))
}
p21b <- evol_vis(net_rnd_10, 'Random Start - 10% Threshold', 10)
p21c <- evol_vis(net_rnd_25, 'Random Start - 30% Threshold', 10)
p21d <- evol_vis(net_rnd_50, 'Random Start - 60% Threshold', 10)
p31c <- evol_vis(net_male_25, 'Male Start - 30% Threshold', 10)
p31d <- evol_vis(net_female_25, 'Female Start - 30% Threshold', 10)
p32c <- evol_vis(net_white_25, 'White Start - 30% Threshold', 10)
p32d <- evol_vis(net_nonwhite_25, 'Non White Start - 30% Threshold', 10)
p33c <- evol_vis(net_deg_25, 'Top 10 Degree Centrality - 30% Threshold', 10)
p34c <- evol_vis(net_betw_25, 'Top 10 Betweenness Centrality - 30% Threshold', 10)