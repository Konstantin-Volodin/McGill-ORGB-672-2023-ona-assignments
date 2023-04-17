library(here)
library(arrow)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)
library(plotly)

### LOAD DATA
data_path <- here('assignments','final_project',"672_project_data")
results <- read_parquet(here(data_path, 'results.parquet'))
applications <- read_parquet(here(data_path, 'clean_applications.parquet'))
edges <- read_parquet(here(data_path, 'clean_edges.parquet'))
examiner_data <- read_parquet(here(data_path, 'clean_examiner.parquet')) %>% rename(name = examiner_id)
examiner_wg_data <- read_parquet(here(data_path, 'clean_examiner_wg.parquet')) %>% rename(name = examiner_id)


### GET SOME WORKGROUPS ONLY
wgs <- c(216)
exm_wgs <- examiner_wg_data %>% 
  filter(examiner_workgroup %in% wgs) %>%
  distinct(name) %>% pull(name)
examiner_data <- read_parquet(here(data_path, 'clean_examiner.parquet')) %>% rename(name = examiner_id)

### CREATE NETWORK
edge_subset <- edges %>% 
  select(to = ego_examiner_id, from = alter_examiner_id) %>% 
  filter(to %in% exm_wgs & from %in% exm_wgs) %>%
  mutate(to = as.character(to), from = as.character(from)) %>%
  drop_na()
node_subset <- edge_subset %>% 
  pivot_longer(cols=c('from','to')) %>% 
  distinct(examiner_id = value) %>%
  rename(name = examiner_id) %>%
  mutate(name = as.character(name))
network <- tbl_graph(node_subset, edge_subset, directed=FALSE)
network <- network %>% left_join(examiner_data, on='name')
network <- network %>% 
  mutate(examiner_race = case_when(examiner_race == 'white' ~ 'white',
                                   is.na(examiner_race) ~ examiner_race,
                                   TRUE ~ 'non-white'))

### GENDER
p1 <- network %>%
  ggraph(layout="mds") +
  geom_edge_link(edge_colour = "#d3d3d3", alpha=0.3) +
  geom_node_point(aes(color=examiner_gender)) +
  theme_void()
p2 <- network %>%
  ggraph(layout="mds") +
  geom_edge_link(edge_colour = "#d3d3d3", alpha=0.3) +
  geom_node_point(aes(color=examiner_race)) +
  theme_void()
grid.arrange(p1,p2)


### VISUALIZE
summary <- results %>% 
  group_by(timestep, desc, thresh) %>%
  summarize(adoption = mean(value)) %>%
  ungroup() %>%
  mutate(thresh = as.factor(thresh))
 

(ggplot(summary %>% filter(desc == 'random_start' & timestep <= 25), 
       aes(x=timestep, y=adoption, color=thresh)) +
  geom_line(size=0.2) +
  theme_minimal()) %>% ggplotly()


(ggplot(summary %>% filter(thresh == 0.25  & timestep <= 25), 
       aes(x=timestep, y=adoption, color=desc)) +
  geom_line(size=0.2) +
  theme_minimal())  %>% ggplotly()
