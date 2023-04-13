library(here)
library(arrow)
library(gender)
library(wru)
library(lubridate)

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)


### LOAD DATA
data_path <- here('assignments','final_project',"672_project_data")
applications <- read_parquet(here(data_path, "app_data_sample.parquet"))
edges <- read_csv(here(data_path, "edges_sample.csv"))


### GENDER
examiner_names <- applications %>% distinct(examiner_name_first)
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select( examiner_name_first = name, gender)
applications <- applications %>% left_join(examiner_names_gender, by = "examiner_name_first")


### RACE
examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct(surname)
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble() %>%
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian", max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic", max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white", TRUE ~ NA_character_
  )) %>% 
  select(surname,race)
applications <- applications %>% left_join(examiner_race, by = c("examiner_name_last" = "surname"))


### TENURE
examiner_dates <- applications %>% select(examiner_id, filing_date, appl_status_date) 
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>%
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
  ) %>% 
  filter(year(latest_date)<2018) %>%
  mutate(tenure_years = tenure_days / 365) %>%
  mutate(tenure = case_when(
    tenure_years <= 1 ~ '<1',
    tenure_years <= 2 ~ '1-2',
    tenure_years <= 5 ~ '3-5',
    tenure_years <= 9 ~ '6-9',
    tenure_years <= 14 ~ '10-14',
    tenure_years <= 100 ~ '15+',
    TRUE ~ NA_character_
  ))
applications <- applications %>% left_join(examiner_dates, by = "examiner_id")

### SAVE DATA
applications %>% write_parquet(here(data_path, 'clean_applications.parquet'))
edges %>% write_parquet(here(data_path, 'clean_edges.parquet'))





### READ DATA
applications <- read_parquet(here(data_path, 'clean_applications.parquet'))
edges <- read_parquet(here(data_path, 'clean_edges.parquet'))

### WORKGROUPS
applications <- applications %>% 
  mutate(examiner_workgroup = str_sub(examiner_art_unit, 1, -2))


### EXAMINER DATA
examiner_data <- applications %>%
  distinct(examiner_id, examiner_gender = gender, 
           examiner_race = race, examiner_tenure = tenure) 




### CREATE NETWORK
edge_subset <- edges %>% 
  select(to = ego_examiner_id, from = alter_examiner_id) %>%
  drop_na()
node_subset <- edge_subset %>% 
  pivot_longer(cols=c('from','to')) %>% 
  distinct(examiner_id = value) %>%
  left_join(examiner_data, on='examiner_id') %>%
  distinct(examiner_id, examiner_gender, examiner_race, examiner_tenure) %>%
  rename(name = examiner_id) %>%
  mutate(name = as.character(name))
network <- graph_from_data_frame(edge_subset, directed = TRUE) %>%
  as_tbl_graph() %>%
  left_join(node_subset, by='name')


### VISUALIZE
# network %>%
#   ggraph(layout="mds") +
#   geom_edge_link(edge_colour = "#d3d3d3", alpha=0.1) +
#   geom_node_point() +
#   theme_void()


### ESTIMATE CENTRALITY SCORES




### CREATE IDEA RANDOM DATA
threshold <- 0.2
network <- network %>%
  mutate(idea_support = rbinom(dim(as.tibble(network))[1], 1, 0.1))


### 
init_net <- network %>% 
  activate(edges) %>%
  as_tibble

### https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html 
### interesting topic


### ESTIMATE STATS
network %>% 
  as_tibble() %>% 
  count(examiner_gender, idea_support)



