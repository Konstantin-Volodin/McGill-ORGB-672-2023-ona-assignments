library(tidyverse)
library(here)
library(readr)
library(stringr)

library(igraph)
library(network)
library(tidygraph)
library(ggraph)


### READ DATA
connections <- read_csv(here("assignments","assignment_1","connections.csv"), skip = 2, show_col_types=FALSE)
connections <- connections %>% 
  rename("first_name"= 1, "last_name" = 2, "email" = 3, 
         "company" = 4, "position" = 5, "connected_date" = 6)


### CLEAN DATA
data <- connections %>% 
  mutate(name = str_c(last_name, '.', substring(first_name,1,1))) %>%
  select(name, company) %>%
  mutate(company = case_when(
    company == '"\"Uchebny centr EKSKURS\" LLC"' ~ 'EKSKURS',
    company == 'AedoAI Inc. (Aedo)' ~ 'Aedo',
    company == 'Allianz Trade in North America' ~ 'Allianz Trade',
    company == 'Amazon Web Services (AWS)' ~ 'Amazon',
    company == 'Canadian Armed Forces | Forces armées canadiennes' ~ 'Canadian Armed Forces',
    company == 'Crown-Indigenous Relations and Northern Affairs Canada' ~ 'CIRNAC',
    company == 'Emploi et Développement social Canada (EDSC) / Employment and Social Development Canada (ESDC)' ~ 'EDSC',
    company == 'Immigration, Refugees and Citizenship Canada / Immigration, Réfugiés et Citoyenneté Canada' ~ 'IRSS',
    company == 'Innovation, Science and Economic Development Canada' ~ 'ISED',
    company == 'McGill University - Desautels Faculty of Management' ~ 'McGill University',
    company == 'Pratt & Whitney Canada' ~ 'Pratt & Whitney',
    company == 'Self-employed' ~ 'Self Employed',
    company == 'Telfer School of Management' ~ 'Telfer',
    company == 'Telfer School of Management at the University of Ottawa' ~ 'Telfer',
    company == 'Telfer School of Management, University of Ottawa' ~ 'Telfer',
    company == 'The Home Depot Canada' ~ 'The Home Depot',
    company == 'Health Canada | Santé Canada' ~ 'Health Canada', 
    company == 'Canadian Tire Corporation' ~ 'Canadian Tire', 
    is.na(company) ~ 'Unemployed',
    TRUE ~ company
  ))


### COUNTS (80/20ish)
companies <- data %>% 
  count(company)
small_comps <- companies %>% 
  filter(n == 1) %>%
  pull('company')
companies <- companies %>% 
  mutate(company = case_when(n == 1 ~ 'Other', n > 1 ~ company)) %>%
  group_by(company) %>%
  mutate(count = sum(n)) %>%
  distinct() %>% 
  select(c('company', 'count')) %>%
  ungroup()
companies <- companies %>%
  add_row(company = 'Total', count = companies %>% pull(count) %>% sum()) %>%
  arrange(desc(count))
companies
ggplot(companies, aes(y=reorder(company,count), x=count)) +
  geom_col() + 
  theme_minimal() 


### UPDATE DATA (80/20)
data <- data %>% 
  mutate(company = case_when(
    company %in% small_comps ~ 'Other',
    TRUE ~ company
  ))


### CREATE NETWORK
nodes <- data %>%
  rename(person = name) %>%
  pivot_longer(cols=c('person','company')) %>% 
  rename(type = name, name = value) %>%
  mutate(name_c = case_when(
    type == 'company' ~ name,
    TRUE ~ ''
  )) %>%
  distinct() %>%
  drop_na()
nodes
edges <- data %>% 
  rename(from = name, to = company) %>%
  drop_na()
network <- tbl_graph(nodes = nodes, edges = edges) %>%
  mutate(degree = centrality_degree(mode='in'))
network

### PLOT NETWORK
network %>%
  ggraph(layout="graphopt") +
  geom_edge_link(edge_colour = "grey", alpha=0.5) +
  geom_node_point(aes(color=type, size=degree)) +
  geom_node_text(aes(label = name_c), repel=TRUE) +
  theme_graph(foreground=NA)


