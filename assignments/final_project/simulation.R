library(here)
library(arrow)
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(gridExtra)
library(foreach)
library(doParallel)


### LOAD DATA
data_path <- here('assignments','final_project',"672_project_data")
applications <- read_parquet(here(data_path, 'clean_applications.parquet'))
edges <- read_parquet(here(data_path, 'clean_edges.parquet'))
examiner_data <- read_parquet(here(data_path, 'clean_examiner.parquet')) %>% rename(name = examiner_id)
examiner_wg_data <- read_parquet(here(data_path, 'clean_examiner_wg.parquet')) %>% rename(name = examiner_id)


### GET SOME WORKGROUPS ONLY
wgs <- c(216)
# wgs = c(171)
exm_wgs <- examiner_wg_data %>% 
  filter(examiner_workgroup %in% wgs) %>%
  distinct(name) %>% pull(name)


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


### NETWORK INITIALIZATION
network <- tbl_graph(node_subset, edge_subset, directed=FALSE)
network <- network %>% activate('nodes') %>% 
  mutate(nb = local_members(mindist = 1))


### SIMULATION FUNCTIONS
simulate_contagion <- function(inp_net, threshold) {
  print(paste0('STARTED: ', threshold))
  
  to_break <- FALSE
  for (i in 1:100) {
    
    if (to_break == TRUE) {
      
      net_old <- inp_net %>% 
        activate('nodes') %>% 
        as_tibble() %>% 
        select(name, idea = paste0('idea_',i-1))
      net_old <- net_old %>% rename('idea_{i}' := idea)
      
      inp_net <- inp_net %>% 
        activate('nodes') %>%
        left_join(net_old, by='name', how='left')
      
    } else {
      
      net_old <- inp_net %>% 
        activate('nodes') %>% 
        as_tibble() %>% 
        select(name, idea = paste0('idea_',i-1))
      
      net_new <- inp_net %>%
        activate('nodes') %>%
        as_tibble() %>%
        rowwise() %>%
        mutate(neighb = (net_old %>% 
                           slice(unlist(nb)) %>% 
                           summarize(idea_s = mean(idea)) %>%
                           pull(idea_s))) %>%
        ungroup() %>%
        select(name, neighb, idea_init = paste0('idea_',i-1)) %>%
        mutate(idea = case_when(idea_init == 1 ~ 1,
                                neighb >= threshold ~ 1,
                                neighb < threshold ~ 0)) %>%
        mutate(idea = as.integer(idea)) %>%
        replace(is.na(.), 0) %>%
        select(name, idea)
      
      if (all_equal(net_new,net_old) == TRUE) {
        to_break <- TRUE
      }
      net_new <- net_new %>% rename('idea_{i}' := idea)
      
      inp_net <- inp_net %>% 
        activate('nodes') %>%
        left_join(net_new, by='name', how='left')
      
    }
    
    print(paste0("      ","Step ",i))
  }
  reso <- inp_net %>% activate(nodes)%>% as_tibble()
  reso <- reso %>% select(-c('nb')) %>%
    pivot_longer(!name, names_to='timestep') %>%
    mutate(timestep = as.integer(str_remove(timestep, 'idea_')))
  
  return(reso)
}
complete_simulation_run <- function(seed, thresh, node_subset, examiner_data, network, simulate_contagion) {
  
  library(tidyverse)
  library(igraph)
  library(tidygraph)
  results <- tibble()
  
  set.seed(seed)
  rand_start <- node_subset %>% sample_n(10) %>% mutate(idea_0 = 1) 
  
  set.seed(seed)
  male_start <- node_subset %>% left_join(examiner_data, on='name') %>% filter(examiner_gender == 'male') %>% 
    sample_n(10) %>% mutate(idea_0 = 1) %>% select(name, idea_0)
  
  set.seed(seed)
  female_start <- node_subset %>% left_join(examiner_data, on='name') %>% filter(examiner_gender == 'female') %>% 
    sample_n(10) %>% mutate(idea_0 = 1) %>% select(name, idea_0)
  
  set.seed(seed)
  white_start <- node_subset %>% left_join(examiner_data, on='name') %>% filter(examiner_race == 'white') %>% 
    sample_n(10) %>% mutate(idea_0 = 1) %>% select(name, idea_0)
  
  set.seed(seed)
  non_white_start <- node_subset %>% left_join(examiner_data, on='name') %>% filter(examiner_race != 'white') %>% 
    sample_n(10) %>% mutate(idea_0 = 1) %>% select(name, idea_0)
  
  set.seed(seed)
  top_10_degree <- network %>% mutate(cent = centrality_degree()) %>% as_tibble() %>%
    top_n(10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  set.seed(seed)
  bot_10_degree <- network %>% mutate(cent = centrality_degree()) %>% as_tibble() %>%
    top_n(-10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  set.seed(seed)
  top_10_bet <- network %>% mutate(cent = centrality_betweenness()) %>% as_tibble() %>%
    top_n(10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  set.seed(seed)
  bot_10_bet <- network %>% mutate(cent = centrality_betweenness()) %>% as_tibble() %>%
    top_n(-10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  set.seed(seed)
  top_10_clos <- network %>% mutate(cent = centrality_closeness()) %>% as_tibble() %>%
    top_n(10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  set.seed(seed)
  bot_10_clos <- network %>% mutate(cent = centrality_closeness()) %>% as_tibble() %>%
    top_n(-10, cent) %>% sample_n(10) %>% select(name) %>% mutate(idea_0 = 1)
  
  
  ### RANDOM START
  print("RANDOM")
  rand_start <- node_subset  %>% left_join(rand_start, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(rand_start, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'random_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### MALE START
  print("MALE")
  male_start <- node_subset  %>% left_join(male_start, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(male_start, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'male_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### FEMALE
  print("FEMALE")
  female_start <- node_subset  %>% left_join(female_start, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(female_start, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'female_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### WHITE
  print("WHITE")
  white_start <- node_subset  %>% left_join(white_start, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(white_start, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'white_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### NON-WHITE
  print("NON WHITE")
  non_white_start <- node_subset  %>% left_join(non_white_start, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(non_white_start, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'non_white_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### TOP DEGREE
  print("TOP DEG")
  top_10_degree <- node_subset  %>% left_join(top_10_degree) %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(top_10_degree, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'top_10_degree_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### BOT DEGREE 
  print("BOT DEG")
  bot_10_degree <- node_subset  %>% left_join(bot_10_degree, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(bot_10_degree, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'bot_10_degree_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### TOP BETW
  print("TOP BETW")
  top_10_bet <- node_subset  %>% left_join(top_10_bet, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(top_10_bet, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'top_10_betw_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### BOT BETW 
  print("BOT BETW")
  bot_10_bet <- node_subset  %>% left_join(bot_10_bet, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(bot_10_bet, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'bot_10_betw_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### TOP CLOSE
  print("TOP CLOSE")
  top_10_clos <- node_subset  %>% left_join(top_10_clos, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(top_10_clos, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'top_10_close_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  ### BOT CLOSE 
  print("BOT CLOSE")
  bot_10_clos <- node_subset  %>% left_join(bot_10_clos, on='name') %>% mutate(idea_0 = case_when(is.na(idea_0) ~ 0, TRUE ~ 1))
  net <- network %>% left_join(bot_10_clos, on='name')
  for (tr in thresh) {
    res <-  simulate_contagion(net, tr)
    res <- res %>% mutate(desc = 'bot_10_close_start', thresh = tr, repl=seed)
    results <- bind_rows(results, res)
  }
  
  return(results)
}


### MULTIPROCESSING
thresh1 <- seq(0.1, 0.5, by=0.01)
thresh2 <- seq(0.6, 0.9, by=0.1)
thresh <- c(thresh1, thresh2)
final_res <- tibble()

cores=detectCores()
cl <- makeCluster(cores[1]-1) 
registerDoParallel(cl)

final_res <- foreach(i=1:20, .combine=rbind) %dopar% {
  tempMatrix = complete_simulation_run(i, thresh, node_subset, examiner_data, network, simulate_contagion) 
  tempMatrix
}
stopCluster(cl)


### EXPORT
final_res %>% write_parquet(here(data_path, 'results.parquet'))


# ### interesting topic
# ### https://eehh-stanford.github.io/SNA-workshop/ergm-intro.html 
# 
# ### diffusion simulation
# ### https://dshizuka.github.io/networkanalysis/08_diffusion.html
# 
# ### looks useful
# ### https://dshizuka.github.io/networkanalysis/tutorials.html 

