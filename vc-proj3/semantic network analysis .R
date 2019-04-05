library(tidyverse)
library(tidytext)
library(stringr)
library(ggplot2)
library(igraph)
library(rgexf)

# data ----
sem_net <- tb_raw %>%
  filter(is_duplicate == FALSE) %>%
  filter(word_count >= 2)

sem_net <- sem_net %>%
  select(date, periode, clean_text)

sem_net <- sem_net %>%
  select(date, Data = clean_text)

# graph all ----
dataSet2 <- sem_net %>%
  select(Data) %>%
  unnest_tokens(bigram, Data, token = "ngrams", n = 2, to_lower = FALSE, drop = FALSE)

dataSet2 <- dataSet2 %>%
  dplyr::count(bigram) %>%
  filter(n >= 10)

graph_file <- simplify(graph.data.frame(dataSet2, directed=FALSE))

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(graph_file)), NAME = V(graph_file)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(graph_file, c(1:ecount(graph_file))))

# save and bring to gephi
write.gexf(nodes = nodes_df, edges = edges_df, defaultedgetype = "undirected", output = "sem_net_twit_btr.gexf")