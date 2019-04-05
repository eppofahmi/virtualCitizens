# Plotting networks in R ----
# An example how to use R and rgexf package to create a .gexf file for network visualization in Gephi

# Load libraries ----
library(tidyverse)
library(tidytext)

dataSet <- tm_data_twit

dataSet_token <- dataSet %>%
  select(clean_text) %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n = 2, to_lower = TRUE) %>%
  dplyr::count(bigram, sort = TRUE) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

colnames(dataSet_token) <- c("V1", "V2", "V3")

# Create a graph. Use simplify to ensure that there are no duplicated edges or self loops
library(plyr)
library(igraph)

gD <- simplify(graph.data.frame(dataSet_token, directed=FALSE))
print(head(gD))

plot(gD)
