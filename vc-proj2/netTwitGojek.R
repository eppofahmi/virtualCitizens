library(tidytext)
library(dplyr)
library(textclean)
library(qdap)

net_data <- net_twit_gojek
net_data$Data <- replace_white(net_data$Data)

net_data$Count <- word_count(net_data$Data, byrow = TRUE, missing = NA,
           digit.remove = TRUE, names = FALSE)

net_data <- net_data %>%
  filter(Count >= 2)

net_data <- net_data %>%
  unnest_tokens(bigram, Data, token = "ngrams", n = 2)

net_data <- net_data %>%
  separate(bigram, into = c("a", 'b'), sep = " ")

net_data <- net_data[ ,-1]


net_gojek <- graph_from_data_frame(d = net_data, directed = FALSE)

write_graph(net_gojek, "net_gojek.graphml", format = "graphml")
