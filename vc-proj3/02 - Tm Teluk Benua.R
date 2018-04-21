# Topic modelling of teluk benua tweets
# there are still need some text cleaning

# Runing RJava
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}

# read in the libraries we're going to use
library(tidyverse)
library(tidytext)
library(topicmodels)
library(tm) 
library(SnowballC)
library(RWeka)



# the LDA function using topicmodels package
bigram_tm <- function(input_text, # should be a columm from a dataframe
                      plot = T, # return a plot? TRUE by defult
                      number_of_topics = 4) # number of topics (4 by default)
{    
  # create a corpus (type of object expected by tm) and document term matrix
  Corpus <- VCorpus(VectorSource(input_text)) # make a VCorpus object spec for RWeka
  
  # function for creating bigram in the DTM
  BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
  # you can explore which term combination gave the most interpretable topic for you
  # by changing numbers inside Weka_control
  
  DTM <- DocumentTermMatrix(Corpus, control=list(tokenize=BigramTokenizer))
  
  # remove any empty rows in our document term matrix (if there are any 
  # we'll get an error when we try to run our LDA)
  unique_indexes <- unique(DTM$i) # get the index of each unique value
  DTM <- DTM[unique_indexes,] # get a subset of only those indexes
  
  # preform LDA & get the words/topic in a tidy text format
  lda <- LDA(DTM, k = number_of_topics, control = list(seed = 1234))
  topics <- tidy(lda, matrix = "beta")
  
  # get the top ten terms for each topic
  top_terms <- topics  %>% # take the topics data frame and..
    group_by(topic) %>% # treat each topic as a different group
    top_n(10, beta) %>% # get the top 10 most informative words
    ungroup() %>% # ungroup
    arrange(topic, -beta) # arrange words in descending informativeness
  
  # if the user asks for a plot (TRUE by default)
  if(plot == T){
    # plot the top ten terms for each topic in order
    top_terms %>% # take the top terms
      mutate(term = reorder(term, beta)) %>% # sort terms by beta value 
      ggplot(aes(term, beta, fill = factor(topic))) + # plot beta by theme
      geom_col(show.legend = FALSE) + # as a bar plot
      facet_wrap(~ topic, scales = "free") + # which each topic in a seperate plot
      labs(x = NULL, y = "Beta") + # no x label, change y label 
      coord_flip() # turn bars sideways
  }else{ 
    # if the user does not request a plot
    # return a list of sorted terms instead
    return(top_terms)
  }
}

# data 
test_data <- read.csv("tm_benua.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

# execution
test_bigram <- bigram_tm(test_data$clean_text, number_of_topics = 10) # plot top ten terms
test_bigram
