library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tidyverse)
library(ggplot2)
library(tm)

#----------------------------------------data--------------------------------------------------
rawTweet_change <- read.csv("change_012014-012018.csv", stringsAsFactors = FALSE, 
                            header = TRUE, sep = ";")
names(rawTweet_change)

colnames(rawTweet_change) <- c("date", "time", "user", "tweets", "replying", "rep_count", 
                            "ret_count", "fav_count","link")

#------------------------------------take and count username-----------------------------------
# PQ: are they (twitter users) consisten in participating in the discussion?
# extract all string start with @ and put in a new column named username_com
rawTweet_change$user_all <- sapply(str_extract_all(rawTweet_change$tweets, "@\\S+", 
                                                   simplify = FALSE), paste, collapse=", ")
# add @ if nedeed
#user_change$User <- paste("@", user_change$User, sep="")

# merge column user and user_all
rawTweet_change$user_all <- paste(rawTweet_change$user, rawTweet_change$user_all, sep=" ")
str(rawTweet_change)

rawTweet_change$user_count <- sapply(rawTweet_change$user_all, 
                              function(x) length(unlist(strsplit(as.character(x), "@\\S+"))))

#--------------------------------- cleaning tweets column ------------------------------------
# fungsi untuk cleaning 
tweet_cleaner <- function(input_text) # nama kolom yang akan dibersihkan
{    
  # create a corpus (type of object expected by tm) and document term matrix
  corpusku <- Corpus(VectorSource(input_text)) # make a corpus object
  # remove urls1
  removeURL1 <- function(x) gsub("http[^[:space:]]*", "", x) 
  corpusku <- tm_map(corpusku, content_transformer(removeURL1))
  #remove urls3
  removeURL2 <- function(x) gsub("pic[^[:space:]]*", "", x) 
  corpusku <- tm_map(corpusku, content_transformer(removeURL2))
  #remove puntuation
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]@#]*", "", x) # kecuali @ dan #
  corpusku <- tm_map(corpusku, content_transformer(removeNumPunct))
  
  corpusku <- tm_map(corpusku, stripWhitespace)
  #kata khusus yang dihapus
  corpusku <- tm_map(corpusku, removeWords, c("iki"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  
  data <- data.frame(text=sapply(corpusku, identity),stringsAsFactors=F)
  
  a <- bind_cols(rawTweet_change, data)
}

# clean
rawTweet_change <- tweet_cleaner(rawTweet_change$tweets)


#------------------------------------ extracting hashtag -------------------------------------
# PQ: is hashtag is also develove through years

rawTweet_change$hashtag <- sapply(str_extract_all(rawTweet_change$text, "#\\S+", 
                                                   simplify = FALSE), paste, collapse=", ")

rawTweet_change$tag_count <- sapply(rawTweet_change$hashtag, 
                                     function(x) length(unlist(strsplit(as.character(x), "#\\S+"))))

# sometimes people also use hastag for theri picture post which become urls here,
# maybe we have to clean it all at first before hashtag extraction

#------------------------------- Combine date and time column --------------------------------
# PQ: ....?
rawTweet_change$date <- paste(rawTweet_change$date, rawTweet_change$time, sep=" ")
rawTweet_change = subset(rawTweet_change, select = -c(time))
rawTweet_change = subset(rawTweet_change, select = -c(tweets))
#---------------------------------Saving the result for next----------------------------------
write_tsv(rawTweet_change, "tweets_change.tsv")