library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tidyverse)
library(ggplot2)

#----------------------------------------data--------------------------------------------------
rawTweet_change <- read.csv("change_012014-012018.csv", stringsAsFactors = FALSE, 
                            header = TRUE, sep = ";", encoding = "UTF-8")
names(rawTweet_change)

colnames(rawTweet_change) <- c("date", "time", "user", "tweets", "replying", "rep_count", 
                            "ret_count", "link")

#------------------------------------take and count username-----------------------------------
# Q: are they (twitter users) consisten in participating in the discussion?
#----------------------------------------------------------------------------------------------

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

# at this point we allready have a new data (tweet_change) with 11 variables

#------------------------------------ extracting hashtag -------------------------------------
# Q: is hashtag is also develove through years
#---------------------------------------------------------------------------------------------

rawTweet_change$hashtag <- sapply(str_extract_all(rawTweet_change$tweets, "#\\S+", 
                                                   simplify = FALSE), paste, collapse=", ")

rawTweet_change$tag_count <- sapply(rawTweet_change$hashtag, 
                                     function(x) length(unlist(strsplit(as.character(x), "#\\S+"))))

#---------------------------------Saving the result for next----------------------------------
# write_tsv(data, "wr_jod.tsv")