# Data Wrangling
# script ini digunakan untuk membuat data frame dari data mentah dengan menambahkan:
# 1. menambah kolom berisi username
# 2. menambah kolom berisi jumlah username per row

# library
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(tidyverse)
library(ggplot2)

# data
data_mentah1 <- read.csv("hasil-new1.csv", stringsAsFactors = FALSE, header = FALSE, sep = ",", 
                         encoding = "UTF-8")

# add @ to column V3
user <- data_mentah1[,c("V3", "V4")]
user$V3 <- paste("@", user$V3, sep="")

# merge the V3 and V4 column
user$username <- paste(user$V3, user$V4, sep=" ")
user$username_com <- str_extract_all(user$username, "@\\w+", simplify = FALSE)

# calculate username per row
user$username_count <- sapply(user$username_com, 
                              function(x) length(unlist(strsplit(as.character(x), "\\^@+"))))

data <- bind_cols(data_mentah1, user)

data <- data[,c("V1", "V2","V3", "V4", "V5", "V6", "V7", "V8", 
                            "username_com", "username_count")]

colnames(data) <- c("date", "time", "username", "tweets", "replying", "rep_coun", 
                    "ret_count", "fav_count", "user_all", "user_count")

# deleting empty row no 13493
data <- data[-13493, ] 

data$user_all <- as.character(data$user_all)
data$user_all = gsub("^c\\(|\\)$", "", data$user_all) # remove c()
data$user_all = gsub("[^[:alnum:]@_]", " ", data$user_all) # remove ""
data$user_all = gsub("\\s+", " ", str_trim(data$user_all)) # remove space

# Saving the result for next 
# write_tsv(data, "wr_jod.tsv")

# Counting user 
jml_user <- unlist(user$username_com)
# calculate hashtag frequencies
jml_user = table(jml_user)
jml_user = as.data.frame(jml_user)

# explorasition 1 - username
# akun mana (siapa) yang paling sering ngetwit dengan tagar?
jml_user %>%
  filter(Freq > 100) %>%
  ggplot(aes(x = reorder(jml_user, Freq), y = Freq)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, 
       y = "Frekuensi sebuah akun mentwit dengan tagar #jogjaoradidol")

# the answear is @dodoputrabangsa
# apakah ada perubahan username dari waktu ke waktu?
# step to ansear the qustion:
## 1. make a new data frame with date, user_all, and user_count
## 2. group the data by year - merge rows in user_all column
## 3. count the usernames for each year/6months