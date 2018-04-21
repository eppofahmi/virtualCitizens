# Sumber data: Playstore

# Library ----
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)

# driver ----
driver_comment <- read.csv("gojekdriverkomen.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE)
driver_comment <- driver_comment %>%
  select(V1) %>%
  separate(V1, into = c("tanggal", "tahun", "nama", "komen"), sep = ",") %>%
  unite_("tanggal", c("tahun", "tanggal"), sep = " ")
driver_comment$tanggal <- as.Date(driver_comment$tanggal, format = "%Y %b %d")

driver_comment <- driver_comment %>%
  mutate(sumber_data = "playstore") %>%
  mutate(aplikasi = "gojek_driver")

# user ----
user_comment <- read.csv("gojekuserkomen.csv", sep = ';', header = FALSE, stringsAsFactors = FALSE)
user_comment <- user_comment %>%
  unite("komen", c(V3:V7), sep = " ")
colnames(user_comment) <- c("tanggal", "nama", "komen")
user_comment$nama <- gsub('[[:punct:] ]+',' ', user_comment$nama)
user_comment$tanggal <- gsub('[[:punct:] ]+',' ', user_comment$tanggal)
user_comment$komen <- gsub('[[:punct:] ]+',' ', user_comment$komen)
user_comment$tanggal <- mdy(user_comment$tanggal)

user_comment <- user_comment %>%
  mutate(sumber_data = "playstore") %>%
  mutate(aplikasi = "gojek_customer")

playstore_data <- bind_rows(user_comment, driver_comment)

# cleaning----
tweet_cleaner2 <- function(input_text) # nama kolom yang akan dibersihkan
{    
  # create a corpus (type of object expected by tm) and document term matrix
  corpusku <- Corpus(VectorSource(input_text)) # make a corpus object
  # remove urls1
  removeURL1 <- function(x) gsub("http[^[:space:]]*", "", x) 
  corpusku <- tm_map(corpusku, content_transformer(removeURL1))
  #remove urls3
  removeURL2 <- function(x) gsub("pic[^[:space:]]*", "", x) 
  corpusku <- tm_map(corpusku, content_transformer(removeURL2))
  #remove username 
  TrimUsers <- function(x) {
    str_replace_all(x, '(@[[:alnum:]_]*)', '')
  }
  corpusku <- tm_map(corpusku, TrimUsers)
  #remove all "#Hashtag1"
  removehashtag <- function(x) gsub("#\\S+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(removehashtag))
  #merenggangkan tanda baca
  #tandabaca1 <- function(x) gsub("((?:\b| )?([.,:;!?()]+)(?: |\b)?)", " \\1 ", x, perl=T)
  #corpusku <- tm_map(corpusku, content_transformer(tandabaca1))
  #remove puntuation
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  corpusku <- tm_map(corpusku, content_transformer(removeNumPunct))
  corpusku <- tm_map(corpusku, stripWhitespace)
  corpusku <- tm_map(corpusku, content_transformer(tolower)) 
  #stopwords bahasa indonesia
  stopwords <- read.csv("stopwords_indo.csv", header = FALSE)
  stopwords <- as.character(stopwords$V1)
  stopwords <- c(stopwords, stopwords())
  corpusku <- tm_map(corpusku, removeWords, stopwords)
  #kata khusus yang dihapus
  corpusku <- tm_map(corpusku, removeWords, c("rt", "ok", "thanks", "thx", "duuuuudddee"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_text <- tweet_cleaner2(playstore_data$komen)

a <- clean_text %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n=2, drop = FALSE) %>%
  count(bigram, sort = TRUE)

rm(a)

# clean_text----
playstore_data <- bind_cols(playstore_data, clean_text)

playstore_data <- playstore_data %>%
  select(sumber_data, aplikasi, date = tanggal, username = nama, text = komen, clean_text)

# saving the data
write_csv(playstore_data, path = "wrangled data proj-2/playstore-gojek.csv")
