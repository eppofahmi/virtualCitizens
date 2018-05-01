# Twit Wrangling tagar #balinotforsale
# Script ini digunakan untuk warangling data twit yang didapat dengan parameter mention ke username @changeOrg_ID

# Kolom tambahan ===========================
# 1. is_duplicate = FALSE/TRUE
# 2. user_all = berisi daftar user yang ada dalam data (dari kolom user dan tweets)
# 3. user_count = jumlah @ dari user_all
# 4. tagar = term dengan awalan # dari kolom `tweets`
# 5. tag_count = jumlah tagar per twit dari kolom `tagar`
# 6. clean_text = text yang sudah dibersihkan dari kolom `tweets`
# 7. word_count = jumlah kata dari kolom `clean_text`
# 8. periode = keterangan asal twit berdasarkan tahun
# 9. sumber_data = menunjukkan sumber data (e.g twitter, change.org, kitabisa.com)
# 10. Parameter = menunjukkan parameter yang digunakan untuk mendapatkan data

# Library ----
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)

bns_raw <- read.csv("twit-bnfs.csv", header = FALSE, stringsAsFactors = FALSE, sep = ";") 
colnames(bns_raw) <- c("date", "time", "user", "tweets", "replying", "rep_count", "ret_count", "fav_count","link")

# converting date format
bns_raw$date <- as.Date(bns_raw$date,format='%d %b %Y')
bns_raw$ret_count <- as.integer(bns_raw$ret_count)

glimpse(bns_raw)

# 1. is_duplicate =================================
bns_raw <- bns_raw %>%
  dplyr::mutate(is_duplicate = duplicated(tweets))

# 2. user_all ===================================== 
bns_raw$user <- (gsub('[@]', ' ', bns_raw$user))
bns_raw$tweets <- gsub("pic[^[:space:]]*", "", bns_raw$tweets)
bns_raw$tweets <- gsub("http[^[:space:]]*", "", bns_raw$tweets)
bns_raw$tweets <- gsub("https[^[:space:]]*", "", bns_raw$tweets)

# put space 
bns_raw$tweets <- gsub("([[:alnum:]])([^[:alnum:][:space:]_])", "\\1 \\2", bns_raw$tweets)

# extracting
bns_raw$user_all <- sapply(str_extract_all(bns_raw$tweets, "(?<=@)[^\\s:]+", simplify = FALSE), paste, collapse=", ")

# merge column user and user_all
bns_raw$user_all <- paste(bns_raw$user, bns_raw$user_all, sep=", ")

# removing punct
bns_raw$user_all <- gsub("[^[:alnum:][:space:]_]", " ", bns_raw$user_all)

# removing white space at the start and end of strings
bns_raw$user_all <- gsub("^[[:space:]]+", "", bns_raw$user_all)
bns_raw$user_all <- gsub("[[:space:]]+$", "", bns_raw$user_all)

# 3. user_count ==================================
bns_raw$user_count <- sapply(bns_raw$user_all, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

# 4. tagar =======================================
bns_raw$hashtag <- sapply(str_extract_all(bns_raw$tweets, "(?<=#)[^\\s]+", simplify = FALSE), paste, collapse=", ")

# removing punct in hashtag
bns_raw$hashtag <- gsub("[^[:alnum:][:space:]-]", " ", bns_raw$hashtag)

# removing white space at the start
bns_raw$hashtag <- gsub("^[[:space:]]+", "", bns_raw$hashtag)
bns_raw$hashtag <- gsub("[[:space:]]+$", "", bns_raw$hashtag)

# 5. tag_count ===================================
bns_raw$tag_count <- sapply(bns_raw$hashtag, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

# 6. clean_text ==================================
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
  corpusku <- tm_map(corpusku, removeWords, c("rt", "cc", "via", "jrx", "balitolakreklamasi","bali", "selamat", "pagi", "bli", "paraf", "petisi", "yks", "thn", "ri", "sign", "can", "go", "mr", "dlm"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_text <- tweet_cleaner2(bns_raw$tweets)

a <- clean_text %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n=2, drop = FALSE) %>%
  count(bigram, sort = TRUE)

#View(a)
rm(a)

# 7. word_count ==================================
clean_text$word_count <- sapply(clean_text$clean_text, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

bns_raw <- bind_cols(bns_raw, clean_text)
rm(clean_text)

# 8. Periode =====================================
# Periode dibagi berdasarkan tahun
bns_raw <- bns_raw %>%
  mutate(periode = case_when(
    date >= "2012-01-01" & date <= "2014-03-31" ~ "periode_1",
    date >= "2014-04-01" & date <= "2014-08-31" ~ "periode_2",
    TRUE ~ "periode_3")) %>%
  mutate(periode = factor(periode, levels = c("periode_1", "periode_2", "periode_3")))

#9. Parameter pencarian======================
bns_raw <- bns_raw %>%
  mutate(sumber_data = "twitter")

bns_raw <- bns_raw %>%
  mutate(parameter = "#balinotforsale")

#10. save ----
names(bns_raw)

bns_raw <- bns_raw %>%
  select(sumber_data, parameter, date, time, periode, user, user_all, user_count, tweets,clean_text, word_count, hashtag, tag_count, is_duplicate, replying, fav_count, rep_count, ret_count, link)

write_csv(bns_raw, path = "wrangled data proj-3/twit-tagar-balinotforsale.csv")