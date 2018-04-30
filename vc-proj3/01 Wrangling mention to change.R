# Twit Wrangling mention to @changeOrg_ID
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

# Data mentah =====================================
change_raw <- read.csv("twit-to change.csv", header = TRUE, 
                    stringsAsFactors = FALSE, sep = ";") 

colnames(change_raw) <- c("date", "time", "user", "tweets", "replying", 
                       "rep_count", "ret_count", "fav_count","link")

# converting date format
change_raw$date <- as.Date(change_raw$date,format='%d/%m/%y')
#change_raw$ret_count <- as.integer(change_raw$ret_count)

glimpse(change_raw)

# 1. is_duplicate =================================
change_raw <- change_raw %>%
  dplyr::mutate(is_duplicate = duplicated(tweets))

# 2. user_all ===================================== 

change_raw$user <- (gsub('[@]', ' ', change_raw$user))

change_raw$tweets <- gsub("pic[^[:space:]]*", "", change_raw$tweets)
change_raw$tweets <- gsub("http[^[:space:]]*", "", change_raw$tweets)
change_raw$tweets <- gsub("https[^[:space:]]*", "", change_raw$tweets)

change_raw$user_all <- sapply(str_extract_all(change_raw$tweets, "(?<=@)[^\\s:]+", simplify = FALSE), paste, collapse=", ")

# add @ if nedeed
#user_change$User <- paste("@", user_change$User, sep=", ")

# merge column user and user_all
change_raw$user_all <- paste(change_raw$user, change_raw$user_all, sep=", ")

# Remove ChangeOrg_ID
change_raw$user_all <- gsub("ChangeOrg_ID", " ", change_raw$user_all)

# removing white space at the and
change_raw$user_all <- gsub("[[:space:]]+$", "", change_raw$user_all)

# removing punct
change_raw$user_all <- (gsub('[,]', ' ', change_raw$user_all))

# 3. user_count ==================================
change_raw$user_count <- sapply(change_raw$user_all, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

# 4. tagar =======================================
change_raw$hashtag <- sapply(str_extract_all(change_raw$tweets, "(?<=#)[^\\s]+", simplify = FALSE), paste, collapse=", ")

change_raw$hashtag <- (gsub('[,]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('[:]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('[.]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('[?]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('["]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('[+]', ' ', change_raw$hashtag))
change_raw$hashtag <- (gsub('[!]', ' ', change_raw$hashtag))

# removing white space at the and
change_raw$hashtag <- gsub("[[:space:]]+$", "", change_raw$hashtag)

# 5. tag_count ===================================
change_raw$tag_count <- sapply(change_raw$hashtag, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

# 6. clean_text ==================================
# recruiter= 
# -ecruiter=

change_raw$tweets <- gsub("recruiter=.*","",change_raw$tweets)
change_raw$tweets <- gsub("-ecruiter=.*","",change_raw$tweets)

# cleaning function
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
    str_replace_all(x, '(?<=@)[^\\s]+', '')
  }
  corpusku <- tm_map(corpusku, TrimUsers)
  #remove all "#Hashtag1"
  removehashtag <- function(x) gsub("?<=#)[^\\s]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(removehashtag))
  #merenggangkan tanda baca
  tandabaca1 <- function(x) gsub("((?:\b| )?([.,:;!?()]+)(?: |\b)?)", " \\1 ", x, perl=T)
  #corpusku <- tm_map(corpusku, content_transformer(tandabaca1))
  #remove puntuation
  removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
  corpusku <- tm_map(corpusku, content_transformer(removeNumPunct))
  corpusku <- tm_map(corpusku, stripWhitespace)
  corpusku <- tm_map(corpusku, content_transformer(tolower)) 
  #stopwords bahasa indonesia
  stopwords <- read.csv("stopwords_indo_change.csv", header = FALSE)
  stopwords <- as.character(stopwords$V1)
  stopwords <- c(stopwords, stopwords())
  corpusku <- tm_map(corpusku, removeWords, stopwords)
  #kata khusus yang dihapus
  corpusku <- tm_map(corpusku, removeWords, c("rt", "cc", "via", "jrx", "acehjakartajambisurabayabalintbpaluambon", "bali", "selamat", "pagi", "bli", "paraf", "petisi", "yks", "thn", "ri", "sign", "can", "go", "mr", "dlm", "recruiterutmsourcesharepetitionutmmediumtwitterutmcampaignsharetwittermobile"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_text <- tweet_cleaner2(change_raw$tweets)

a <- clean_text %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n=2, drop = FALSE) %>%
  count(bigram, sort = TRUE)

#View(a)
rm(a)

# 7. word_count ==================================
clean_text$word_count <- sapply(clean_text$clean_text, 
                                function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

change_raw <- bind_cols(change_raw, clean_text)
rm(clean_text)

# 8. Periode =====================================
# Periode dibagi berdasarkan tahun
change_raw <- change_raw %>%
  mutate(periode = case_when(
    date >= "2013-01-01" & date <= "2014-12-31" ~ "periode_1",
    date >= "2015-01-01" & date <= "2015-12-31" ~ "periode_2",
    date >= "2016-01-01" & date <= "2016-12-31" ~ "periode_3",
    date >= "2017-01-01" & date <= "2017-12-31" ~ "periode_4",
    TRUE ~ "periode_5")) %>%
  mutate(periode = factor(periode, levels = c("periode_1", "periode_2", "periode_3", "periode_4", "periode_5")))

#9. Parameter pencarian======================
change_raw <- change_raw %>%
  mutate(sumber_data = "twitter")

change_raw <- change_raw %>%
  mutate(parameter = "to_@changeOrg_ID")

#10. save ----
names(change_raw)

change_raw <- change_raw %>%
  select(sumber_data, parameter, date, time, periode, user, user_all, user_count, tweets,clean_text, word_count, hashtag, tag_count, is_duplicate, replying, fav_count, rep_count, ret_count, link)

write_csv(change_raw, path = "wrangled data proj-3/twit-mention-change.csv")
