# Sumber data: Twitter
# Parameter pencarian:
# 1. #savegojek
# 2. #savedrivergojek
# 3. #saveojekonline

# Library ----
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(igraph)

# Data ----
saveojekonline <- read.csv("twit-saveojekonline.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
savegojek <- read.csv("twit-savegojek.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
savedrivergojek <- read.csv("twit-savedrivergojek.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
twit_cerita <- read.csv("twit-ceritatransonline.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)

twit_cerita$V7 <- as.integer(twit_cerita$V7)
twit_cerita$V8 <- as.integer(twit_cerita$V8)

twit_gojek <- bind_rows(saveojekonline %>%
                          mutate(parameter = "#saveojekonline"), 
                        savegojek %>%
                          mutate(parameter = "#savegojek"), 
                        savedrivergojek %>%
                          mutate(parameter = "#savedrivergojek"), 
                        twit_cerita %>%
                          mutate(parameter = "to_@CeritaTranspOL"))

rm(savedrivergojek, savegojek, saveojekonline, twit_cerita)

glimpse(twit_gojek)

twit_gojek <- twit_gojek %>%
  mutate(sumber_data = "twitter")

colnames(twit_gojek) <- c("date", "time", "user", "tweets", "replying", 
                       "rep_count", "ret_count", "fav_count","link", 
                       "parameter", "sumber_data")

# converting date format ----
twit_gojek$date <- as.Date(twit_gojek$date,format='%d %b %Y')

# 1. is_duplicate =================================
twit_gojek <- twit_gojek %>%
  dplyr::mutate(is_duplicate = duplicated(tweets))

# 2. user_all ===================================== 
twit_gojek$user <- (gsub('[@]', ' ', twit_gojek$user))
twit_gojek$tweets <- gsub("pic[^[:space:]]*", "", twit_gojek$tweets)
twit_gojek$tweets <- gsub("http[^[:space:]]*", "", twit_gojek$tweets)
twit_gojek$tweets <- gsub("https[^[:space:]]*", "", twit_gojek$tweets)

# put space 
twit_gojek$tweets <- gsub("([[:alnum:]])([^[:alnum:][:space:]_])", "\\1 \\2", twit_gojek$tweets)

twit_gojek$user_all <- sapply(str_extract_all(twit_gojek$tweets, "(?<=@)[^\\s:]+", simplify = FALSE), paste, collapse=", ")

# merge column user and user_all
twit_gojek$user_all <- paste(twit_gojek$user, twit_gojek$user_all, sep=", ")

# Remove ChangeOrg_ID
twit_gojek$user_all <- gsub("ChangeOrg_ID", " ", twit_gojek$user_all)

# removing white space at the and
twit_gojek$user_all <- gsub("[[:space:]]+$", "", twit_gojek$user_all)

# removing punct
twit_gojek$user_all <- (gsub('[,]', ' ', twit_gojek$user_all))

# 3. user_count ==================================
twit_gojek$user_count <- sapply(twit_gojek$user_all, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))


# 4. tagar =======================================
twit_gojek$hashtag <- sapply(str_extract_all(twit_gojek$tweets, "(?<=#)[^\\s]+", simplify = FALSE), paste, collapse=", ")

twit_gojek$hashtag <- (gsub('[,]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('[:]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('[.]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('[?]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('["]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('[+]', ' ', twit_gojek$hashtag))
twit_gojek$hashtag <- (gsub('[!]', ' ', twit_gojek$hashtag))

# removing white space at the and
twit_gojek$hashtag <- gsub("[[:space:]]+$", "", twit_gojek$hashtag)

# 5. tag_count ===================================
twit_gojek$tag_count <- sapply(twit_gojek$hashtag, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

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
  corpusku <- tm_map(corpusku, removeWords, c("rt", "cc", "via", "jrx", "balitolakreklamasi", "acehjakartajambisurabayabalintbpaluambon", "bali", "selamat", "pagi", "bli", 'ed','co','bd','ri','gl','leb','ra','dun', 'hat', 'onl', 'rv', 'mem', 'gad', 'ket', 'set', 'mungk', 'orang', 'www', 'gojek', 'indones', "ter", 'bero'))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_text <- tweet_cleaner2(twit_gojek$tweets)

a <- clean_text %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n=2, drop = FALSE) %>%
  count(bigram, sort = TRUE)

#View(a)
rm(a)

# 7. word_count ==================================
clean_text$word_count <- sapply(clean_text$clean_text, 
                                function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

twit_gojek <- bind_cols(twit_gojek, clean_text)
rm(clean_text)

# 8. Periode =====================================
# periode_1 = awal - maret 2014 ---------------> 2013-07-01 & 2014-03-31
# periode_2 = april 2014 - agustus 2014 -------> 2014-04-01 & 2014-08-31
# periode_3 = september 2014 hingga akhir -----> 2014-09-01 & 2018-03-31

twit_gojek <- twit_gojek %>%
  mutate(periode = case_when(
    date >= "2015-12-01" & date <= "2016-12-31" ~ "periode_1",
    date >= "2017-01-01" & date <= "2017-12-31" ~ "periode_2",
    TRUE ~ "periode_3")) %>%
  mutate(periode = factor(periode, levels = c("periode_1", "periode_2", "periode_3")))

glimpse(twit_gojek)

#9. Parameter pencarian======================
#twit_gojek <- twit_gojek %>%
#  mutate(sumber_data = "twitter")

#twit_gojek <- twit_gojek %>%
#  mutate(parameter = "#balitolakreklamasi")

#10. save data ----

names(twit_gojek)

twit_gojek <- twit_gojek %>%
  select(sumber_data, parameter, date, time, periode, user, user_all, user_count, tweets,clean_text, word_count, hashtag, tag_count, is_duplicate, replying, fav_count, rep_count, ret_count, link)

twit_gojek <- twit_gojek %>%
  filter(is_duplicate == FALSE) %>%
  filter(!user == "@poocongs")
  
write_csv(twit_gojek, path = "wrangled data proj-2/twit-gojek.csv")

#11. net data ----
net_twit_gojek <- twit_gojek %>%
  filter(!user == "@poocongs") %>%
  filter(is_duplicate == FALSE) %>%
  filter(user_count >= 2) %>%
  select(user_all)

colnames(net_twit_gojek) <- "Data"

write_csv(net_twit_gojek, path = "wrangled data proj-2/net-twit-gojek.csv")