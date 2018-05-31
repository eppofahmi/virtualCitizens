# Wrangling komentar petisi tentang teluk benoa

# library
library(tm)
library(textclean)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)

# Data ----
# petisi 1
dirwd <- paste(getwd(),"/wrangled data proj-3/",sep='')
petisi1 <- read.csv(paste(dirwd,"change-segera cabut.csv", sep=''), header = TRUE, sep = ';', stringsAsFactors = FALSE)
# petisi 2
petisi2 <- read.csv(paste(dirwd,"change-pak jokowi.csv", sep=''), header = TRUE, sep = ';', stringsAsFactors = FALSE)
# petisi 3
petisi3 <- read.csv(paste(dirwd,"change-revoke.csv", sep=''), header = TRUE, sep = ';', stringsAsFactors = FALSE)

petisi_tb <- bind_rows(petisi1 %>%
                         mutate(judul= "petisi_1"),
                       petisi2 %>%
                         mutate(judul= "petisi_2"),
                       petisi3 %>%
                         mutate(judul= "petisi_3"))

rm(petisi1, petisi2, petisi3)
petisi_tb$clean_text <- petisi_tb$Alasan

# clean 1 ----
# punct and symbol normalisation ----
petisi_tb$clean_text <- replace_white(petisi_tb$clean_text)
petisi_tb$clean_text <- add_comma_space(petisi_tb$clean_text)
petisi_tb$clean_text <- replace_symbol(petisi_tb$clean_text)
petisi_tb$clean_text <- replace_non_ascii(petisi_tb$clean_text, remove.nonconverted = TRUE)
petisi_tb$clean_text <- gsub("((?:\b| )?([.,:;!?()]+)(?: |\b)?)", " \\1 ", petisi_tb$clean_text, perl=T)
petisi_tb$clean_text <- replace_white(petisi_tb$clean_text)


# mengganti - menjadi to untuk tangal
petisi_tb$clean_text <- gsub("\\b-\\b", ' to ', petisi_tb$clean_text)
petisi_tb$clean_text <- gsub("\\b/\\b", ' ', petisi_tb$clean_text)
# remove punct
petisi_tb$clean_text <- strip(petisi_tb$clean_text, char.keep = c("/"), digit.remove = FALSE, apostrophe.remove = TRUE, lower.case = FALSE)

# replacing number with chr
petisi_tb$clean_text <- replace_number(petisi_tb$clean_text, num.paste = TRUE, remove = FALSE)

# clean 2 ----
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
  corpusku <- tm_map(corpusku, removeWords, c("yg", "yg", "n", "hemmmmmm", "adala", "adalah", "aj", "aje", "aka", "menandatangani", "ku", "sdh", "dr", "udah", "gw", "tsb", "dll", "sya", "say", "bli", "dlm", "orng", "ok", "deh", "man", "msh", "thn", "nggak", "skrg", "yth", "balii", "dpt", "teluk", "benoa"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

petisi_clean <- tweet_cleaner2(petisi_tb$clean_text)
petisi_tb <- bind_cols(petisi_tb, petisi_clean)
rm(petisi_clean)

names(petisi_tb)

petisi_tb <- petisi_tb %>%
  select(-clean_text)

colnames(petisi_tb) <- c("nama", "waktu", "alasan", "likes", "links", "judul", "alasan_clean")

petisi_tb$word_count <- sapply(petisi_tb$alasan_clean, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

write_csv(petisi_tb, path = 'wrangled data proj-3/petisi_tb.csv')


# ekplorasi term ----
petisi_term <- petisi_tb %>%
  filter(word_count >= 2) %>%
  unnest_tokens(kata, alasan_clean, token = "ngrams", n = 2, to_lower = TRUE, drop = TRUE) %>%
  count(kata, sort = TRUE)

petisi_term %>%
  head(n = 20) %>%
  ggplot(aes(reorder(kata, n), n)) + geom_col() +
  ggtitle("Reklamasi Teluk Benoa dalam Media") +
  labs(x = "Media", y = "Jumlah Berita") + 
  coord_flip()
