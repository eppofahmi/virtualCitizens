library(tm)
library(textclean)
library(tidyverse)
library(tidytext)

media_data <- tb_media

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
  corpusku <- tm_map(corpusku, removeWords, c("baca", "simak", "or", "to", "teluk", "benoa", "zeroms", "opacity", "transition", "text", "bold", "font", "weight", "onepx", "decoration", "important", "none"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_text <- tweet_cleaner2(media_data$konten)

media_data <- bind_cols(tb_media, clean_text)

a <- media_data %>%
  unnest_tokens(bigram, clean_text, token = "ngrams", n=3, drop = FALSE) %>%
  count(bigram, sort = TRUE)

a %>%
  top_n(10) %>%
  ggplot(aes(reorder(bigram, n), n)) + geom_col() + 
  coord_flip()
  
head(n=2, media_data$konten)

write_csv(media_data, path = 'wrangled data proj-3/media data.csv')

# ekspor ke txt per row ----
to_txt <- media_data %>%
  select(judul, tanggal, media, konten)

to_txt$judul <- toupper(to_txt$judul)
to_txt$media <- toupper(to_txt$media)


for(i in seq(nrow(to_txt))) {
  text <- as.character(to_txt$konten[i]) 
  judul <- as.character(to_txt$judul[i])
  tanggal <- as.character(to_txt$tanggal[i])
  media <- as.character(to_txt$media[i])
  # Create the file name
  filename <- paste0("/Volumes/mydata/RStudio/virtualCitizens/vc-proj3/output file proj-3/", to_txt$judul[i], ".txt")
  sink(file = filename) %>% # open file to write 
    cat(media, tanggal, judul, text, sep = "|")  # write the file
  sink() # close the file
}

unique(to_txt$judul)
