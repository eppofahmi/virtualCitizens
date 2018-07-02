# media online di ambil dari beritabali.com, balipost.com, tempo.co, dan kompas.com

# lib ----
library(textclean)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(tm)

# data ----
dirwd <- paste(getwd(),"/wrangled data proj-2/",sep='')
gojek_tempo <- read_csv(paste(dirwd, "media-tempo-transportasionline.csv", sep = ''), col_names = FALSE)
gojek_kompas <- read_csv(paste(dirwd, "media-kompas-transportasionline.csv", sep = ''), col_names = FALSE)
gojek_detik <- read_csv(paste(dirwd, "media-detik-transportasionline.csv", sep = ''), col_names = FALSE)

# menggabungkan
gojek_media <- bind_rows(gojek_tempo %>%
                        mutate(sumber = "tempo"), 
                      gojek_kompas %>%
                        mutate(sumber = "kompas"), 
                      gojek_detik %>%
                        mutate(sumber = "detik"))

rm(gojek_kompas, gojek_tempo, gojek_detik)

colnames(gojek_media) <- c("tanggal", "judul", "konten", "media")

# remving duplicate conten
gojek_media <- gojek_media %>%
  mutate(duplicate = duplicated(judul, konten)) %>%
  filter(duplicate == FALSE)

# cleaning 1 ---- 
# 1. removing non ascii chr from `judul` and `konten`
gojek_media$judul <- replace_non_ascii(gojek_media$judul, remove.nonconverted = TRUE)

gojek_media$konten <- replace_non_ascii(gojek_media$konten, remove.nonconverted = TRUE)
gojek_media$konten <- replace_white(gojek_media$konten)
gojek_media$konten <- add_comma_space(gojek_media$konten)
gojek_media$konten <- replace_symbol(gojek_media$konten)
gojek_media$konten <- strip(gojek_media$konten, char.keep = c("-", "/", ".", ","), digit.remove = FALSE, apostrophe.remove = TRUE, lower.case = FALSE)

# 2. replacing number with words
gojek_media$konten_clean <- gojek_media$konten
gojek_media$konten_clean <- replace_number(gojek_media$konten_clean, num.paste = TRUE, remove = FALSE)

# 3. removing media names 
gojek_media$konten_clean <- gsub("\\bTEMPO.CO\\b", '', gojek_media$konten_clean)
gojek_media$konten_clean <- gsub("\\bKOMPAS.com\\b", '', gojek_media$konten_clean)
gojek_media$konten_clean <- gsub("\\bGo-Jek\\b", 'GoJek', gojek_media$konten_clean)
gojek_media$konten_clean <- gsub("\\b-\\b", ' ', gojek_media$konten_clean)
gojek_media$konten_clean <- gsub("\\b/\\b", ' ', gojek_media$konten_clean)
gojek_media$konten_clean <- gsub("\\b.\\b", ' ', gojek_media$konten_clean)

gojek_media$konten_clean <- replace_number(gojek_media$konten_clean, num.paste = TRUE, remove = FALSE)

print(as.character(head(gojek_media$konten_clean, n = 1)))

# cleaning 2 ----
media_cleaner <- function(input_text) # nama kolom yang akan dibersihkan
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
  stopwords <- read.csv("stopwords_indo.csv", header = FALSE)
  stopwords <- as.character(stopwords$V1)
  stopwords <- c(stopwords, stopwords())
  corpusku <- tm_map(corpusku, removeWords, stopwords)
  #kata khusus yang dihapus
  corpusku <- tm_map(corpusku, removeWords, c("baca", "simak", "or", "to", "transportasi", "online", "cmd", "googletag", "displaydiv", "pushfunction", "aa"))
  corpusku <- tm_map(corpusku, stripWhitespace)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(clean_text=sapply(corpusku, identity),stringsAsFactors=F)
}

clean_konten <- media_cleaner(gojek_media$konten_clean)
media_data <- bind_cols(gojek_media, clean_konten)

media_data <- media_data %>%
  select(-konten_clean)

colnames(media_data) <- c("tanggal", "judul", "konten", "media", "is_duplicate", "clean_konten")

write_csv(media_data, path = 'wrangled data proj-2/media data proj-2.csv')

# eksplrasi ----
library(tidytext)

media_term <- media_data %>%
  unnest_tokens(term, clean_konten,  token = "ngrams", n = 2,  drop = TRUE) %>%
  count(term, sort = TRUE)

media_term %>%
  top_n(20) %>%
  ggplot(aes(reorder(term, n), n)) + geom_col() + coord_flip()

rm(clean_konten, gojek_media)

# put on txt per row ----
to_txt <- media_data %>%
  select(judul, tanggal, media, konten)

to_txt$judul <- toupper(to_txt$judul)
to_txt$media <- toupper(to_txt$media)

to_txt$judul <- gsub("([-])|[[:punct:]]", "\\1", to_txt$judul)

for(i in seq(nrow(to_txt))) {
  text <- as.character(to_txt$konten[i]) 
  judul <- as.character(to_txt$judul[i])
  tanggal <- as.character(to_txt$tanggal[i])
  media <- as.character(to_txt$media[i])
  # Create the file name
  filename <- paste0("/Volumes/mydata/RStudio/virtualCitizens/vc-proj2/output file proj-2/", to_txt$judul[i], ".txt")
  sink(file = filename) %>% # open file to write 
    cat(media, tanggal, judul, text, sep = "|")  # write the file
  sink() # close the file
}
