library(tidyverse)
library(tidytext)
library(textclean)
library(ggplot2)
library(igraph)
library(stringi)
library(katadasaR)
library(tm)
library(translateR)

setwd( "/Volumes/mydata/RStudio/virtualCitizens/vc-proj3")

verb_1 <- read_delim("verbatime_bali.csv", delim = ";")
colnames(verb_1) <- c("subjek", "percakapan")

# col subjek cleaning ----
rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
verb_1$subjek <- gsub("\\bA dan C\\b", "C", verb_1$subjek)
verb_1$subjek <- rem_spc_front(verb_1$subjek)
verb_1$subjek <- rem_spc_back(verb_1$subjek)

# klasifikasi konten ----
verb_1 <- verb_1 %>%
  mutate(kategori_subjek = case_when(
    subjek == "Bli" ~ "partisipan",
    subjek == "Bokis" ~ "partisipan",
    subjek == "Gilang" ~ "partisipan",
    TRUE ~ "peneliti")) %>%
  mutate(kategori_subjek = factor(kategori_subjek, levels = c("partisipan", "peneliti")))

verb_1$percakapan <- replace_number(x = verb_1$percakapan, remove = TRUE)

glimpse(verb_1)

# cleaning percakapan ----
# 1. replace number to chr
# 2. replace non ASCII
# 3. replace extra white space
# 3. Lowercasing
# 4. Punctuation
# 5. Stopwords
# 6. Special words (if nedeed)
# 7. menggunakan `katadasaR` -> sapply(words, katadasaR)

percakapan <- verb_1$percakapan # to vector
percakapan <- stri_conv(percakapan, from = NULL, to = "ASCII", to_raw = FALSE) #convert chr to ascii
percakapan <- replace_non_ascii(percakapan)
percakapan <- tolower(percakapan)
percakapan <- replace_white(percakapan)
percakapan <- data_frame(percakapan)

rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)


# seberapa sering subjek nanya/menjawab ---- 
verb_1 %>%
  select(subjek) %>%
  group_by(subjek) %>%
  count(subjek) %>%
  ggplot(aes(subjek, n)) + geom_col() + 
  ggtitle("Jumlah pernyataan/pertanyaan yang disampaikan") +
  labs(x = "subjek", y = "jumlah")

# perbandingan jumlah kata penanya & penjawab ----
verb_1 %>%
  unnest_tokens(kata, percakapan, token = "ngrams", n = 2, to_lower = TRUE)

write_csv(verb_1, path = "wrangled data proj-3/hasil-wawancara.csv")
