# media online di ambil dari beritabali.com, balipost.com, tempo.co, dan kompas.com

set.seed(2018)

# lib ----
library(tidyverse)
library(tidytext)
library(lubridate)
library(ggplot2)
library(textclean)
library(tm)

# data ----
dirwd <- paste(getwd(),"/wrangled data proj-3/",sep='')
tb_tempo <- read_csv(paste(dirwd, "media-tempo-reklamasitelukbenoa.csv", sep = ''), col_names = FALSE)
tb_kompas <- read_csv(paste(dirwd, "media-kompas-reklamasitelukbenoa.csv", sep = ''), col_names = FALSE)
tb_beritabali <- read_csv(paste(dirwd, "media-beritabali-reklamasitelukbenoa.csv", sep = ''), col_names = FALSE)
tb_balipost <- read_csv(paste(dirwd, "media-balipost-reklamasitelukbenoa.csv", sep = ''), col_names = FALSE)

# menggabungkan
tb_media <- bind_rows(tb_tempo %>%
                        mutate(sumber = "tempo"), 
                      tb_kompas %>%
                        mutate(sumber = "kompas"), 
                      tb_balipost %>%
                        mutate(sumber = "balipost"),
                      tb_beritabali %>%
                        mutate(sumber = "beritabali")
                      )

rm(tb_balipost, tb_beritabali, tb_kompas, tb_tempo)

colnames(tb_media) <- c("tanggal", "judul", "konten", "media")


konten_asli <- as.data.frame(tb_media$konten)

# check duplicates
tb_media <- tb_media %>%
  mutate(duplicate = duplicated(konten))

#tb_media <- tb_media %>%
  filter(duplicate == FALSE)

# Eksplorasi 1 ----
tb_media %>%
  group_by(media) %>%
  count(media, sort = TRUE) %>%
  ggplot(aes(reorder(media, n), n)) +
  geom_col() + 
  ggtitle("Reklamasi Teluk Benoa dalam Media") +
  labs(x = "Media", y = "Jumlah Berita")

# Wrangling ----
tb_media$tanggal <- as.Date(tb_media$tanggal)

distribusi <- tb_media %>%
  select(tanggal, media) %>% 
  separate(tanggal, into = c("tahun", "bulan", "tanggal"), sep = "-") %>%
  group_by(tahun) %>%
  count(media)

distribusi %>%
  ggplot(aes(reorder(media, n), n, fill = tahun)) + geom_col() +
  ggtitle("Reklamasi Teluk Benoa dalam Media") +
  labs(x = "Media", y = "Jumlah Berita")

ggplot(distribusi, aes(x=tahun, y=n, fill=media, width = 0.7))+
  geom_histogram(stat="identity",position="dodge", show.legend = TRUE, binwidth = 50) +
  labs(x = NULL, y = "Jumlah Berita")

rm(distribusi)  

# cleaning $konten ----

tb_media$konten <- gsub("\\bTEMPO.CO\\b", '', tb_media$konten)
tb_media$konten <- gsub("\\bKOMPAS.com\\b", '', tb_media$konten)
tb_media$konten <- gsub("\\bBALIPOST.com\\b", '', tb_media$konten)
tb_media$konten <- gsub("\\bBeritabali.com\\b", '', tb_media$konten)
tb_media$konten <- gsub("\\bBeritaBali.com\\b", '', tb_media$konten)

head(n = 2, tb_media$konten)

# punct and symbol normalisation ----
tb_media$konten <- replace_white(tb_media$konten) # replacing white space
tb_media$konten <- add_comma_space(tb_media$konten)
tb_media$konten <- replace_symbol(tb_media$konten)
tb_media$konten <- replace_non_ascii(tb_media$konten, remove.nonconverted = TRUE)

#tb_media$konten <- gsub("((?:\b| )?([.,:;!?()]+)(?: |\b)?)", " \\1 ", tb_media$konten, perl=T)
tb_media$konten <- replace_white(tb_media$konten) # replacing white space

# mengganti - menjadi to untuk tangal
tb_media$konten <- tb_media$konten <- gsub("\\b-\\b", ' to ', tb_media$konten)
head(n = 2, tb_media$konten)
tb_media$konten <- tb_media$konten <- gsub("\\b/\\b", ' ', tb_media$konten)
# remove punct
tb_media$konten <- strip(tb_media$konten, char.keep = c("/"), digit.remove = FALSE, apostrophe.remove = TRUE, lower.case = FALSE)
# replacing number with chr
tb_media$konten <- replace_number(tb_media$konten, num.paste = TRUE, remove = FALSE)

# judul ----
tb_media$judul <- add_comma_space(tb_media$judul)
tb_media$judul <- replace_symbol(tb_media$judul)
tb_media$judul <- replace_non_ascii(tb_media$judul, remove.nonconverted = TRUE)
tb_media$judul <- replace_white(tb_media$judul) # replacing white space
tb_media$judul <- strip(tb_media$judul, char.keep = c("-", "/"), digit.remove = FALSE, apostrophe.remove = TRUE, lower.case = FALSE)

head(n = 2, tb_media$konten)

