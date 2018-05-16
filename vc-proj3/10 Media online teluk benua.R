# media online di ambil dari beritabali.com, balipost.com, tempo.co, dan kompas.com

# lib ----
library(tidyverse)
library(lubridate)
library(ggplot2)

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

# check duplicates
tb_media <- tb_media %>%
  mutate(duplicate = duplicated(konten))

tb_media <- tb_media %>%
  filter(duplicate == FALSE)

# jumlah ----
tb_media %>%
  group_by(media) %>%
  count(media, sort = TRUE) %>%
  ggplot(aes(reorder(media, n), n)) +
  geom_col() + 
  ggtitle("Reklamasi Teluk Benoa dalam Media") +
  labs(x = "Media", y = "Jumlah Berita")

# tanggal ----
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
