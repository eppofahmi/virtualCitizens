# media online di ambil dari beritabali.com, balipost.com, tempo.co, dan kompas.com

# lib ----
library(tidyverse)
library(lubridate)
library(ggplot2)

# data ----
dirwd <- paste(getwd(),"/wrangled data proj-2/",sep='')
gojek_tempo <- read_csv(paste(dirwd, "media-tempo-transportasionline.csv", sep = ''), col_names = FALSE)
gojek_kompas <- read_csv(paste(dirwd, "media-kompas-transportasionline.csv", sep = ''), col_names = FALSE)

# menggabungkan
gojek_media <- bind_rows(gojek_tempo %>%
                        mutate(sumber = "tempo"), 
                      gojek_kompas %>%
                        mutate(sumber = "kompas"))

rm(gojek_kompas, gojek_tempo)

colnames(gojek_media) <- c("tanggal", "judul", "konten", "media")

# check duplicates
gojek_media <- gojek_media %>%
  mutate(duplicate = duplicated(konten))

gojek_media <- gojek_media %>%
  filter(duplicate == FALSE)

# jumlah ----
gojek_media %>%
  group_by(media) %>%
  count(media, sort = TRUE) %>%
  ggplot(aes(reorder(media, n), n)) +
  geom_col() + 
  ggtitle("Reklamasi Teluk Benoa dalam Media") +
  labs(x = "Media", y = "Jumlah Berita")

# tanggal ----
gojek_media$tanggal <- as.Date(gojek_media$tanggal)

distribusi <- gojek_media %>%
  select(tanggal, media) %>% 
  separate(tanggal, into = c("tahun", "bulan", "tanggal"), sep = "-") %>%
  group_by(tahun) %>%
  count(media)

distribusi %>%
  ggplot(aes(reorder(media, n), n, fill = tahun)) + geom_col() +
  ggtitle("Transportasi Online dalam Media") +
  labs(x = "Media", y = "Jumlah Berita")
