# skrip ini digunakan untuk menyiapkan data berupa berita yang diambil dari detik.com dengan kata kunci transportasi online. 

library(textclean)
library(tidytext)
library(lubridate)
library(tidyverse)
library(tm)

dirwd <- paste(getwd(),"/wrangled data proj-2/",sep='')
detik_gojek <- read_csv(paste(dirwd, "media-detik-transportasionline.csv", sep = ''), col_names = FALSE)

