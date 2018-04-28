library(rvest)
library(purrr)
library(dplyr)

# seacrhing urls 
# 423 hasil pencarian

url_base <- "https://www.change.org/search?q=benoa&offset=%d"

map_df(c(0,10), function(i) {
  cat(".")
  page <- read_html(sprintf(url_base, i))
  data.frame(title = html_text(html_nodes(page, ".xs-mbs")), 
             date = html_text(html_nodes(page, ".symbol-clock+ span")),
             suporter = html_text(html_nodes(page, ".symbol-supporters+ span")), 
             target = html_text(html_nodes(page, ".xs-mbn .type-s")),
             inisiator = html_text(html_nodes(page, ".list-rule") %>% 
                                     html_nodes("div .type-ellipsis") %>%
                                     html_nodes(".type-ellipsis strong")),
             link = data.frame(links = html_nodes(page, ".list-rule") %>%
                                 html_nodes("a") %>%
                                 html_attr("href")),
             
             stringsAsFactors = FALSE)
  
}) -> tes_chang

# Cleaning
## data cleaning ----
library(tidyverse)

benoa_petisi <- tes_chang %>%
  separate(date, into = c('non', 'date'), sep = 7)
benoa_petisi <- benoa_petisi %>%
  select(title, date, jml_pendukung = suporter, terpetisi = target, 
         inisiator, link = links)

# 9.643 pendukung
benoa_petisi <- benoa_petisi %>%
  separate(jml_pendukung, into = c("juml_pendukung", "non"), sep = " ")


benoa_petisi <- benoa_petisi %>%
  select(date, title, jml_pendukung = juml_pendukung, terpetisi, inisiator, link)

benoa_petisi$jml_pendukung <- gsub('[[:punct:] ]+','', benoa_petisi$jml_pendukung)

benoa_petisi$jml_pendukung <- as.integer(benoa_petisi$jml_pendukung)

library(lubridate)

benoa_petisi$date <- gsub("Des", "Dec", benoa_petisi$date)
benoa_petisi$date <- gsub("Agt", "Aug", benoa_petisi$date)
benoa_petisi$date <- gsub("Okt", "Oct", benoa_petisi$date)
benoa_petisi$date <- gsub("Mei", "May", benoa_petisi$date)

benoa_petisi$date <- dmy(benoa_petisi$date)

write_csv(benoa_petisi, path = "wrangled data proj-3/daftar petisi benoa.csv")
