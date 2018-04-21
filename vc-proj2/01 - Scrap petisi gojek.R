library(rvest)
library(purrr)
library(dplyr)

## geting the data

url_base <- "https://www.change.org/search?q=gojek&offset=%d"

map_df(c(0,10,20,30,40), function(i) {
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

## data cleaning ----
library(tidyverse)

gojek_petisi <- tes_chang %>%
  separate(date, into = c('non', 'date'), sep = 7)
gojek_petisi <- gojek_petisi %>%
  select(title, date, jml_pendukung = suporter, terpetisi = target, 
         inisiator, link = links)
View(gojek_petisi)
# 9.643 pendukung
gojek_petisi <- gojek_petisi %>%
  separate(jml_pendukung, into = c("juml_pendukung", "non"), sep = " ")
gojek_petisi <- a %>%
  select(date, title, jml_pendukung = juml_pendukung, terpetisi, inisiator, link)

gojek_petisi$jml_pendukung <- gsub('[[:punct:] ]+','', gojek_petisi$jml_pendukung)

gojek_petisi$jml_pendukung <- as.integer(gojek_petisi$jml_pendukung)

library(lubridate)

gojek_petisi$date <- gsub("Des", "Dec", gojek_petisi$date)
gojek_petisi$date <- gsub("Agt", "Aug", gojek_petisi$date)
gojek_petisi$date <- gsub("Okt", "Oct", gojek_petisi$date)
gojek_petisi$date <- gsub("Mei", "May", gojek_petisi$date)

gojek_petisi$date <- dmy(gojek_petisi$date)

write_csv(gojek_petisi, path = "wrangled data proj-2/daftar petisi ttg gojek.csv")