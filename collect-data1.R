library(rvest)
library(purrr)
library(dplyr)

url_base <- "https://www.change.org/search?q=gojek&offset=%d"

map_df(c(0,10,20,30,40), function(i) {
  cat(".")
  page <- read_html(sprintf(url_base, i))
  data.frame(judul = html_text(html_nodes(page, ".xs-mbs")), 
             tanggal = html_text(html_nodes(page, ".symbol-clock+ span")),
             pendukung = html_text(html_nodes(page, ".symbol-supporters+ span")), 
             target = html_text(html_nodes(page, ".xs-mbn .type-s")),
             pengusul = html_text(html_nodes(page, ".list-rule") %>% 
                                    html_nodes("div .type-ellipsis") %>%
                                    html_nodes(".type-ellipsis strong")),
             link = data.frame(links = html_nodes(page, ".list-rule") %>%
                                 html_nodes("a") %>%
                                 html_attr("href")),
             
             stringsAsFactors = FALSE)
  
}) -> tes_chang