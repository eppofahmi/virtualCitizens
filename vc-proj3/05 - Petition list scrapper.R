library(rvest)
library(purrr)
library(dplyr)

# seacrhing urls 
# 423 hasil pencarian

# urls ---- 
url_base <- "https://www.change.org/search?q=uu&offset=%d"

# scrap ----
map_df(c(seq(from = 0, to = 490, by = 10)), function(i) {
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

