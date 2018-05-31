library(tidyverse)
library(tidytext)
library(ggplot2)


# daftar petisi
dirwd <- paste(getwd(),"/wrangled data proj-3/",sep='')

net_telukBenua <- read_csv(paste(dirwd,"change-net.csv",sep=''), col_names = TRUE)

names(net_telukBenua)

net_telukBenua %>%
  select(Label, betweenesscentrality, closnesscentrality) %>%
  arrange(desc(closnesscentrality)) %>%
  filter(!str_detect(Label, 'JRX_SID')) %>%
  filter(!str_detect(Label, 'ForBALI13')) %>%
  filter(!str_detect(Label, 'jokowi')) %>%
  filter(!str_detect(Label, 'gendovara')) %>%
  filter(!str_detect(Label, 'SID_Official')) %>%
  filter(!str_detect(Label, 'SBYudhoyono')) %>%
  filter(!str_detect(Label, 'xRMBLx')) %>%
  filter(!str_detect(Label, 'ERX_BASTARD')) %>%
  filter(!str_detect(Label, 'BOBBYBIKUL')) %>%
  head(n = 30) %>%
  ggplot(aes(x = reorder(Label, closnesscentrality), y = closnesscentrality)) + 
  geom_col() +
  coord_flip()