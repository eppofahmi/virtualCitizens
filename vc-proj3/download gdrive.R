# Download gdrvive file in r

library(googledrive)
library(tidyverse)

id0 <- "1RAalTLYhpprDCpE_wos48ymufN8mjMG_" #twit-mention-change.csv 
id1 <- "11WTedaaE79EtT4vOGNU2vfG7xgcLtJcE" # twit-tagar-balitolakreklamasi.csv
id2 <- "1984jmFur63PmYvoC8Ab6P2iwfUQYVp71" # twit-tagar-balinotforsale.csv


twit_to_change <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id0))
btr_raw <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id1))
bns_raw <- read_csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id2))

glimpse(btr_raw)


to_change <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id0))

my_grdrive <- drive_find(n_max = 50)

id_graph <- "1mEOcrRI0UaQyIE_ktFV-nf1_Vm-kL8hr"

net_tb <- sprintf("https://docs.google.com/uc?id=%s&export=download", id_graph)
