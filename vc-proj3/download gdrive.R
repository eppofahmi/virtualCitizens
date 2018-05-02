# Download gdrvive file in r

library(googledrive)
library(tidyverse)

id0 <- "1RAalTLYhpprDCpE_wos48ymufN8mjMG_" #twit-mention-change.csv 
id1 <- "0B-wuZ2XMFIBUd09Ob0pKVkRzQTA" # twit-tagar-balitolakreklamasi.csv
id2 <- "1984jmFur63PmYvoC8Ab6P2iwfUQYVp71" # twit-tagar-balinotforsale.csv

to_change <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id))


drive_find(n_max = 50)
