# Sumber data: Twitter
# Parameter pencarian:
# 1. #savegojek
# 2. #savedrivergojek
# 3. #saveojekonline

# Library ----
library(lubridate)
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)

saveojekonline <- read.csv("twit-saveojekonline.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
savegojek <- read.csv("twit-savegojek.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
savedrivergojek <- read.csv("twit-savedrivergojek.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)
twit_cerita <- read.csv("twit-ceritatransonline.csv", sep = ";", header = FALSE, stringsAsFactors = FALSE)

