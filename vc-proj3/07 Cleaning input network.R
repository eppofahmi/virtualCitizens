# cleaning input network analysis

library(tidyverse)
library(tidytext)
library(stringr)

# data ----
dirwd <- paste(getwd(),"/wrangled data proj-3/",sep='')
net_data <- read.csv(paste(dirwd,"net_benoa.csv",sep=''), header = TRUE
                     , stringsAsFactors = FALSE)

# removing raw username that contain only 1 or 2 chr
net_user <- net_data %>%
  unnest_tokens(user, Data, to_lower = FALSE, drop = FALSE) %>%
  mutate(user_chr = nchar(user, keepNA= TRUE))

deleted_user <- net_user %>%
  filter(user_chr <= 2) %>%
  mutate(replaced = "")

library(tm)

user_cleaner <- function(input_text) # nama kolom yang akan dibersihkan
{    
  # create a corpus (type of object expected by tm) and document term matrix
  corpusku <- Corpus(VectorSource(input_text)) # make a corpus object
  stopwords <- as.character(deleted_user$user)
  stopwords <- c(stopwords, stopwords())
  corpusku <- tm_map(corpusku, removeWords, stopwords)
  #removing white space in the begining
  rem_spc_front <- function(x) gsub("^[[:space:]]+", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_front))
  #removing white space at the end
  rem_spc_back <- function(x) gsub("[[:space:]]+$", "", x)
  corpusku <- tm_map(corpusku, content_transformer(rem_spc_back))
  data <- data.frame(Data=sapply(corpusku, identity),stringsAsFactors=F)
}

net_data <- user_cleaner(net_data$Data)

# take rows that have 2 or more user
net_data$user_count <- sapply(net_data$Data, function(x) length(unlist(strsplit(as.character(x), "\\S+"))))

net_data <- net_data %>%
  filter(user_count >= 2)

# normalisasi username ----
# hal ini dilakukan untuk mengatasi twit yang mungkin tidak sengaja salah mengetik username yang dimaksud. Misalnya seharusnya @JRX_SID dengan huruf kapital namun menulis @jrx_sid. Normaliasi hanya dilakukan pada 20 akun paling sering terlibat dengan asumsi akun-akun tersebut akan memberikan banyak pengaruh dalam hasil SNA. Selain dua puluh akun, normalisasi juga dilkukan pada akun aktor/lembaga terkenal/populer seperti presiden dan dpr

#1. JRX_SID ----
net_data$Data <- gsub("\\bjrx_sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJrx_sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJrx_SID\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrx_SID\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrx_siad\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrx_id\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_S\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_ID\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_SI\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrxsid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bSakaJRX_SID\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_SID_\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJrx_Sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_Sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_SIDBula\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX_SiD\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrx_Sid\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRX\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bjrx\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJRXSID\\b", "JRX_SID", net_data$Data)
net_data$Data <- gsub("\\bJrx\\b", "JRX_SID", net_data$Data)

#2. ForBALI13 ----
net_data$Data <- gsub("\\bforbali13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBali13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForbali13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforBALI13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforBali13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBali\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBALI\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bFORBALI13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForbali\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforBali\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bFORBALI\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBali_13\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali1\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBALI1\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBali2013\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali12\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali13akan\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali14\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bleakBALI_forbali13\\b", "leakBALI_ ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforbali31\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForBAL\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bForB\\b", "ForBALI13", net_data$Data)
net_data$Data <- gsub("\\bforb\\b", "ForBALI13", net_data$Data)

#3. jokowi ----
net_data$Data <- gsub("\\bjokowi_do2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjJokowi_do2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJOKOWI_DO2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJokowi\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjokowi_do\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjokowido2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjokowi_Do2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJokowi_do2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJokowi_Do2\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjokowi_dodo\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bjokowi_\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJokowi_ID\\b", "jokowi", net_data$Data)
net_data$Data <- gsub("\\bJokowi_Ina\\b", "jokowi", net_data$Data)

#4. sby ----
net_data$Data <- gsub("\\bsbyudhoyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSByudhoyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYUDHOYONO\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhoyono0\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhoyonoCR7FOOLED\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhoyno\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bsbyudhoyo\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bsbyudoyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhohyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhoyo\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBY\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYodhyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudhyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bSBYudoyono\\b", "SBYudhoyono", net_data$Data)
net_data$Data <- gsub("\\bsby\\b", "SBYudhoyono", net_data$Data)

#5. gendovara ----
net_data$Data <- gsub("\\bGendovara\\b", "gendovara", net_data$Data)
net_data$Data <- gsub("\\b8rianna_gendovara\\b", "8rianna_ gendovara", net_data$Data)

#6. xRMBLx ----
net_data$Data <- gsub("\\bxrmblx\\b", "xRMBLx", net_data$Data)
net_data$Data <- gsub("\\bXRMBLX\\b", "xRMBLx", net_data$Data)
net_data$Data <- gsub("\\bxRMBLX\\b", "xRMBLx", net_data$Data)

#7. SID_Official ----
net_data$Data <- gsub("\\bsid_official\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_official\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_official\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSid_official\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_OFFICIAl\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_Of\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_Of\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_Offical\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_Officialon\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_Oficial\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_offi\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_oficial\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bsid_oficial\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_OFFICIAL\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_0fficial\\b", "SID_Official", net_data$Data)
net_data$Data <- gsub("\\bSID_\\b", "SID_Official", net_data$Data)

net_data$Data <- gsub("\\bBaliekarock\\b", "baliekarock", net_data$Data)
net_data$Data <- gsub("\\bBALIEKAROCK\\b", "baliekarock", net_data$Data)
net_data$Data <- gsub("\\bBaliEkaRock\\b", "baliekarock", net_data$Data)
net_data$Data <- gsub("\\b8rianna_baliekarock\\b", "baliekarock", net_data$Data)

net_data$Data <- gsub("\\bbobbybikul\\b", "BOBBYBIKUL", net_data$Data)
net_data$Data <- gsub("\\bBobbybikul\\b", "BOBBYBIKUL", net_data$Data)
net_data$Data <- gsub("\\bBobbyBikul\\b", "BOBBYBIKUL", net_data$Data)
net_data$Data <- gsub("\\bBobbyBiKul\\b", "BOBBYBIKUL", net_data$Data)

net_data$Data <- gsub("\\bBobbyalcoholic\\b", "bobbyalcoholic", net_data$Data)
net_data$Data <- gsub("\\bBobbyAlcoholic\\b", "bobbyalcoholic", net_data$Data)
net_data$Data <- gsub("\\bbobbyalcoholicrider\\b", "bobbyalcoholic", net_data$Data)
net_data$Data <- gsub("\\berx_bastard\\b", "ERX_BASTARD", net_data$Data)

net_data$Data <- gsub("\\b8rianna_punkrocklowrider\\b", "8rianna_ punkrocklowrider", net_data$Data)
net_data$Data <- gsub("\\b8rianna_Selamat\\b", "8rianna_", net_data$Data)
net_data$Data <- gsub("\\b8rianna_baliekarock\\b", "8rianna_ baliekarock", net_data$Data)
net_data$Data <- gsub("\\b8rianna\\b", "8rianna_", net_data$Data)
net_data$Data <- gsub("\\b8riana_\\b", "8rianna_", net_data$Data)

net_data$Data <- gsub("\\bAnak_alam\\b", "anak_alam", net_data$Data)
net_data$Data <- gsub("\\bAnak_Alam\\b", "anak_alam", net_data$Data)

net_data$Data <- gsub("\\bleakbali_\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bLeakBALI_\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakBALI_Yen\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakBALI_selamat\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakBali_\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakbali_s\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakBALI\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleak_bali4\\b", "leakBALI_", net_data$Data)
net_data$Data <- gsub("\\bleakBali_\\b", "leakBALI_", net_data$Data)



# rechecking username ----
net_user <- net_data %>%
  unnest_tokens(user, Data, to_lower = FALSE, drop = FALSE) %>%
  count(user, sort = TRUE) %>%
  mutate(user_chr = nchar(user, keepNA= TRUE))

# menyimpan file 
net_data <- net_data %>%
  select(Data)

write_csv(net_data, path = "wrangled data proj-3/net_tb_clean.csv")

# create graph file ----
library(igraph)
library("rgexf")

dataSet1 <- net_data %>%
  select(Data) %>%
  unnest_tokens(bigram, Data, token = "ngrams", n = 2, to_lower = FALSE) %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

dataSet2 <- net_data %>%
  select(Data) %>%
  unnest_tokens(bigram, Data, token = "ngrams", n = 2, to_lower = FALSE) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

graph_file <- simplify(graph.data.frame(dataSet2, directed=FALSE))

# Create a dataframe nodes: 1st column - node ID, 2nd column -node name
nodes_df <- data.frame(ID = c(1:vcount(graph_file)), NAME = V(graph_file)$name)

# Create a dataframe edges: 1st column - source node ID, 2nd column -target node ID
edges_df <- as.data.frame(get.edges(graph_file, c(1:ecount(graph_file))))

# save and bring to gephi
write.gexf(nodes = nodes_df, edges = edges_df, defaultedgetype = "undirected", output = "net_teluk_benua_test.gexf")
