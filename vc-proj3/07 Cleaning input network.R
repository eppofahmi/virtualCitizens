# cleaning input network analysis

library(tidyverse)
library(tidytext)
library(stringr)

# data ----
dirwd <- paste(getwd(),"/wrangled data proj-3/",sep='')
net_tb <- read_csv(paste(dirwd,"net_benua.csv",sep=''), col_names = TRUE)

# normalisasi username ----
# hal ini dilakukan untuk mengatasi twit yang mungkin tidak sengaja salah mengetik username yang dimaksud. Misalnya seharusnya @JRX_SID dengan huruf kapital namun menulis @jrx_sid. Normaliasi hanya dilakukan pada 20 akun paling sering terlibat dengan asumsi akun-akun tersebut akan memberikan banyak pengaruh dalam hasil SNA. Selain dua puluh akun, normalisasi juga dilkukan pada akun aktor/lembaga terkenal/populer seperti presiden dan dpr

#1. JRX_SID ----
net_tb$Data <- gsub("\\bjrx_sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJrx_sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJrx_SID\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrx_SID\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrx_siad\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrx_id\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_S\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_ID\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_SI\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrxsid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bSakaJRX_SID\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_SID_\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJrx_Sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_Sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_SIDBula\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX_SiD\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrx_Sid\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRX\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bjrx\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJRXSID\\b", "JRX_SID", net_tb$Data)
net_tb$Data <- gsub("\\bJrx\\b", "JRX_SID", net_tb$Data)

#2. ForBALI13 ----
net_tb$Data <- gsub("\\bforbali13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBali13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForbali13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforBALI13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforBali13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBali\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBALI\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bFORBALI13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForbali\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforBali\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bFORBALI\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBali_13\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali1\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBALI1\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBali2013\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali12\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali13akan\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali14\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bleakBALI_forbali13\\b", "leakBALI_ ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforbali31\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForBAL\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bForB\\b", "ForBALI13", net_tb$Data)
net_tb$Data <- gsub("\\bforb\\b", "ForBALI13", net_tb$Data)

#3. jokowi ----
net_tb$Data <- gsub("\\bjokowi_do2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjJokowi_do2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJOKOWI_DO2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJokowi\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjokowi_do\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjokowido2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjokowi_Do2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJokowi_do2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJokowi_Do2\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjokowi_dodo\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bjokowi_\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJokowi_ID\\b", "jokowi", net_tb$Data)
net_tb$Data <- gsub("\\bJokowi_Ina\\b", "jokowi", net_tb$Data)

#4. sby ----
net_tb$Data <- gsub("\\bsbyudhoyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSByudhoyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYUDHOYONO\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhoyono0\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhoyonoCR7FOOLED\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhoyno\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bsbyudhoyo\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bsbyudoyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhohyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhoyo\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBY\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYodhyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudhyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bSBYudoyono\\b", "SBYudhoyono", net_tb$Data)
net_tb$Data <- gsub("\\bsby\\b", "SBYudhoyono", net_tb$Data)

#5. gendovara ----
net_tb$Data <- gsub("\\bGendovara\\b", "gendovara", net_tb$Data)
net_tb$Data <- gsub("\\b8rianna_gendovara\\b", "8rianna_ gendovara", net_tb$Data)

#6. xRMBLx ----
net_tb$Data <- gsub("\\bxrmblx\\b", "xRMBLx", net_tb$Data)
net_tb$Data <- gsub("\\bXRMBLX\\b", "xRMBLx", net_tb$Data)
net_tb$Data <- gsub("\\bxRMBLX\\b", "xRMBLx", net_tb$Data)

#7. SID_Official ----
net_tb$Data <- gsub("\\bsid_official\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_official\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_official\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSid_official\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_OFFICIAl\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_Of\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_Of\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_Offical\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_Officialon\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_Oficial\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_offi\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_oficial\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bsid_oficial\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_OFFICIAL\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_0fficial\\b", "SID_Official", net_tb$Data)
net_tb$Data <- gsub("\\bSID_\\b", "SID_Official", net_tb$Data)

net_tb$Data <- gsub("\\bBaliekarock\\b", "baliekarock", net_tb$Data)
net_tb$Data <- gsub("\\bBALIEKAROCK\\b", "baliekarock", net_tb$Data)
net_tb$Data <- gsub("\\bBaliEkaRock\\b", "baliekarock", net_tb$Data)
net_tb$Data <- gsub("\\b8rianna_baliekarock\\b", "baliekarock", net_tb$Data)

net_tb$Data <- gsub("\\bbobbybikul\\b", "BOBBYBIKUL", net_tb$Data)
net_tb$Data <- gsub("\\bBobbybikul\\b", "BOBBYBIKUL", net_tb$Data)
net_tb$Data <- gsub("\\bBobbyBikul\\b", "BOBBYBIKUL", net_tb$Data)
net_tb$Data <- gsub("\\bBobbyBiKul\\b", "BOBBYBIKUL", net_tb$Data)

net_tb$Data <- gsub("\\bBobbyalcoholic\\b", "bobbyalcoholic", net_tb$Data)
net_tb$Data <- gsub("\\bBobbyAlcoholic\\b", "bobbyalcoholic", net_tb$Data)
net_tb$Data <- gsub("\\bbobbyalcoholicrider\\b", "bobbyalcoholic", net_tb$Data)
net_tb$Data <- gsub("\\berx_bastard\\b", "ERX_BASTARD", net_tb$Data)
net_tb$Data <- gsub("\\bBobbyBiKul\\b", "ERX_BASTARD", net_tb$Data)

net_tb$Data <- gsub("\\b8rianna_punkrocklowrider\\b", "8rianna_ punkrocklowrider", net_tb$Data)
net_tb$Data <- gsub("\\b8rianna_Selamat\\b", "8rianna_", net_tb$Data)
net_tb$Data <- gsub("\\b8rianna_baliekarock\\b", "8rianna_ baliekarock", net_tb$Data)
net_tb$Data <- gsub("\\b8rianna\\b", "8rianna_", net_tb$Data)
net_tb$Data <- gsub("\\b8riana_\\b", "8rianna_", net_tb$Data)

net_tb$Data <- gsub("\\bAnak_alam\\b", "anak_alam", net_tb$Data)
net_tb$Data <- gsub("\\bAnak_Alam\\b", "anak_alam", net_tb$Data)

net_tb$Data <- gsub("\\bleakbali_\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bLeakBALI_\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakBALI_Yen\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakBALI_selamat\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakBali_\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakbali_s\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakBALI\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleak_bali4\\b", "leakBALI_", net_tb$Data)
net_tb$Data <- gsub("\\bleakBali_\\b", "leakBALI_", net_tb$Data)

# total user ----
net_data <- net_tb %>%
  unnest_tokens(user, Data, to_lower = FALSE) %>%
  count(user, sort = TRUE) %>% 
  mutate(user_chr = nchar(user, keepNA= TRUE))

# data yang digunakan untuk network analisis difilter dari twit yang melibatkan minimal 2 akun yaitu username pengirim dan satu username dalam konten twit. 

a <- net_tb %>%
  filter(user_count >= 2) %>%
  select(Data)

# menyimpan file 
write_csv(a, path = "wrangled data proj-3/net benua clean.csv")