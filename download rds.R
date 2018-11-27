library(curl)

# rangkuman
rangkuman <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/rangkuman.rds?raw=true")))

# eksplorasi
data_eksplorasi <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/data_eksplorasi.rds?raw=true")))

# timeseries
ts_change <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/ts_change.rds?raw=true")))
ts_gojek <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/ts_gojek.rds?raw=true")))

## table output
data_vircit <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/data_vircit.rds?raw=true")))

## semantic network
data_net <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/semantic_network.rds?raw=true")))

## social network
net_user <- readRDS(gzcon(url("https://github.com/eppofahmi/virtualCitizens/blob/master/dashboardData/net_user.rds?raw=true")))
