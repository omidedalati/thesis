
rm(list = ls())

library(package = "dplyr")
library(package = "outliers")
library(package = "tsoutliers")
library(package = "snht")
library(package = "trend")
library(package = "randtests")

RBSN <- readRDS(file = "Result/ETo_RBSN_Dataset_Monthly_1338_1397.rds") %>% 
    filter(STNID %in% c("407440-99999", "407450-99999")) %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)

var = "PRCP"

for (st in c("407440-99999", "407450-99999")) {
    
    df <- RBSN %>% 
        filter(STNID == st)
    
    var_ts <- ts(data = df[var])
    
    print(runs.test(x = var_ts))
    
    # print(x = tsoutliers::tso(y = var_ts, 
    #                           types = c("AO", "LS", "TC", "IO", "SLS"),
    #                           maxit.iloop = 20,
    #                           maxit.oloop = 20))   
    
}

runs.test(x = var_ts)
