
rm(list = ls())


library(package = "dplyr")


data <- read.csv(file = "Result/RefET/ETo_RBSN_RefET_Monthly_Shamsi.csv") %>% 
    filter(STNID %in% c("407440-99999", "407450-99999")) %>% 
    filter(Shamsi_Year >= 1368 & Shamsi_Year <= 1396) %>% 
    select(STNID, Shamsi_Year, Shamsi_Month, FAO56_PM, HS, PRECIP)




data <- extracted_value %>% 
    filter(STNID %in% c("407440-99999", "407450-99999")) %>% 
    filter(YEAR >= 1989 & YEAR <= 2018) %>% 
    dplyr::select(STNID, YEAR, MONTH, PET)


data_yearly <- data %>% 
    group_by(STNID, YEAR) %>% 
    summarise(PET = sum(PET, na.rm = TRUE)) %>% 
    as.data.frame()
    
a_m <- data %>% 
    filter(STNID == "407450-99999") 

a_y <- data_yearly %>% 
    filter(STNID == "407450-99999") 
    
data %>% 
    filter(STNID == "407450-99999") %>% 
    group_by(MONTH) %>% 
    summarise_all(mean)
    
