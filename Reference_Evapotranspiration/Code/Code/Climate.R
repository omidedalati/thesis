
rm(list = ls())

library(package = "dplyr")
library(package = "ClimClass")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407450-99999")

RBSN_PERSIAN <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST) %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)

# Part 01

data <- RBSN_PERSIAN %>% 
    select(YEAR, MONTH, DAY, tm, rrr24) %>% 
    group_by(YEAR) %>% 
    summarise(tm = mean(x = tm, na.rm = TRUE),
              rrr24 = mean(x = rrr24, na.rm = TRUE) * 365)

print(mean(x = data$tm, na.rm = TRUE))
print(mean(x = data$rrr24, na.rm = TRUE))
print(mean(x = data$rrr24, na.rm = TRUE) / (10 + mean(x = data$tm, na.rm = TRUE)))

# Part 02
DC = 20

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

data <- RBSN_PERSIAN %>% 
    select(YEAR, MONTH, DAY, rrr24, tmin, tmax) %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(
        Tx_ATTRIB = ifelse(test = all(is.na(tmax)),
                             yes = NA,
                             no = sum(!is.na(tmax))),
        
        Tx = ifelse(test = Tx_ATTRIB >= DC,
                    yes = mean(x = tmax, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        
        Tn_ATTRIB = ifelse(test = all(is.na(tmin)),
                           yes = NA,
                           no = sum(!is.na(tmin))),
        
        Tn = ifelse(test = Tn_ATTRIB >= DC,
                    yes = mean(x = tmin, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        P_ATTRIB = ifelse(test = all(is.na(rrr24)),
                             yes = NA,
                             no = sum(!is.na(rrr24))),
        
        P = ifelse(test = P_ATTRIB >= DC,
                      yes = mean(x = rrr24, na.rm = TRUE) %>% round(digits = 1),
                      no = NA)) %>%
    left_join(y = Day_Month, by = 'MONTH') %>% 
    
    mutate(P = P * DAY_MONTH) %>% 
    
    rename(year = YEAR, month = MONTH) %>% 
    
    select(year, month, P, Tn, Tx)

climate(series = data, first.yr = 1368, last.yr = 1396, max.perc.missing = 30) %>%
    koeppen_geiger()
