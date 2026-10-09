
rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "ggplot2")
library(package = "zoo")
library(package = "clipr")
library(package = "tidyr")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407440-99999")

RBSN_PERSIAN <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST) %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)

# Part 01

data <- RBSN_PERSIAN %>% 
    group_by(YEAR) %>% 
    summarise(Precipitation = mean(x = rrr24, na.rm = TRUE) * 365) %>% 
    mutate(Average = mean(x = Precipitation, na.rm = TRUE),
           Mo_Ave_3 = rollmean(x = Precipitation, k = 3, fill = list(NA, NULL, NA)),
           Mo_Ave_5 = rollmean(x = Precipitation, k = 5, fill = list(NA, NULL, NA)))

write_clip(content = data)

# Part 02

data <- RBSN_PERSIAN %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(Precipitation = mean(x = rrr24, na.rm = TRUE)) %>% 
    mutate(Precipitation = Precipitation * ifelse(test = MONTH <= 6,
                                                  yes = 31,
                                                  no = ifelse(test = MONTH == 12,
                                                              yes = 28,
                                                              no = 30))) %>% 
    arrange(MONTH) %>% 
    spread(MONTH, Precipitation)

write_clip(content = data)

