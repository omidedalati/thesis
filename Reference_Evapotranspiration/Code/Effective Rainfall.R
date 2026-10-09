
rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "ggplot2")
library(package = "zoo")
library(package = "clipr")
library(package = "tidyr")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407450-99999")

RBSN_PERSIAN <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST) %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)

# Part 01

data <- RBSN_PERSIAN %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(Precipitation = mean(x = rrr24, na.rm = TRUE)) %>% 
    mutate(Precipitation = Precipitation * ifelse(test = MONTH <= 6,
                                                  yes = 31,
                                                  no = ifelse(test = MONTH == 12,
                                                              yes = 29,
                                                              no = 30))) %>% 
    mutate(Effective_Rainfall = ifelse(test = ifelse(test = Precipitation <= 70,
                                                     yes = 0.6 * Precipitation - 10,
                                                     no = 0.8 * Precipitation - 24) < 0,
                                       yes = 0,
                                       no = ifelse(test = Precipitation <= 70,
                                                   yes = 0.6 * Precipitation - 10,
                                                   no = 0.8 * Precipitation - 24))) %>% 
    select(YEAR, MONTH, Effective_Rainfall) %>% 
    arrange(MONTH) %>% 
    spread(MONTH, Effective_Rainfall)

write_clip(content = data)

data <- RBSN_PERSIAN %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(Precipitation = mean(x = rrr24, na.rm = TRUE)) %>% 
    mutate(Precipitation = Precipitation * ifelse(test = MONTH <= 6,
                                                  yes = 31,
                                                  no = ifelse(test = MONTH == 12,
                                                              yes = 29,
                                                              no = 30))) %>% 
    group_by(MONTH) %>% 
    summarise(Precipitation = mean(x = Precipitation, na.rm = TRUE)) %>% 
    mutate(Effective_Rainfall = ifelse(test = ifelse(test = Precipitation <= 70,
                                                     yes = 0.6 * Precipitation - 10,
                                                     no = 0.8 * Precipitation - 24) < 0,
                                       yes = 0,
                                       no = ifelse(test = Precipitation <= 70,
                                                   yes = 0.6 * Precipitation - 10,
                                                   no = 0.8 * Precipitation - 24)))

write_clip(content = data)
