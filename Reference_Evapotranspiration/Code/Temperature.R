
# Initial Setting -----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "ggplot2")
library(package = "clipr")
library(package = "tidyr")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407450-99999")

RBSN_PERSIAN <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST) %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)


# Part 01

data <- RBSN_PERSIAN %>%
select(YEAR, MONTH, DAY, tmax) %>% 
    spread(key = MONTH, value = tmax)

write_clip(content = data)

data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, tm) %>% 
    spread(key = MONTH, value = tm)

write_clip(content = data)

data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, tmin) %>% 
    spread(key = MONTH, value = tmin)

write_clip(content = data)


# Part 02

data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, tmax, tm, tmin) %>% 
    group_by(MONTH) %>% 
    summarise(tmax = mean(x = tmax, na.rm = TRUE),
              tm = mean(x = tm, na.rm = TRUE),
              tmin = mean(x = tmin, na.rm = TRUE))

write_clip(content = data)

# Part 03

data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, tmax, tm, tmin) %>% 
    group_by(YEAR) %>% 
    summarise(tmax = ifelse(test = sum(is.na(tmax)) > 60,
                            yes = NA,
                            no = mean(x = tmax, na.rm = TRUE)),
              
              tm = ifelse(test = sum(is.na(tm)) > 60,
                          yes = NA,
                          no = mean(x = tm, na.rm = TRUE)),
              
              tmin = ifelse(test = sum(is.na(tmin)) > 60,
                            yes = NA,
                            no = mean(x = tmin, na.rm = TRUE)))

write_clip(content = data)
