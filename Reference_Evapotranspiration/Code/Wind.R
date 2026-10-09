
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
select(YEAR, MONTH, DAY, ffm) %>% 
    spread(key = MONTH, value = ffm)

write_clip(content = data)

# Part 02
data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, ffm) %>% 
    group_by(YEAR) %>% 
    summarise(ffm = ifelse(test = sum(is.na(ffm)) > 60,
                            yes = NA,
                            no = mean(x = ffm, na.rm = TRUE)))

write_clip(content = data)

# Part 03: Max Wind Rose

library(package = "clifro")
library(package = "rWind")
library(package = "openair")

data <- RBSN_PERSIAN %>%
    select(YEAR, MONTH, DAY, ff_max, dd_max) %>% 
    rename(ws = ff_max, wd = dd_max)

windRose(mydata = data, cols = "default")

write_clip(content = data)
