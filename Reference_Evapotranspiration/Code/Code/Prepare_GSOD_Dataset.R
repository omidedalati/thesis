
# Initial Setting -----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "reticulate")
library(package = "worldmet")
library(package = "GSODR")
library(package = "SPEI")
library(package = "stringr")
library(package = "ggplot2")
library(package = "tcltk")
library(package = "tidyr")
library(package = "pbapply")

## Load My Functions
reticulate::use_python(python = "C:/Users/pooya/anaconda3/python.exe")
source(file = "Code/func.R")

## Select Folder To Save Output Files
if (exists('utils::choose.dir')) {
    Folder.Output <- choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
    Folder.Output <- tcltk::tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

# Select Stations & Download Data -------------------------------------------------------------

## Lat & Long Mashhad Station
lat = 36.235
lon = 59.641

## Find Stations
ISD <- worldmet::getMeta(
    lat = lat,
    lon = lon,
    n = 1,
    end.year = "current",
    plot = FALSE
)

stations = ISD$code

## Select Years
years = 1959:2019

## Download Data
data <- list()

for (i in stations) {
    data[[i]] <- GSODR::get_GSOD(years = years,
                                 station = i)
}

if (length(x = stations) == 1) {
    data <- data[[1]]
} else {
    data <- do.call(what = rbind, args = data)
}

# Data Cleansing ------------------------------------------------------------------------------

## Create Base Date
StartDate  = "1959-01-01"
EndDate    = "2019-12-31"
FormatDate = "%Y-%m-%d"

BaseDate <- base::seq.POSIXt(from = as.POSIXct(x = StartDate,
                                               tz = "GMT",
                                               format = FormatDate),
                             to   = as.POSIXct(x = EndDate,
                                               tz = "GMT",
                                               format = FormatDate),
                             by   = "day") %>% as.data.frame.POSIXct()

names(BaseDate) <- "YEARMODA"

data$YEARMODA = as.POSIXct(x = data$YEARMODA,
                           tz = "GMT",
                           format = "%Y-%m-%d")

df <- list()

for (i in unique(x = data$STNID)) {
    
    tmp <- data %>% dplyr::filter(STNID == i)
    Name <- unique(x = tmp$NAME)
    Lat <- unique(x = tmp$LATITUDE)
    Long <- unique(x = tmp$LONGITUDE)
    Elev <- unique(x = tmp$ELEVATION)
    
    df[[i]] <- tmp %>%
        dplyr::left_join(x = BaseDate, y = ., by = "YEARMODA") %>%
        dplyr::mutate(STNID = i,
                      NAME = Name,
                      LATITUDE = Lat,
                      LONGITUDE = Long,
                      ELEVATION = Elev) %>%
        dplyr::select(YEARMODA, STNID, NAME, LATITUDE, LONGITUDE, ELEVATION, TEMP:RH)
    
}

data_daily <- do.call(what = rbind, args = df)
rownames(data_daily) <- NULL

rm(list = c('data', 'BaseDate', 'df', 'tmp', 'ISD'))

# Convert Gregorian Date to Shamsi Date -------------------------------------------------------

## Create Base Date
StartDate  = min(data_daily$YEARMODA) %>% as.character()
EndDate    = max(data_daily$YEARMODA) %>% as.character()
FormatDate = "%Y-%m-%d"

BaseDate <- base::seq.POSIXt(from = as.POSIXct(x = StartDate,
                                               tz = "GMT",
                                               format = FormatDate),
                             to   = as.POSIXct(x = EndDate,
                                               tz = "GMT",
                                               format = FormatDate),
                             by   = "day") %>% as.data.frame.POSIXct()

names(BaseDate) <- "date"

BaseDate$Shamsi_Date <- pbapply(X = as.data.frame(x = as.character(x = BaseDate$date)),
                                MARGIN = 1,
                                FUN = function(x) Gregorian(x)$persian_string())

data_daily <- data_daily %>%
    left_join(y = BaseDate, by = c('YEARMODA' = 'date')) %>% 
    mutate(YEAR = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                         FUN = "[",
                         1) %>% as.numeric(),
           MONTH = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                          FUN = "[",
                          2) %>% as.numeric(),
           DAY = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                        FUN = "[",
                        3) %>% as.numeric())

saveRDS(object = data_daily,
        file = paste0(Folder.Output, "/", "GSOD_Dataset_Khorasan_Daily_1959_2019.rds"))