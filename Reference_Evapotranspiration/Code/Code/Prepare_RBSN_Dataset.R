
# Initial Setting -----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "stringr")
library(package = "ggplot2")
library(package = "tcltk")
library(package = "pbapply")

# Load My Functions
reticulate::use_python(python = "C:/Users/pooya/anaconda3/python.exe")
source(file = "Code/func.R")

# Select Folder To Save Output Files
if (exists('utils::choose.dir')) {
  Folder.Output <-
    choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  Folder.Output <-
    tcltk::tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

# Convert Gregorian Date to Shamsi Date -------------------------------------------------------

data_daily <- readRDS(file = choose.files(caption = "Select *.rds File"))

## Day Critical
DC = 25

## Create Base Date
StartDate  = min(data_daily$date) %>% as.character()
EndDate    = max(data_daily$date) %>% as.character()
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
  left_join(y = BaseDate, by = 'date') %>% 
  mutate(STNID = paste0(as.character(station_id), "0-99999"),
         YEAR = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                       FUN = "[",
                       1) %>% as.numeric(),
         MONTH = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                        FUN = "[",
                        2) %>% as.numeric(),
         DAY = sapply(X = str_split(string = Shamsi_Date, pattern = "-"),
                      FUN = "[",
                      3) %>% as.numeric())

saveRDS(object = data_daily,
        file = paste0(Folder.Output, "/", "RBSN_Dataset_Khorasan_Daily_1959_2018.rds"))
