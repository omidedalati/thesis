
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

data.daily <- do.call(what = rbind, args = df)
rownames(data.daily) <- NULL

rm(list = c('data', 'BaseDate', 'df', 'tmp', 'ISD'))

# Calculate Monthly Mean - Gregorian ----------------------------------------------------------

## Day Critical
DC = 20

data.monthly <- data.daily %>%
    group_by(STNID, YEAR = year(YEARMODA), MONTH = month(YEARMODA)) %>%
    summarise(
        LAT = unique(x = LATITUDE),
        
        LONG = unique(x = LONGITUDE),
        
        ELEV = unique(x = ELEVATION),
        
        TEMP_ATTRIB = ifelse(test = all(is.na(TEMP)),
                             yes = NA,
                             no = sum(!is.na(TEMP))),
        
        TEMP = ifelse(test = TEMP_ATTRIB >= DC,
                      yes = mean(x = TEMP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        DEWP_ATTRIB = ifelse(test = all(is.na(DEWP)),
                             yes = NA,
                             no = sum(!is.na(DEWP))),
        
        DEWP = ifelse(test = DEWP_ATTRIB >= DC,
                      yes = mean(x = DEWP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        SLP_ATTRIB = ifelse(test = all(is.na(SLP)),
                            yes = NA,
                            no = sum(!is.na(SLP))),
        
        SLP = ifelse(test = SLP_ATTRIB >= DC,
                     yes = mean(x = SLP, na.rm = TRUE) %>% round(digits = 1),
                     no = NA),
        
        STP_ATTRIB = ifelse(test = all(is.na(STP)),
                            yes = NA,
                            no = sum(!is.na(STP))),
        
        STP = ifelse(test = STP_ATTRIB >= DC,
                     yes = mean(x = STP, na.rm = TRUE) %>% round(digits = 1),
                     no = NA),
        
        WDSP_ATTRIB = ifelse(test = all(is.na(WDSP)),
                             yes = NA,
                             no = sum(!is.na(WDSP))),
        
        WDSP = ifelse(test = WDSP_ATTRIB >= DC,
                      yes = mean(x = WDSP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        MXSPD_ATTRIB = ifelse(test = all(is.na(MXSPD)),
                              yes = NA,
                              no = sum(!is.na(MXSPD))),
        
        MXSPD = ifelse(test = MXSPD_ATTRIB >= DC,
                       yes = mean(x = MXSPD, na.rm = TRUE) %>% round(digits = 1),
                       no = NA),
        
        TMAX_ATTRIB = ifelse(test = all(is.na(MAX)),
                             yes = NA,
                             no = sum(!is.na(MAX))),
        
        TMAX = ifelse(test = TMAX_ATTRIB >= DC,
                      yes = mean(x = MAX, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        TMIN_ATTRIB = ifelse(test = all(is.na(MIN)),
                             yes = NA,
                             no = sum(!is.na(MIN))),
        
        TMIN = ifelse(test = TMIN_ATTRIB >= DC,
                      yes = mean(x = MIN, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        PRCP_ATTRIB = ifelse(test = all(is.na(PRCP)),
                             yes = NA,
                             no = sum(!is.na(PRCP))),
        
        PRCP = ifelse(test = PRCP_ATTRIB >= DC,
                      yes = sum(PRCP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),

        EA_ATTRIB = ifelse(test = all(is.na(EA)),
                            yes = NA,
                            no = sum(!is.na(EA))),
        
        EA = ifelse(test = EA_ATTRIB >= DC,
                    yes = mean(x = EA, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        
        ES_ATTRIB = ifelse(test = all(is.na(ES)),
                            yes = NA,
                            no = sum(!is.na(ES))),
        
        ES = ifelse(test = ES_ATTRIB >= DC,
                    yes = mean(x = ES, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        
        RH_ATTRIB = ifelse(test = all(is.na(RH)),
                            yes = NA,
                            no = sum(!is.na(RH))),
        
        RH = ifelse(test = RH_ATTRIB >= DC,
                    yes = mean(x = RH, na.rm = TRUE) %>% round(digits = 1),
                    no = NA)
    )

data.monthly[sapply(data.monthly, is.infinite)] <- NA
data.monthly[sapply(data.monthly, is.nan)] <- NA

# Calculate Monthly Mean - Shamsi -------------------------------------------------------------

## Day Critical
DC = 20 

## TODO : Find A Better Way
data.daily.shamsi <- data.daily

data.daily.shamsi$YEARMODA_SHAMSI <-
    pbapply::pbapply(
        X = as.data.frame(x = as.character(x = data.daily.shamsi$YEARMODA)),
        MARGIN = 1,
        FUN = function(x)
            Gregorian(x)$persian_string()
    )

data.daily.shamsi <- data.daily.shamsi %>%
    mutate(YEAR = sapply(X = str_split(string = YEARMODA_SHAMSI, pattern = "-"),
                         FUN = "[",
                         1) %>% as.numeric(),
           MONTH = sapply(X = str_split(string = YEARMODA_SHAMSI, pattern = "-"),
                          FUN = "[",
                          2) %>% as.numeric(),
           DAY = sapply(X = str_split(string = YEARMODA_SHAMSI, pattern = "-"),
                        FUN = "[",
                        3) %>% as.numeric()
    )

## Calculate Monthly Mean
data.monthly.shamsi <- data.daily.shamsi %>%
    group_by(STNID, YEAR, MONTH) %>%
    summarise(
        LAT = unique(x = LATITUDE),
        
        LONG = unique(x = LONGITUDE),
        
        ELEV = unique(x = ELEVATION),
        
        TEMP_ATTRIB = ifelse(test = all(is.na(TEMP)),
                             yes = NA,
                             no = sum(!is.na(TEMP))),
        
        TEMP = ifelse(test = TEMP_ATTRIB >= DC,
                      yes = mean(x = TEMP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        DEWP_ATTRIB = ifelse(test = all(is.na(DEWP)),
                             yes = NA,
                             no = sum(!is.na(DEWP))),
        
        DEWP = ifelse(test = DEWP_ATTRIB >= DC,
                      yes = mean(x = DEWP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        SLP_ATTRIB = ifelse(test = all(is.na(SLP)),
                            yes = NA,
                            no = sum(!is.na(SLP))),
        
        SLP = ifelse(test = SLP_ATTRIB >= DC,
                     yes = mean(x = SLP, na.rm = TRUE) %>% round(digits = 1),
                     no = NA),
        
        STP_ATTRIB = ifelse(test = all(is.na(STP)),
                            yes = NA,
                            no = sum(!is.na(STP))),
        
        STP = ifelse(test = STP_ATTRIB >= DC,
                     yes = mean(x = STP, na.rm = TRUE) %>% round(digits = 1),
                     no = NA),
        
        WDSP_ATTRIB = ifelse(test = all(is.na(WDSP)),
                             yes = NA,
                             no = sum(!is.na(WDSP))),
        
        WDSP = ifelse(test = WDSP_ATTRIB >= DC,
                      yes = mean(x = WDSP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        MXSPD_ATTRIB = ifelse(test = all(is.na(MXSPD)),
                              yes = NA,
                              no = sum(!is.na(MXSPD))),
        
        MXSPD = ifelse(test = MXSPD_ATTRIB >= DC,
                       yes = mean(x = MXSPD, na.rm = TRUE) %>% round(digits = 1),
                       no = NA),
        
        TMAX_ATTRIB = ifelse(test = all(is.na(MAX)),
                             yes = NA,
                             no = sum(!is.na(MAX))),
        
        TMAX = ifelse(test = TMAX_ATTRIB >= DC,
                      yes = mean(x = MAX, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        TMIN_ATTRIB = ifelse(test = all(is.na(MIN)),
                             yes = NA,
                             no = sum(!is.na(MIN))),
        
        TMIN = ifelse(test = TMIN_ATTRIB >= DC,
                      yes = mean(x = MIN, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        PRCP_ATTRIB = ifelse(test = all(is.na(PRCP)),
                             yes = NA,
                             no = sum(!is.na(PRCP))),
        
        PRCP = ifelse(test = PRCP_ATTRIB >= DC,
                      yes = sum(PRCP, na.rm = TRUE) %>% round(digits = 1),
                      no = NA),
        
        EA_ATTRIB = ifelse(test = all(is.na(EA)),
                           yes = NA,
                           no = sum(!is.na(EA))),
        
        EA = ifelse(test = EA_ATTRIB >= DC,
                    yes = mean(x = EA, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        
        ES_ATTRIB = ifelse(test = all(is.na(ES)),
                           yes = NA,
                           no = sum(!is.na(ES))),
        
        ES = ifelse(test = ES_ATTRIB >= DC,
                    yes = mean(x = ES, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
        
        RH_ATTRIB = ifelse(test = all(is.na(RH)),
                           yes = NA,
                           no = sum(!is.na(RH))),
        
        RH = ifelse(test = RH_ATTRIB >= DC,
                    yes = mean(x = RH, na.rm = TRUE) %>% round(digits = 1),
                    no = NA)
    )

data.monthly.shamsi[sapply(data.monthly.shamsi, is.infinite)] <- NA
data.monthly.shamsi[sapply(data.monthly.shamsi, is.nan)] <- NA

# ############### Data Preparation - Shamsi - Method 1 ###############
# 
# data.monthly.Gregorian <-
#     readRDS(file = "Dataset/GSOD_Monthly_Gregorian.rds")
# 
# # Convert Month Gregorian to Shamsi
# data.monthly.Shamsi <- list()
# 
# for (station in unique(x = data.monthly.Gregorian$STNID)) {
#     df <- data.monthly.Gregorian %>%
#         filter(STNID == station) %>%
#         as.data.frame()
#     
#     STNID <- unique(x = df$STNID) %>% na.omit()
#     LON.GSOD <- unique(x = df$LON.GSOD) %>% na.omit()
#     LAT.GSOD <- unique(x = df$LAT.GSOD) %>% na.omit()
#     ELEV.GSOD <- unique(x = df$ELEV.GSOD) %>% na.omit()
#     
#     EXCLUDED_VARS <- colnames(x = df)[colnames(x = df) %>%
#                                           grep(pattern = "ATTRIBUTES", x = .)]
#     
#     data.monthly.Shamsi[[station]] <- df %>%
#         select(-c(
#             EXCLUDED_VARS,
#             "STNID",
#             "LON.GSOD",
#             "LAT.GSOD",
#             "ELEV.GSOD"
#         )) %>%
#         Month.Gregorian.to.Shamsi(MONTH.ABB = FALSE) %>%
#         group_by(PERSIAN.YEAR, PERSIAN.MONTH) %>%
#         summarise_all(sum) %>%
#         rename(YEAR = PERSIAN.YEAR, MONTH = PERSIAN.MONTH) %>%
#         mutate(
#             STNID = STNID,
#             LON.GSOD = LON.GSOD,
#             LAT.GSOD = LAT.GSOD,
#             ELEV.GSOD = ELEV.GSOD
#         ) %>%
#         select(STNID,
#                LON.GSOD,
#                LAT.GSOD,
#                ELEV.GSOD,
#                YEAR,
#                MONTH,
#                everything())
# }
# 
# data.monthly.Shamsi <- do.call(what = rbind,
#                                args = data.monthly.Shamsi)
# 
# data.monthly.Shamsi[sapply(data.monthly.Shamsi, is.infinite)] <- NA
# data.monthly.Shamsi[sapply(data.monthly.Shamsi, is.nan)] <- NA
# 
# data.monthly.Shamsi <- as.data.frame(x = data.monthly.Shamsi)
# 
# saveRDS(
#     object = data.monthly.Shamsi,
#     file = paste0(Folder.Output, "/", "GSOD_Monthly_Khorasan_Shamsi.rds")
# )

# ETo Calculator ------------------------------------------------------------------------------

StationPET <- list()

for (ST in unique(x = data.monthly$STNID)) {
    
    print(ST)
    
    df <- as.data.frame(x = data.monthly %>% filter(STNID == ST))
    
    lat <- unique(x = df$LAT)
    print(lat)
    
    z <- unique(x = df$ELEV)
    print(z)
    
    if (is.na(x = lat)) {
        
        next()
        
    }
    
    StationPET[[ST]] <- df %>%
        mutate(
            
            ETo_HS = ETo.Hargreaves.Monthly(Tmin = .$TMIN,
                                            Tmax = .$TMAX,
                                            Ra = NA,
                                            lat = lat[1],
                                            Pre = .$PRCP,
                                            na.rm = TRUE) %>%
                matrix(ncol = 1, byrow = TRUE) %>%
                round(digits = 1),
            
            # ETo_PM = ETo.PenmanMonteith.Monthly(Tmin = .$TMIN,
            #                                     Tmax = .$TMAX,
            #                                     U2 = .$WDSP,
            #                                     Ra = NA,
            #                                     lat = lat[1],
            #                                     Rs = NA,
            #                                     tsun = NA,
            #                                     CC = NA,
            #                                     ed = NA,
            #                                     Tdew = .$DEWP,
            #                                     RH = .$RH,
            #                                     P = .$STP / 10,
            #                                     P0 = .$SLP / 10,
            #                                     z = z[1],
            #                                     crop = "short",
            #                                     na.rm = TRUE) %>%
            #     matrix(ncol = 1, byrow = TRUE) %>%
            #     round(digits = 1),
            
            ETo_TW = ETo.Thornthwaite.Monthly(Tave = .$TEMP,
                                              lat = lat[1],
                                              na.rm = TRUE) %>%
                matrix(ncol = 1, byrow = TRUE) %>%
                round(digits = 1),
        )
    
}

StationPET <- do.call(what = rbind,
                      args = StationPET)

rownames(StationPET) <- NULL


write.csv(
    x = StationPET,
    file = paste0(Folder.Output, "/", "ETo_GSOD_Dataset_Khorasan_Monthly.csv")
)

saveRDS(object = StationPET,
        file = paste0(Folder.Output, "/", "ETo_GSOD_Dataset_Khorasan_Monthly.rds"))

