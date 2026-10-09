
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

# Summary of Dataset --------------------------------------------------------------------------

data_daily <- readRDS(file = choose.files(caption = "Select *.rds File"))

str(object = data_daily)

data_daily %>%
  select(station_id, station_name, region_name, lat, lon, station_elevation) %>% 
  distinct_all() %>% 
  `rownames<-`(NULL)

# Convert Daily Data to Monthly Data ----------------------------------------------------------

## Day Critical
DC = 20

data_monthly <- data_daily %>%
  mutate(STNID = paste0(as.character(station_id), "0-99999")) %>%
  group_by(STNID, YEAR = year(date), MONTH = month(date)) %>%
  summarise(
    LAT = unique(x = lat),
    
    LONG = unique(x = lon),
    
    ELEV = unique(x = station_elevation),
    
    TEMP_ATTRIB = ifelse(test = all(is.na(tm)),
                         yes = NA,
                         no = sum(!is.na(tm))),
    
    TEMP = ifelse(test = TEMP_ATTRIB >= DC,
                  yes = mean(x = tm, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    DEWP_ATTRIB = ifelse(test = all(is.na(td_m)),
                         yes = NA,
                         no = sum(!is.na(td_m))),
    
    DEWP = ifelse(test = DEWP_ATTRIB >= DC,
                  yes = mean(x = td_m, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    SLP_ATTRIB = ifelse(test = all(is.na(pm)),
                                 yes = NA,
                                 no = sum(!is.na(pm))),
    
    SLP = ifelse(test = SLP_ATTRIB >= DC,
                 yes =  mean(x = pm, na.rm = TRUE) %>% round(digits = 1),
                 no = NA),

    STP_ATTRIB = ifelse(test = all(is.na(p0m)),
                        yes = NA,
                        no = sum(!is.na(p0m))),
    
    STP = ifelse(test = STP_ATTRIB >= DC,
                 yes = mean(x = p0m, na.rm = TRUE) %>% round(digits = 1),
                 no = NA),

    WDSP_ATTRIB = ifelse(test = all(is.na(ffm)),
                         yes = NA,
                         no = sum(!is.na(ffm))),
    
    WDSP = ifelse(test = WDSP_ATTRIB >= DC,
                  yes = mean(x = ffm, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    MXSPD_ATTRIB = ifelse(test = all(is.na(ff_max)),
                          yes = NA,
                          no = sum(!is.na(ff_max))),
    
    MXSPD = ifelse(test = MXSPD_ATTRIB >= DC,
                   yes = mean(x = ff_max, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),

    TMAX_ATTRIB = ifelse(test = all(is.na(tmax)),
                         yes = NA,
                         no = sum(!is.na(tmax))),
    
    TMAX = ifelse(test = TMAX_ATTRIB >= DC,
                  yes = mean(x = tmax, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    TMIN_ATTRIB = ifelse(test = all(is.na(tmin)),
                         yes = NA,
                         no = sum(!is.na(tmin))),
    
    TMIN = ifelse(test = TMIN_ATTRIB >= DC,
                  yes = mean(x = tmin, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    PRCP_ATTRIB = ifelse(test = all(is.na(rrr24)),
                         yes = NA,
                         no = sum(!is.na(rrr24))),
    
    PRCP = ifelse(test = PRCP_ATTRIB >= DC,
                  yes = sum(rrr24, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    EA_ATTRIB = ifelse(test = all(is.na(ewm)),
                       yes = NA,
                       no = sum(!is.na(ewm))),
    
    EA = ifelse(test = EA_ATTRIB >= DC,
                yes = mean(x = ewm, na.rm = TRUE) * 10 %>% round(digits = 1),
                no = NA),

    ES_ATTRIB = ifelse(test = all(is.na(ewsm)),
                       yes = NA,
                       no = sum(!is.na(ewsm))),
    
    ES = ifelse(test = ES_ATTRIB >= DC,
                yes = mean(x = ewsm, na.rm = TRUE) * 10 %>% round(digits = 1),
                no = NA),

    RH_ATTRIB = ifelse(test = all(is.na(um)),
                       yes = NA,
                       no = sum(!is.na(um))),
    
    RH = ifelse(test = RH_ATTRIB >= DC,
                yes = mean(x = um, na.rm = TRUE) %>% round(digits = 1),
                no = NA),

    RHMAX_ATTRIB = ifelse(test = all(is.na(umax)),
                          yes = NA,
                          no = sum(!is.na(umax))),
    
    RHMAX = ifelse(test = RHMAX_ATTRIB >= DC,
                   yes = mean(x = umax, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),

    RHMIN_ATTRIB = ifelse(test = all(is.na(umin)),
                          yes = NA,
                          no = sum(!is.na(umin))),
    
    RHMIN = ifelse(test = RHMIN_ATTRIB >= DC,
                   yes = mean(x = umin, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),

    SSHN_ATTRIB = ifelse(test = all(is.na(sshn)),
                         yes = NA,
                         no = sum(!is.na(sshn))),
    
    SSHN = ifelse(test = SSHN_ATTRIB >= DC,
                  yes = mean(x = sshn, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),

    RADGLO_ATTRIB = ifelse(test = all(is.na(radglo24)),
                           yes = NA,
                           no = sum(!is.na(radglo24))),
    
    RADGLO = ifelse(test = RADGLO_ATTRIB >= DC,
                    yes = mean(x = radglo24, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),

    EVAP_ATTRIB = ifelse(test = all(is.na(evt)),
                         yes = NA,
                         no = sum(!is.na(evt))),
    
    EVAP = ifelse(test = EVAP_ATTRIB >= DC,
                  yes = sum(evt, na.rm = TRUE) %>% round(digits = 1),
                  no = NA)
  )

data_monthly[sapply(data_monthly, is.infinite)] <- NA

data_monthly[sapply(data_monthly, is.nan)] <- NA

data_monthly <- as.data.frame(x = data_monthly)

saveRDS(object = data_monthly,
        file = paste0(Folder.Output, "/", "RBSN_Dataset_Khorasan_Monthly_1959_2018.rds"))

# Convert Gregorian Date to Shamsi Date -------------------------------------------------------

data_daily <- readRDS(file = choose.files(caption = "Select *.rds File"))

## Day Critical
DC = 20

# TODO : Find A Better Way

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
        file = paste0(Folder.Output, "/", "RBSN_Dataset_Khorasan_Daily.rds"))

data_monthly <- data_daily %>%
  
  group_by(STNID, YEAR, MONTH) %>%
  
  summarise(
    LAT = unique(x = lat),
    
    LONG = unique(x = lon),
    
    ELEV = unique(x = station_elevation),
    
    TEMP_ATTRIB = ifelse(test = all(is.na(tm)),
                         yes = NA,
                         no = sum(!is.na(tm))),
    
    TEMP = ifelse(test = TEMP_ATTRIB >= DC,
                  yes = mean(x = tm, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    DEWP_ATTRIB = ifelse(test = all(is.na(td_m)),
                         yes = NA,
                         no = sum(!is.na(td_m))),
    
    DEWP = ifelse(test = DEWP_ATTRIB >= DC,
                  yes = mean(x = td_m, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    SLP_ATTRIB = ifelse(test = all(is.na(pm)),
                        yes = NA,
                        no = sum(!is.na(pm))),
    
    SLP = ifelse(test = SLP_ATTRIB >= DC,
                 yes =  mean(x = pm, na.rm = TRUE) %>% round(digits = 1),
                 no = NA),
    
    STP_ATTRIB = ifelse(test = all(is.na(p0m)),
                        yes = NA,
                        no = sum(!is.na(p0m))),
    
    STP = ifelse(test = STP_ATTRIB >= DC,
                 yes = mean(x = p0m, na.rm = TRUE) %>% round(digits = 1),
                 no = NA),
    
    WDSP_ATTRIB = ifelse(test = all(is.na(ffm)),
                         yes = NA,
                         no = sum(!is.na(ffm))),
    
    WDSP = ifelse(test = WDSP_ATTRIB >= DC,
                  yes = mean(x = ffm, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    MXSPD_ATTRIB = ifelse(test = all(is.na(ff_max)),
                          yes = NA,
                          no = sum(!is.na(ff_max))),
    
    MXSPD = ifelse(test = MXSPD_ATTRIB >= DC,
                   yes = mean(x = ff_max, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),
    
    TMAX_ATTRIB = ifelse(test = all(is.na(tmax)),
                         yes = NA,
                         no = sum(!is.na(tmax))),
    
    TMAX = ifelse(test = TMAX_ATTRIB >= DC,
                  yes = mean(x = tmax, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    TMIN_ATTRIB = ifelse(test = all(is.na(tmin)),
                         yes = NA,
                         no = sum(!is.na(tmin))),
    
    TMIN = ifelse(test = TMIN_ATTRIB >= DC,
                  yes = mean(x = tmin, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    PRCP_ATTRIB = ifelse(test = all(is.na(rrr24)),
                         yes = NA,
                         no = sum(!is.na(rrr24))),
    
    PRCP = ifelse(test = PRCP_ATTRIB >= DC,
                  yes = sum(rrr24, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    EA_ATTRIB = ifelse(test = all(is.na(ewm)),
                       yes = NA,
                       no = sum(!is.na(ewm))),
    
    EA = ifelse(test = EA_ATTRIB >= DC,
                yes = mean(x = ewm, na.rm = TRUE) * 10 %>% round(digits = 1),
                no = NA),
    
    ES_ATTRIB = ifelse(test = all(is.na(ewsm)),
                       yes = NA,
                       no = sum(!is.na(ewsm))),
    
    ES = ifelse(test = ES_ATTRIB >= DC,
                yes = mean(x = ewsm, na.rm = TRUE) * 10 %>% round(digits = 1),
                no = NA),
    
    RH_ATTRIB = ifelse(test = all(is.na(um)),
                       yes = NA,
                       no = sum(!is.na(um))),
    
    RH = ifelse(test = RH_ATTRIB >= DC,
                yes = mean(x = um, na.rm = TRUE) %>% round(digits = 1),
                no = NA),
    
    RHMAX_ATTRIB = ifelse(test = all(is.na(umax)),
                          yes = NA,
                          no = sum(!is.na(umax))),
    
    RHMAX = ifelse(test = RHMAX_ATTRIB >= DC,
                   yes = mean(x = umax, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),
    
    RHMIN_ATTRIB = ifelse(test = all(is.na(umin)),
                          yes = NA,
                          no = sum(!is.na(umin))),
    
    RHMIN = ifelse(test = RHMIN_ATTRIB >= DC,
                   yes = mean(x = umin, na.rm = TRUE) %>% round(digits = 1),
                   no = NA),
    
    SSHN_ATTRIB = ifelse(test = all(is.na(sshn)),
                         yes = NA,
                         no = sum(!is.na(sshn))),
    
    SSHN = ifelse(test = SSHN_ATTRIB >= DC,
                  yes = mean(x = sshn, na.rm = TRUE) %>% round(digits = 1),
                  no = NA),
    
    RADGLO_ATTRIB = ifelse(test = all(is.na(radglo24)),
                           yes = NA,
                           no = sum(!is.na(radglo24))),
    
    RADGLO = ifelse(test = RADGLO_ATTRIB >= DC,
                    yes = mean(x = radglo24, na.rm = TRUE) %>% round(digits = 1),
                    no = NA),
    
    EVAP_ATTRIB = ifelse(test = all(is.na(evt)),
                         yes = NA,
                         no = sum(!is.na(evt))),
    
    EVAP = ifelse(test = EVAP_ATTRIB >= DC,
                  yes = sum(evt, na.rm = TRUE) %>% round(digits = 1),
                  no = NA)
  )

data_monthly[sapply(data_monthly, is.infinite)] <- NA

data_monthly[sapply(data_monthly, is.nan)] <- NA

data_monthly <- as.data.frame(x = data_monthly)

saveRDS(object = data_monthly,
        file = paste0(Folder.Output, "/", "RBSN_Dataset_Khorasan_Monthly_1338_1397.rds"))

# ETo Calculator ------------------------------------------------------------------------------

StationPET <- list()

for (ST in unique(x = data_monthly$STNID)) {
  
  print(ST)
  
  df <- as.data.frame(x = data_monthly %>% filter(STNID == ST))
  
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
      
      ETo_PM = ETo.PenmanMonteith.Monthly(Tmin = .$TMIN,
                                          Tmax = .$TMAX,
                                          U2 = .$WDSP,
                                          Ra = NA,
                                          lat = lat[1],
                                          Rs = NA,
                                          tsun = .$SSHN,
                                          CC = NA,
                                          ed = NA,
                                          Tdew = .$DEWP,
                                          RH = .$RH,
                                          P = .$STP / 10,
                                          P0 = .$SLP / 10,
                                          z = z[1],
                                          crop = "short",
                                          na.rm = TRUE) %>%
          matrix(ncol = 1, byrow = TRUE) %>%
          round(digits = 1),
      
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
  file = paste0(Folder.Output, "/", "ETo_RBSN_Dataset_Khorasan_Monthly.csv"),
  row.names = FALSE
)

saveRDS(object = StationPET,
        file = paste0(Folder.Output, "/", "ETo_RBSN_Dataset_Khorasan_Monthly.rds"))
