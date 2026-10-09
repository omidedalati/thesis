
# Initial Settings ----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "tidyr")
library(package = "ggplot2")
library(package = "lubridate")
library(package = "stringr")
library(package = "clipr")

# Load Raw Data -------------------------------------------------------------------------------

RBSN <- readRDS(file = "Result/ETo_RBSN_Dataset_Daily_1959_2018.rds") %>% 
    rename(Shamsi_Year = YEAR,
           Shamsi_Month = MONTH,
           Shamsi_Day = DAY) %>% 
    mutate(YEAR = year(x = date),
           MONTH = month(x = date),
           DAY = day(x = date))

# Extract Required Data -----------------------------------------------------------------------

RefET_Data <- RBSN %>% 
    select(date, STNID, lat, lon, station_elevation,
           ffm, tmax, tmin, tm, rrr24, umax, umin, um, sshn, td_m, twet_m) %>% 
    mutate(YEAR = year(x = date),
           MONTH = month(x = date),
           DAY = day(x = date)) %>% 
    rename(LAT = lat,
           LON = lon,
           ELEV = station_elevation,
           WS = ffm,
           TX = tmax,
           TN = tmin,
           TEMP = tm,
           PRECIP = rrr24,
           RHX = umax,
           RHN = umin,
           RH = um,
           SSHN = sshn,
           DP = td_m,
           TW = twet_m) %>% 
    select(STNID, LAT, LON, ELEV, YEAR, MONTH, DAY, TX, TN, TEMP, SSHN, DP, RHN, RHX, RH, TW, WS, PRECIP) %>% 
    mutate_all(~replace(x = ., list = is.na(.), values = -999))

# Calculate ETo -------------------------------------------------------------------------------

for (st in unique(x = RefET_Data$STNID)) {
    
    df <- RefET_Data %>% 
        filter(STNID == st)
    
    Latitude <- unique(x = df$LAT)
    Longitude <- unique(x = df$LON)
    Elevation <- unique(x = df$ELEV)
    
    df %>% 
        select(-STNID, -LAT, -LON, -ELEV) %>% 
        write.csv(file = paste0("Result/RefET/", st, ".csv"),
                  row.names = FALSE)
    
    DEF_FILE = readLines(con = "Result/RefET/DefinitionFile.def", n = -1)
    DEF_FILE[2] = paste0("2,2,", Elevation, ",", Latitude, ",0")
    writeLines(text = DEF_FILE, con = "Result/RefET/DefinitionFile.def")
    
    RefET_exe_Path <- "C:\\Users\\pooya\\Programs\\RefET\\REF-ET.exe"
    
    RefET_Result_Path <- "C:\\Users\\pooya\\Dropbox\\HydroTech\\Project_01\\Reference_Evapotranspiration\\Result\\RefET\\"
    
    RefET_Data_Path <- paste0(RefET_Result_Path, st, ".csv")
    
    RefET_Def_Path <- paste0(RefET_Result_Path, "DefinitionFile.def")
    
    command <- str_c(RefET_exe_Path, "'-'", RefET_Data_Path, RefET_Def_Path, sep = " ")
    
    system(command = command, wait = TRUE)
    
}

# Read ETo Result -----------------------------------------------------------------------------

Result_List_File <- list.files(path = "Result/RefET/",
                               pattern = ".out",
                               full.names = TRUE)

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

Day_Month_Shamsi <- data.frame(Shamsi_Month = c(1:12),
                               DAY_MONTH = c(31, 31, 31, 31, 31, 31, 30, 30, 30, 30, 30, 29))

DC <- 20

Result_ETo_Daily <- list()
Result_ETo_Monthly <- list()
Result_ETo_Monthly_Shamsi <- list()

for (i in Result_List_File) {
    
    df_day <- read.csv(file = i, 
                       header = FALSE, 
                       skip = 6, 
                       sep = "")
    
    df_day[, -c(1:3)][df_day[, -c(1:3)] == -999] <- NA
    
    
    df_day[, -c(1:8, 15)][df_day[, -c(1:8, 15)] < 0] <- 0
    
    colnames(x = df_day) <- c("MONTH", "DAY", "YEAR",
                              "TX", "TN", "nN", "WS", "DP",
                              "ASCE_PM", "FAO56_PM", "HS", "PT", "MAKK", "TURC",
                              "PRECIP")
    
    df_day <- df_day %>% 
        mutate(STNID = substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4))
    
    df_day <- df_day %>% 
        left_join(y = RBSN %>% select(STNID, YEAR, MONTH, DAY, Shamsi_Year, Shamsi_Month, Shamsi_Day),
                  by = c("STNID", "YEAR", "MONTH", "DAY"))
    
    df_month <- df_day %>%
        select(YEAR, MONTH, DAY,
               TX, TN, nN, WS, DP, PRECIP,
               ASCE_PM, FAO56_PM, HS, PT, MAKK, TURC) %>%
        group_by(YEAR, MONTH) %>%
        summarise(
            TX_ATTRIB = ifelse(test = all(is.na(TX)),
                               yes = NA,
                               no = sum(!is.na(TX))),
            
            TX = ifelse(test = TX_ATTRIB >= DC,
                        yes = (mean(x = TX, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            TN_ATTRIB = ifelse(test = all(is.na(TN)),
                               yes = NA,
                               no = sum(!is.na(TN))),
            
            TN = ifelse(test = TN_ATTRIB >= DC,
                        yes = (mean(x = TN, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            nN_ATTRIB = ifelse(test = all(is.na(nN)),
                               yes = NA,
                               no = sum(!is.na(nN))),
            
            nN = ifelse(test = nN_ATTRIB >= DC,
                        yes = (mean(x = nN, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            WS_ATTRIB = ifelse(test = all(is.na(WS)),
                               yes = NA,
                               no = sum(!is.na(WS))),
            
            WS = ifelse(test = WS_ATTRIB >= DC,
                        yes = (mean(x = WS, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            DP_ATTRIB = ifelse(test = all(is.na(DP)),
                               yes = NA,
                               no = sum(!is.na(DP))),
            
            DP = ifelse(test = DP_ATTRIB >= DC,
                        yes = (mean(x = DP, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            PRECIP_ATTRIB = ifelse(test = all(is.na(PRECIP)),
                                   yes = NA,
                                   no = sum(!is.na(PRECIP))),
            
            PRECIP = ifelse(test = PRECIP_ATTRIB >= DC,
                            yes = (mean(x = PRECIP, na.rm = TRUE)) %>% round(digits = 1),
                            no = NA),
            
            ASCE_PM_ATTRIB = ifelse(test = all(is.na(ASCE_PM)),
                                    yes = NA,
                                    no = sum(!is.na(ASCE_PM))),
            
            ASCE_PM = ifelse(test = ASCE_PM_ATTRIB >= DC,
                             yes = (mean(x = ASCE_PM, na.rm = TRUE)) %>% round(digits = 1),
                             no = NA),
            
            FAO56_PM_ATTRIB = ifelse(test = all(is.na(FAO56_PM)),
                                     yes = NA,
                                     no = sum(!is.na(FAO56_PM))),
            
            FAO56_PM = ifelse(test = FAO56_PM_ATTRIB >= DC,
                              yes = (mean(x = FAO56_PM, na.rm = TRUE)) %>% round(digits = 1),
                              no = NA),
            
            HS_ATTRIB = ifelse(test = all(is.na(HS)),
                               yes = NA,
                               no = sum(!is.na(HS))),
            
            HS = ifelse(test = HS_ATTRIB >= DC,
                        yes = (mean(x = HS, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            PT_ATTRIB = ifelse(test = all(is.na(PT)),
                               yes = NA,
                               no = sum(!is.na(PT))),
            
            PT = ifelse(test = PT_ATTRIB >= DC,
                        yes = (mean(x = PT, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            MAKK_ATTRIB = ifelse(test = all(is.na(MAKK)),
                                 yes = NA,
                                 no = sum(!is.na(MAKK))),
            
            MAKK = ifelse(test = MAKK_ATTRIB >= DC,
                          yes = (mean(x = MAKK, na.rm = TRUE)) %>% round(digits = 1),
                          no = NA),
            
            TURC_ATTRIB = ifelse(test = all(is.na(TURC)),
                                 yes = NA,
                                 no = sum(!is.na(TURC))),
            
            TURC = ifelse(test = TURC_ATTRIB >= DC,
                          yes = (mean(x = TURC, na.rm = TRUE)) %>% round(digits = 1),
                          no = NA)
        ) %>%
        left_join(y = Day_Month, by = 'MONTH') %>%
        mutate(ASCE_PM = ASCE_PM * DAY_MONTH,
               FAO56_PM = FAO56_PM * DAY_MONTH,
               HS = HS * DAY_MONTH,
               PT = PT * DAY_MONTH,
               MAKK = MAKK * DAY_MONTH,
               TURC = TURC * DAY_MONTH,
               PRECIP = PRECIP * DAY_MONTH) %>% 
        mutate(STNID = substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4))
    
    df_month_shamsi <- df_day %>%
        select(Shamsi_Year, Shamsi_Month, Shamsi_Day,
               TX, TN, nN, WS, DP, PRECIP,
               ASCE_PM, FAO56_PM, HS, PT, MAKK, TURC) %>%
        group_by(Shamsi_Year, Shamsi_Month) %>%
        summarise(
            TX_ATTRIB = ifelse(test = all(is.na(TX)),
                               yes = NA,
                               no = sum(!is.na(TX))),
            
            TX = ifelse(test = TX_ATTRIB >= DC,
                        yes = (mean(x = TX, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            TN_ATTRIB = ifelse(test = all(is.na(TN)),
                               yes = NA,
                               no = sum(!is.na(TN))),
            
            TN = ifelse(test = TN_ATTRIB >= DC,
                        yes = (mean(x = TN, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            nN_ATTRIB = ifelse(test = all(is.na(nN)),
                               yes = NA,
                               no = sum(!is.na(nN))),
            
            nN = ifelse(test = nN_ATTRIB >= DC,
                        yes = (mean(x = nN, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            WS_ATTRIB = ifelse(test = all(is.na(WS)),
                               yes = NA,
                               no = sum(!is.na(WS))),
            
            WS = ifelse(test = WS_ATTRIB >= DC,
                        yes = (mean(x = WS, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            DP_ATTRIB = ifelse(test = all(is.na(DP)),
                               yes = NA,
                               no = sum(!is.na(DP))),
            
            DP = ifelse(test = DP_ATTRIB >= DC,
                        yes = (mean(x = DP, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            PRECIP_ATTRIB = ifelse(test = all(is.na(PRECIP)),
                                   yes = NA,
                                   no = sum(!is.na(PRECIP))),
            
            PRECIP = ifelse(test = PRECIP_ATTRIB >= DC,
                            yes = (mean(x = PRECIP, na.rm = TRUE)) %>% round(digits = 1),
                            no = NA),
            
            ASCE_PM_ATTRIB = ifelse(test = all(is.na(ASCE_PM)),
                                    yes = NA,
                                    no = sum(!is.na(ASCE_PM))),
            
            ASCE_PM = ifelse(test = ASCE_PM_ATTRIB >= DC,
                             yes = (mean(x = ASCE_PM, na.rm = TRUE)) %>% round(digits = 1),
                             no = NA),
            
            FAO56_PM_ATTRIB = ifelse(test = all(is.na(FAO56_PM)),
                                     yes = NA,
                                     no = sum(!is.na(FAO56_PM))),
            
            FAO56_PM = ifelse(test = FAO56_PM_ATTRIB >= DC,
                              yes = (mean(x = FAO56_PM, na.rm = TRUE)) %>% round(digits = 1),
                              no = NA),
            
            HS_ATTRIB = ifelse(test = all(is.na(HS)),
                               yes = NA,
                               no = sum(!is.na(HS))),
            
            HS = ifelse(test = HS_ATTRIB >= DC,
                        yes = (mean(x = HS, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            PT_ATTRIB = ifelse(test = all(is.na(PT)),
                               yes = NA,
                               no = sum(!is.na(PT))),
            
            PT = ifelse(test = PT_ATTRIB >= DC,
                        yes = (mean(x = PT, na.rm = TRUE)) %>% round(digits = 1),
                        no = NA),
            
            MAKK_ATTRIB = ifelse(test = all(is.na(MAKK)),
                                 yes = NA,
                                 no = sum(!is.na(MAKK))),
            
            MAKK = ifelse(test = MAKK_ATTRIB >= DC,
                          yes = (mean(x = MAKK, na.rm = TRUE)) %>% round(digits = 1),
                          no = NA),
            
            TURC_ATTRIB = ifelse(test = all(is.na(TURC)),
                                 yes = NA,
                                 no = sum(!is.na(TURC))),
            
            TURC = ifelse(test = TURC_ATTRIB >= DC,
                          yes = (mean(x = TURC, na.rm = TRUE)) %>% round(digits = 1),
                          no = NA)
        ) %>%
        left_join(y = Day_Month_Shamsi, by = 'Shamsi_Month') %>%
        mutate(ASCE_PM = ASCE_PM * DAY_MONTH,
               FAO56_PM = FAO56_PM * DAY_MONTH,
               HS = HS * DAY_MONTH,
               PT = PT * DAY_MONTH,
               MAKK = MAKK * DAY_MONTH,
               TURC = TURC * DAY_MONTH,
               PRECIP = PRECIP * DAY_MONTH) %>% 
        mutate(STNID = substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4))
    
    Result_ETo_Daily[[substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4)]] <- df_day
    Result_ETo_Monthly[[substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4)]] <- df_month
    Result_ETo_Monthly_Shamsi[[substr(x = i, start = nchar(x = i) - 15, stop = nchar(x = i) - 4)]] <- df_month_shamsi
    
}

Result_ETo_Daily <- do.call(what = rbind, args = Result_ETo_Daily)

write.csv(x = Result_ETo_Daily,
          file = "Result/RefET/ETo_RBSN_RefET_Daily.csv",
          row.names = FALSE)

Result_ETo_Monthly <- do.call(what = rbind, args = Result_ETo_Monthly)

write.csv(x = Result_ETo_Monthly,
          file = "Result/RefET/ETo_RBSN_RefET_Monthly.csv",
          row.names = FALSE)

Result_ETo_Monthly_Shamsi <- do.call(what = rbind, args = Result_ETo_Monthly_Shamsi)

write.csv(x = Result_ETo_Monthly_Shamsi,
          file = "Result/RefET/ETo_RBSN_RefET_Monthly_Shamsi.csv",
          row.names = FALSE)
