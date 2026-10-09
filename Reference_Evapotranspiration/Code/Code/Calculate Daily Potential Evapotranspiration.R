

# Initial Settings ----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "ggplot2")
library(package = "lubridate")
library(package = "zoo")

source(file = "Code/Evapotranspiration.R")

# Calculate ETo Daily -------------------------------------------------------------------------

# Load Data

ST <- c("407400-99999",
        "407410-99999",
        "407430-99999",
        "407440-99999",
        "407450-99999",
        "407460-99999",
        "407620-99999",
        "407630-99999",
        "407920-99999",
        "408060-99999")

RBSN_Dataset <- readRDS(file = "Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST)

# Calculate ETo

RBSN_Result <- list()

for (st in unique(x = RBSN_Dataset$STNID)) {
    
    data_st <- RBSN_Dataset %>% 
        filter(STNID == st)
    
    constant <- list(lambda = 2.45,
                     sigma = 4.903 * 10 ^ -9,
                     Gsc = 0.0820,
                     G = 0,
                     as = 0.23,
                     bs = 0.5,
                     z = 10,
                     Elev = unique(x = data_st$station_elevation)[1],
                     lat_rad = unique(x = data_st$lat)[1] * pi / 180,
                     alphaPT = 1.26)
    
    data_ETP <- list(
        Date.daily = data_st$date,
        J = yday(data_st$date),
        Tmax = data_st$tmax,
        Tmin = data_st$tmin,
        RHmax = data_st$umax,
        RHmin = data_st$umin,
        n = data_st$sshn,
        u2 = data_st$ffm,
        Precip = data_st$rrr24,
        Rs = NULL,
        Cd = NULL,
        uz = NULL,
        va = NULL,
        vs = NULL
    )
    
    # Penman-Monteith Formulation -----------------------------------------------------------------
    
    data_st$ET.PenmanMonteith <- ET.PenmanMonteith(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        solar = "sunshine hours",
        wind = "yes",
        crop = "short",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    # Hargreaves-Samani Formulation ---------------------------------------------------------------
    
    data_st$ET.HargreavesSamani <- ET.HargreavesSamani(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    # Jensen-Haise Formulation --------------------------------------------------------------------
    
    data_st$ET.JensenHaise <- ET.JensenHaise(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        solar = "sunshine hours",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    # Makkink Formulation -------------------------------------------------------------------------
    
    data_st$ET.Makkink <- ET.Makkink(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        solar = "sunshine hours",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    # Priestley-Taylor Formulation ----------------------------------------------------------------
    
    data_st$ET.PriestleyTaylor <- ET.PriestleyTaylor(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        solar = "sunshine hours",
        alpha = 0.23,
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    # Turc Formulation ----------------------------------------------------------------------------
    
    data_st$ET.Turc <- ET.Turc(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        solar = "sunshine hours",
        humid = TRUE,
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    RBSN_Result[[st]] <- data_st
    
}

RBSN_Result <- do.call(what = rbind, args = RBSN_Result)

rownames(x = RBSN_Result) <- NULL

saveRDS(object = RBSN_Result, file = "Result/ETo_RBSN_Dataset_Daily_1959_2018.rds")
