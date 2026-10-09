
# Initial Settings ----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "zoo")

source(file = "C:/thesis/Shirazi_Code/Reference_Evapotranspiration/Code/Evapotranspiration.R")

# RBSN Dataset --------------------------------------------------------------------------------

RBSN_Dataset <- readRDS(file = "C:/thesis/Shirazi_Code/Reference_Evapotranspiration/Result/RBSN_Dataset_Khorasan_Daily_1959_2018.rds") %>% 
    filter(STNID %in% c("407440-99999", "407450-99999"))

# Shamsi Calendar

RBSN_Result <- list()

DC = 25

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(rep(x = 31, 6), rep(x = 30, 5), 28))

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
    
    
    RBSN_Result[[st]] <- data_st %>% 
        
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
                          no = NA),
            
            ET.PenmanMonteith_ATTRIB = ifelse(test = all(is.na(ET.PenmanMonteith)),
                                              yes = NA,
                                              no = sum(!is.na(ET.PenmanMonteith))),
            
            ET.PenmanMonteith = ifelse(test = ET.PenmanMonteith_ATTRIB >= DC,
                                       yes = mean(x = ET.PenmanMonteith, na.rm = TRUE) %>% round(digits = 1),
                                       no = NA),
            
            ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
                                                yes = NA,
                                                no = sum(!is.na(ET.HargreavesSamani))),
            
            ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
                                         yes = mean(x = ET.HargreavesSamani, na.rm = TRUE) %>% round(digits = 1),
                                         no = NA),
            
            ET.JensenHaise_ATTRIB = ifelse(test = all(is.na(ET.JensenHaise)),
                                           yes = NA,
                                           no = sum(!is.na(ET.JensenHaise))),
            
            ET.JensenHaise = ifelse(test = ET.JensenHaise_ATTRIB >= DC,
                                    yes = mean(x = ET.JensenHaise, na.rm = TRUE) %>% round(digits = 1),
                                    no = NA),
            
            ET.Makkink_ATTRIB = ifelse(test = all(is.na(ET.Makkink)),
                                           yes = NA,
                                           no = sum(!is.na(ET.Makkink))),
            
            ET.Makkink = ifelse(test = ET.Makkink_ATTRIB >= DC,
                                    yes = mean(x = ET.Makkink, na.rm = TRUE) %>% round(digits = 1),
                                    no = NA),
            
            ET.PriestleyTaylor_ATTRIB = ifelse(test = all(is.na(ET.PriestleyTaylor)),
                                           yes = NA,
                                           no = sum(!is.na(ET.PriestleyTaylor))),
            
            ET.PriestleyTaylor = ifelse(test = ET.PriestleyTaylor_ATTRIB >= DC,
                                    yes = mean(x = ET.PriestleyTaylor, na.rm = TRUE) %>% round(digits = 1),
                                    no = NA),
            
            ET.Turc_ATTRIB = ifelse(test = all(is.na(ET.Turc)),
                                           yes = NA,
                                           no = sum(!is.na(ET.Turc))),
            
            ET.Turc = ifelse(test = ET.Turc_ATTRIB >= DC,
                                    yes = mean(x = ET.Turc, na.rm = TRUE) %>% round(digits = 1),
                                    no = NA)
        ) %>% left_join(y = Day_Month, by = 'MONTH') %>% 
        
        mutate(ET.PenmanMonteith = ET.PenmanMonteith * DAY_MONTH,
               ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH,
               ET.JensenHaise = ET.JensenHaise * DAY_MONTH,
               ET.Makkink = ET.Makkink * DAY_MONTH,
               ET.PriestleyTaylor = ET.PriestleyTaylor * DAY_MONTH,
               ET.Turc = ET.Turc * DAY_MONTH,) %>% 
        
        select(STNID, YEAR, MONTH, DAY_MONTH,
               everything())
    
}

RBSN_Result <- do.call(what = rbind, args = RBSN_Result)

saveRDS(object = RBSN_Result, file = "Result/ETo_RBSN_Dataset_Monthly_1338_1397.rds")

# # Gregorian Calendar

RBSN_Result <- list()

DC = 25

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

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
    
    
    RBSN_Result[[st]] <- data_st %>% 
        
        group_by(STNID, YEAR = year(x = date), MONTH = month(x = date)) %>% 
        
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
                          no = NA),
            
            ET.PenmanMonteith_ATTRIB = ifelse(test = all(is.na(ET.PenmanMonteith)),
                                              yes = NA,
                                              no = sum(!is.na(ET.PenmanMonteith))),
            
            ET.PenmanMonteith = ifelse(test = ET.PenmanMonteith_ATTRIB >= DC,
                                       yes = mean(x = ET.PenmanMonteith, na.rm = TRUE) %>% round(digits = 1),
                                       no = NA),
            
            ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
                                                yes = NA,
                                                no = sum(!is.na(ET.HargreavesSamani))),
            
            ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
                                         yes = mean(x = ET.HargreavesSamani, na.rm = TRUE) %>% round(digits = 1),
                                         no = NA),
            
            ET.JensenHaise_ATTRIB = ifelse(test = all(is.na(ET.JensenHaise)),
                                           yes = NA,
                                           no = sum(!is.na(ET.JensenHaise))),
            
            ET.JensenHaise = ifelse(test = ET.JensenHaise_ATTRIB >= DC,
                                    yes = mean(x = ET.JensenHaise, na.rm = TRUE) %>% round(digits = 1),
                                    no = NA),
            
            ET.Makkink_ATTRIB = ifelse(test = all(is.na(ET.Makkink)),
                                       yes = NA,
                                       no = sum(!is.na(ET.Makkink))),
            
            ET.Makkink = ifelse(test = ET.Makkink_ATTRIB >= DC,
                                yes = mean(x = ET.Makkink, na.rm = TRUE) %>% round(digits = 1),
                                no = NA),
            
            ET.PriestleyTaylor_ATTRIB = ifelse(test = all(is.na(ET.PriestleyTaylor)),
                                               yes = NA,
                                               no = sum(!is.na(ET.PriestleyTaylor))),
            
            ET.PriestleyTaylor = ifelse(test = ET.PriestleyTaylor_ATTRIB >= DC,
                                        yes = mean(x = ET.PriestleyTaylor, na.rm = TRUE) %>% round(digits = 1),
                                        no = NA),
            
            ET.Turc_ATTRIB = ifelse(test = all(is.na(ET.Turc)),
                                    yes = NA,
                                    no = sum(!is.na(ET.Turc))),
            
            ET.Turc = ifelse(test = ET.Turc_ATTRIB >= DC,
                             yes = mean(x = ET.Turc, na.rm = TRUE) %>% round(digits = 1),
                             no = NA)
        ) %>% left_join(y = Day_Month, by = 'MONTH') %>% 
        
        mutate(ET.PenmanMonteith = ET.PenmanMonteith * DAY_MONTH,
               ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH,
               ET.JensenHaise = ET.JensenHaise * DAY_MONTH,
               ET.Makkink = ET.Makkink * DAY_MONTH,
               ET.PriestleyTaylor = ET.PriestleyTaylor * DAY_MONTH,
               ET.Turc = ET.Turc * DAY_MONTH,) %>% 
        
        select(STNID, YEAR, MONTH, DAY_MONTH,
               everything())
    
}

RBSN_Result <- do.call(what = rbind, args = RBSN_Result)

saveRDS(object = RBSN_Result, file = "Result/ETo_RBSN_Dataset_Monthly_1959_2018.rds")

# GSOD Dataset --------------------------------------------------------------------------------

GSOD_Dataset <- readRDS(file = "Result/GSOD_Dataset_Khorasan_Daily_1959_2019.rds") %>% 
    filter(STNID %in% c("407450-99999"))

# Shamsi Calendar

GSOD_Result <- list()

DC = 25

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(rep(x = 31, 6), rep(x = 30, 5), 28))

for (st in unique(x = GSOD_Dataset$STNID)) {
    
    data_st <- GSOD_Dataset %>% 
        filter(STNID == st)
    
    constant <- list(lambda = 2.45,
                     sigma = 4.903 * 10 ^ -9,
                     Gsc = 0.0820,
                     G = 0,
                     as = 0.23,
                     bs = 0.5,
                     z = 10,
                     Elev = unique(x = data_st$ELEVATION)[1],
                     lat_rad = unique(x = data_st$LATITUDE)[1] * pi / 180,
                     alphaPT = 1.26)
    
    data_ETP <- list(
        Date.daily = data_st$YEARMODA,
        J = yday(data_st$YEARMODA),
        Tmax = data_st$MAX,
        Tmin = data_st$MIN,
        RH = data_st$RH,
        u2 = data_st$WDSP,
        Precip = data_st$PRCP,
        Rs = NULL,
        Cd = NULL,
        uz = NULL,
        va = NULL,
        vs = NULL
    )

    # Hargreaves-Samani Formulation ---------------------------------------------------------------
    
    data_st$ET.HargreavesSamani <- ET.HargreavesSamani(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    GSOD_Result[[st]] <- data_st %>% 
        
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
                         yes =  mean(x = SLP, na.rm = TRUE) %>% round(digits = 1),
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
                        yes = mean(x = EA, na.rm = TRUE) * 10 %>% round(digits = 1),
                        no = NA),
            
            ES_ATTRIB = ifelse(test = all(is.na(ES)),
                               yes = NA,
                               no = sum(!is.na(ES))),
            
            ES = ifelse(test = ES_ATTRIB >= DC,
                        yes = mean(x = ES, na.rm = TRUE) * 10 %>% round(digits = 1),
                        no = NA),
            
            RH_ATTRIB = ifelse(test = all(is.na(RH)),
                               yes = NA,
                               no = sum(!is.na(RH))),
            
            RH = ifelse(test = RH_ATTRIB >= DC,
                        yes = mean(x = RH, na.rm = TRUE) %>% round(digits = 1),
                        no = NA),
            
            ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
                                                yes = NA,
                                                no = sum(!is.na(ET.HargreavesSamani))),
            
            ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
                                         yes = mean(x = ET.HargreavesSamani, na.rm = TRUE) %>% round(digits = 1),
                                         no = NA)
            
        ) %>% left_join(y = Day_Month, by = 'MONTH') %>% 
        
        mutate(ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH) %>% 
        
        select(STNID, YEAR, MONTH, DAY_MONTH,
               everything())
    
}

GSOD_Result <- do.call(what = rbind, args = GSOD_Result)

saveRDS(object = GSOD_Result, file = "Result/ETo_GSOD_Dataset_Monthly_1338_1398.rds")

# Gregorian Calendar

GSOD_Result <- list()

DC = 25

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

for (st in unique(x = GSOD_Dataset$STNID)) {
    
    data_st <- GSOD_Dataset %>% 
        filter(STNID == st)
    
    constant <- list(lambda = 2.45,
                     sigma = 4.903 * 10 ^ -9,
                     Gsc = 0.0820,
                     G = 0,
                     as = 0.23,
                     bs = 0.5,
                     z = 10,
                     Elev = unique(x = data_st$ELEVATION)[1],
                     lat_rad = unique(x = data_st$LATITUDE)[1] * pi / 180,
                     alphaPT = 1.26)
    
    data_ETP <- list(
        Date.daily = data_st$YEARMODA,
        J = yday(data_st$YEARMODA),
        Tmax = data_st$MAX,
        Tmin = data_st$MIN,
        RH = data_st$RH,
        u2 = data_st$WDSP,
        Precip = data_st$PRCP,
        Rs = NULL,
        Cd = NULL,
        uz = NULL,
        va = NULL,
        vs = NULL
    )
    
    # Hargreaves-Samani Formulation ---------------------------------------------------------------
    
    data_st$ET.HargreavesSamani <- ET.HargreavesSamani(
        data = data_ETP,
        constants = constant,
        ts = "daily",
        message = "no",
        AdditionalStats = "no",
        save.csv = "no"
    )$ET.Daily
    
    GSOD_Result[[st]] <- data_st %>% 
        
        group_by(STNID, YEAR = year(x = YEARMODA), MONTH = month(x = YEARMODA)) %>% 
        
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
                         yes =  mean(x = SLP, na.rm = TRUE) %>% round(digits = 1),
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
                        yes = mean(x = EA, na.rm = TRUE) * 10 %>% round(digits = 1),
                        no = NA),
            
            ES_ATTRIB = ifelse(test = all(is.na(ES)),
                               yes = NA,
                               no = sum(!is.na(ES))),
            
            ES = ifelse(test = ES_ATTRIB >= DC,
                        yes = mean(x = ES, na.rm = TRUE) * 10 %>% round(digits = 1),
                        no = NA),
            
            RH_ATTRIB = ifelse(test = all(is.na(RH)),
                               yes = NA,
                               no = sum(!is.na(RH))),
            
            RH = ifelse(test = RH_ATTRIB >= DC,
                        yes = mean(x = RH, na.rm = TRUE) %>% round(digits = 1),
                        no = NA),
            
            ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
                                                yes = NA,
                                                no = sum(!is.na(ET.HargreavesSamani))),
            
            ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
                                         yes = mean(x = ET.HargreavesSamani, na.rm = TRUE) %>% round(digits = 1),
                                         no = NA)
            
        ) %>% left_join(y = Day_Month, by = 'MONTH') %>% 
        
        mutate(ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH) %>% 
        
        select(STNID, YEAR, MONTH, DAY_MONTH,
               everything())
    
}

GSOD_Result <- do.call(what = rbind, args = GSOD_Result)

saveRDS(object = GSOD_Result, file = "Result/ETo_GSOD_Dataset_Monthly_1959_2019.rds")





# 
# 
# 
# DC <- 01
# 
# result_yearly <- result %>% 
#     group_by(STNID, YEAR) %>% 
#     summarise(
#         ET.PenmanMonteith_ATTRIB = ifelse(test = all(is.na(ET.PenmanMonteith)),
#                                           yes = NA,
#                                           no = sum(!is.na(ET.PenmanMonteith))),
#         
#         ET.PenmanMonteith = ifelse(test = ET.PenmanMonteith_ATTRIB >= DC,
#                                    yes = mean(x = ET.PenmanMonteith, na.rm = TRUE) %>% round(digits = 1),
#                                    no = NA),
#         
#         ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
#                                             yes = NA,
#                                             no = sum(!is.na(ET.HargreavesSamani))),
#         
#         ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
#                                      yes = mean(x = ET.HargreavesSamani, na.rm = TRUE) %>% round(digits = 1),
#                                      no = NA),
#         
#         ET.JensenHaise_ATTRIB = ifelse(test = all(is.na(ET.JensenHaise)),
#                                        yes = NA,
#                                        no = sum(!is.na(ET.JensenHaise))),
#         
#         ET.JensenHaise = ifelse(test = ET.JensenHaise_ATTRIB >= DC,
#                                 yes = mean(x = ET.JensenHaise, na.rm = TRUE) %>% round(digits = 1),
#                                 no = NA),
#         
#         ET.Makkink_ATTRIB = ifelse(test = all(is.na(ET.Makkink)),
#                                    yes = NA,
#                                    no = sum(!is.na(ET.Makkink))),
#         
#         ET.Makkink = ifelse(test = ET.Makkink_ATTRIB >= DC,
#                             yes = mean(x = ET.Makkink, na.rm = TRUE) %>% round(digits = 1),
#                             no = NA),
#         
#         ET.PriestleyTaylor_ATTRIB = ifelse(test = all(is.na(ET.PriestleyTaylor)),
#                                            yes = NA,
#                                            no = sum(!is.na(ET.PriestleyTaylor))),
#         
#         ET.PriestleyTaylor = ifelse(test = ET.PriestleyTaylor_ATTRIB >= DC,
#                                     yes = mean(x = ET.PriestleyTaylor, na.rm = TRUE) %>% round(digits = 1),
#                                     no = NA),
#         
#         ET.Turc_ATTRIB = ifelse(test = all(is.na(ET.Turc)),
#                                 yes = NA,
#                                 no = sum(!is.na(ET.Turc))),
#         
#         ET.Turc = ifelse(test = ET.Turc_ATTRIB >= DC,
#                          yes = mean(x = ET.Turc, na.rm = TRUE) %>% round(digits = 1),
#                          no = NA)
#     ) %>%  mutate(ET.PenmanMonteith = ET.PenmanMonteith * 12,
#                   ET.HargreavesSamani = ET.HargreavesSamani * 12,
#                   ET.JensenHaise = ET.JensenHaise * 12,
#                   ET.Makkink = ET.Makkink * 12,
#                   ET.PriestleyTaylor = ET.PriestleyTaylor * 12,
#                   ET.Turc = ET.Turc * 12)
# 
# library(package = "ggplot2")
# 
# ggplot(data = result_yearly) + 
#     geom_line(mapping = aes(x = YEAR, y = ET.PenmanMonteith)) + 
#     geom_point(mapping = aes(x = YEAR, y = ET.PenmanMonteith, color = ET.PenmanMonteith_ATTRIB == 12)) + 
#     facet_wrap(facets = ~ STNID)
# 
# 
# 
