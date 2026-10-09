
# Initial Settings ----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "ggplot2")
library(package = "clipr")
library(package = "tidyr")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407440-99999", "407450-99999")

TimeFrame <- c('Start' = 1368, 'End' = 1396)

RBSN <- readRDS(file = "Result/ETo_RBSN_Dataset_Daily_1959_2018.rds") %>% 
    filter(STNID %in% ST) %>% 
    filter(YEAR >= TimeFrame['Start'] & YEAR <= TimeFrame['End']) %>% 
    mutate(
        JDAY = case_when(MONTH == 1 ~ (0 * 31 + DAY),
                         MONTH == 2 ~ (1 * 31 + DAY),
                         MONTH == 3 ~ (2 * 31 + DAY),
                         MONTH == 4 ~ (3 * 31 + DAY),
                         MONTH == 5 ~ (4 * 31 + DAY),
                         MONTH == 6 ~ (5 * 31 + DAY),
                         MONTH == 7 ~ (6 * 31 + DAY),
                         MONTH == 8 ~ (6 * 31 + 1 * 30 + DAY),
                         MONTH == 9 ~ (6 * 31 + 2 * 30 + DAY),
                         MONTH == 10 ~ (6 * 31 + 3 * 30 + DAY),
                         MONTH == 11 ~ (6 * 31 + 4 * 30 + DAY),
                         MONTH == 12 ~ (6 * 31 + 5 * 30 + DAY))
    )

Day_Month <- data.frame(MONTH = c(1:12),
                        DAY_MONTH = c(rep(x = 31, 6), rep(x = 30, 5), 29))

# Plot ----------------------------------------------------------------------------------------

# Plot 01 - BoxPlot - Daily ETo for Each Month

Station <- "407450-99999"

data <- RBSN %>%
    filter(STNID == Station) %>% 
    select(YEAR, MONTH, DAY, ET.HargreavesSamani) %>% 
    spread(key = MONTH, value = ET.HargreavesSamani)

write_clip(content = data)

# Plot 02 - Comparison Monthly ETo

Station <- "407450-99999"

data <- RBSN %>%
    filter(STNID == Station) %>% 
    left_join(y = Day_Month, by = 'MONTH') %>% 
    select(YEAR, MONTH, DAY_MONTH, ET.PenmanMonteith:ET.Turc) %>% 
    group_by(YEAR, MONTH) %>% 
    summarise_all(mean, na.rm = TRUE) %>% 
    mutate(ET.PenmanMonteith = ET.PenmanMonteith * DAY_MONTH,
           ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH,
           ET.JensenHaise = ET.JensenHaise * DAY_MONTH,
           ET.Makkink = ET.Makkink * DAY_MONTH,
           ET.PriestleyTaylor = ET.PriestleyTaylor * DAY_MONTH,
           ET.Turc = ET.Turc * DAY_MONTH)

write_clip(content = data)

# Plot 03 - Comparison Time Series ETo

Station <- "407440-99999"

DC <- 25

data <- RBSN %>%
    filter(STNID == Station) %>% 
    left_join(y = Day_Month, by = 'MONTH') %>% 
    select(YEAR, MONTH, DAY_MONTH, ET.PenmanMonteith:ET.Turc) %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(
        ET.PenmanMonteith_ATTRIB = ifelse(test = all(is.na(ET.PenmanMonteith)),
                                          yes = NA,
                                          no = sum(!is.na(ET.PenmanMonteith))),
        
        ET.PenmanMonteith = ifelse(test = ET.PenmanMonteith_ATTRIB >= DC,
                                   yes = (mean(x = ET.PenmanMonteith, na.rm = TRUE)) %>% round(digits = 1),
                                   no = NA),
        
        ET.HargreavesSamani_ATTRIB = ifelse(test = all(is.na(ET.HargreavesSamani)),
                                            yes = NA,
                                            no = sum(!is.na(ET.HargreavesSamani))),
        
        ET.HargreavesSamani = ifelse(test = ET.HargreavesSamani_ATTRIB >= DC,
                                     yes = (mean(x = ET.HargreavesSamani, na.rm = TRUE)) %>% round(digits = 1),
                                     no = NA),
        
        ET.JensenHaise_ATTRIB = ifelse(test = all(is.na(ET.JensenHaise)),
                                       yes = NA,
                                       no = sum(!is.na(ET.JensenHaise))),
        
        ET.JensenHaise = ifelse(test = ET.JensenHaise_ATTRIB >= DC,
                                yes = (mean(x = ET.JensenHaise, na.rm = TRUE)) %>% round(digits = 1),
                                no = NA),
        
        ET.Makkink_ATTRIB = ifelse(test = all(is.na(ET.Makkink)),
                                   yes = NA,
                                   no = sum(!is.na(ET.Makkink))),
        
        ET.Makkink = ifelse(test = ET.Makkink_ATTRIB >= DC,
                            yes = (mean(x = ET.Makkink, na.rm = TRUE)) %>% round(digits = 1),
                            no = NA),
        
        ET.PriestleyTaylor_ATTRIB = ifelse(test = all(is.na(ET.PriestleyTaylor)),
                                           yes = NA,
                                           no = sum(!is.na(ET.PriestleyTaylor))),
        
        ET.PriestleyTaylor = ifelse(test = ET.PriestleyTaylor_ATTRIB >= DC,
                                    yes = (mean(x = ET.PriestleyTaylor, na.rm = TRUE)) %>% round(digits = 1),
                                    no = NA),
        
        ET.Turc_ATTRIB = ifelse(test = all(is.na(ET.Turc)),
                                yes = NA,
                                no = sum(!is.na(ET.Turc))),
        
        ET.Turc = ifelse(test = ET.Turc_ATTRIB >= DC,
                         yes = (mean(x = ET.Turc, na.rm = TRUE)) %>% round(digits = 1),
                         no = NA)
    ) %>%
    left_join(y = Day_Month, by = 'MONTH') %>% 
    mutate(ET.PenmanMonteith = ET.PenmanMonteith * DAY_MONTH,
           ET.HargreavesSamani = ET.HargreavesSamani * DAY_MONTH,
           ET.JensenHaise = ET.JensenHaise * DAY_MONTH,
           ET.Makkink = ET.Makkink * DAY_MONTH,
           ET.PriestleyTaylor = ET.PriestleyTaylor * DAY_MONTH,
           ET.Turc = ET.Turc * DAY_MONTH)


data_mean <- data %>% 
    group_by(MONTH) %>% 
    summarise(
        ET.PenmanMonteith = mean(x = ET.PenmanMonteith, na.rm = TRUE),
        ET.HargreavesSamani = mean(x = ET.HargreavesSamani, na.rm = TRUE),
        ET.JensenHaise = mean(x = ET.JensenHaise, na.rm = TRUE),
        ET.Makkink = mean(x = ET.Makkink, na.rm = TRUE),
        ET.PriestleyTaylor = mean(x = ET.PriestleyTaylor, na.rm = TRUE),
        ET.Turc = mean(x = ET.Turc, na.rm = TRUE),
    )

Column_Names <- c("ET.PenmanMonteith",
                  "ET.HargreavesSamani",
                  "ET.JensenHaise",
                  "ET.Makkink",
                  "ET.PriestleyTaylor",
                  "ET.Turc"  )

for (c in Column_Names) {
    
    for (m in 1:12) {
        
        data[(is.na(data[, c]) & data[, 'MONTH'] == m), c] <- data_mean[data_mean[,'MONTH'] == m, c]
        
    }
    
}

data <- data %>% 
    group_by(YEAR) %>% 
    summarise(
        ET.PenmanMonteith = mean(x = ET.PenmanMonteith, na.rm = TRUE) * 12,
        ET.HargreavesSamani = mean(x = ET.HargreavesSamani, na.rm = TRUE) * 12,
        ET.JensenHaise = mean(x = ET.JensenHaise, na.rm = TRUE) * 12,
        ET.Makkink = mean(x = ET.Makkink, na.rm = TRUE) * 12,
        ET.PriestleyTaylor = mean(x = ET.PriestleyTaylor, na.rm = TRUE) * 12,
        ET.Turc = mean(x = ET.Turc, na.rm = TRUE) * 12,
    )

write_clip(content = data)




# Plot 02 - BoxPlot - Monthly ETo

Station <- "407440-99999"

data <- RBSN %>%
    filter(STNID == Station) %>% 
    select(YEAR, MONTH, DAY, ET.PenmanMonteith, ET.HargreavesSamani) %>% 
    group_by(YEAR, MONTH) %>% 
    summarise(ET_ATTRIB = ifelse(test = all(is.na(ET.PenmanMonteith)),
                                                yes = NA,
                                                no = sum(!is.na(ET.PenmanMonteith))),
                          
                          ET.PenmanMonteith = ifelse(test = ET.PenmanMonteith_ATTRIB >= DC,
                                                     yes = mean(x = ET.PenmanMonteith, na.rm = TRUE) %>% round(digits = 1),
                                                     no = NA),)
    
    spread(key = MONTH, value = ET.PenmanMonteith)

write_clip(content = data)










data <- data.frame(
    YEAR = RBSN$YEAR,
    MONTH = RBSN$MONTH,
    ETo_PM = RBSN$ET.PenmanMonteith,
    ETo_HS = RBSN$ET.HargreavesSamani,
    ETo_TerraClimate = TerraClimate$VALUE
)

data %>% 
    # group_by(MONTH) %>% 
    # summarise_all(mean, na.rm = TRUE) %>% 
    ggplot() +
    # geom_point(mapping = aes(x = ETo_PM, y = ETo_HS), color = "red") +
    geom_point(mapping = aes(x = ETo_PM, y = ETo_TerraClimate), color = "green") +
    facet_wrap(~ MONTH)





TerraClimate <- readRDS(file = "Result/ETo_TerraClimate_Dataset_Monthly_1337_1398.rds") %>% 
    filter(STNID == "407450-99999") %>% 
    filter(YEAR >= 1368 & YEAR <= 1396)

data <- data.frame(
    YEAR = RBSN$YEAR,
    MONTH = RBSN$MONTH,
    ETo_PM = RBSN$ET.PenmanMonteith,
    ETo_HS = RBSN$ET.HargreavesSamani,
    ETo_TerraClimate = TerraClimate$VALUE
)

data %>% 
    # group_by(MONTH) %>% 
    # summarise_all(mean, na.rm = TRUE) %>% 
    ggplot() +
    # geom_point(mapping = aes(x = ETo_PM, y = ETo_HS), color = "red") +
    geom_point(mapping = aes(x = ETo_PM, y = ETo_TerraClimate), color = "green") +
    facet_wrap(~ MONTH)
    
