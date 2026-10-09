
# Initial Settings ----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "ggplot2")
library(package = "clipr")
library(package = "tidyr")

# Load Data -----------------------------------------------------------------------------------

ST <- c("407440-99999", "407450-99999")

TimeFrame <- c('Start' = 1368, 'End' = 1396)

# Daily Data

RefET_Daily <- read.csv(file = "Result/RefET/ETo_RBSN_RefET_Daily.csv") %>% 
    mutate(Shamsi_JDay = case_when(MONTH == 1 ~ (0 * 31 + Shamsi_Day),
                                   MONTH == 2 ~ (1 * 31 + Shamsi_Day),
                                   MONTH == 3 ~ (2 * 31 + Shamsi_Day),
                                   MONTH == 4 ~ (3 * 31 + Shamsi_Day),
                                   MONTH == 5 ~ (4 * 31 + Shamsi_Day),
                                   MONTH == 6 ~ (5 * 31 + Shamsi_Day),
                                   MONTH == 7 ~ (6 * 31 + Shamsi_Day),
                                   MONTH == 8 ~ (6 * 31 + 1 * 30 + Shamsi_Day),
                                   MONTH == 9 ~ (6 * 31 + 2 * 30 + Shamsi_Day),
                                   MONTH == 10 ~ (6 * 31 + 3 * 30 + Shamsi_Day),
                                   MONTH == 11 ~ (6 * 31 + 4 * 30 + Shamsi_Day),
                                   MONTH == 12 ~ (6 * 31 + 5 * 30 + Shamsi_Day))) %>% 
    select(STNID, YEAR, MONTH, DAY, Shamsi_Year, Shamsi_Month, Shamsi_Day, Shamsi_JDay, everything()) %>% 
    filter(STNID %in% ST) %>% 
    filter(Shamsi_Year >= TimeFrame['Start'] & Shamsi_Year <= TimeFrame['End']) %>%
    reshape2::melt(id = c('STNID', 'YEAR', 'MONTH', 'DAY', 'Shamsi_Year', 'Shamsi_Month', 'Shamsi_Day', 'Shamsi_JDay'),
                   variable.name = "Variable",
                   value.name = "Value") %>% 
    mutate(YEAR = factor(x = YEAR),
           MONTH = factor(x = MONTH),
           DAY = factor(x = DAY),
           Shamsi_Year = factor(x = Shamsi_Year),
           Shamsi_Month = factor(x = Shamsi_Month),
           Shamsi_Day = factor(x = Shamsi_Day),
           Shamsi_JDay = factor(x = Shamsi_JDay))

RefET_Daily$STNID <- factor(x = RefET_Daily$STNID,
                            levels = c("407440-99999", "407450-99999"),
                            labels = c("Golmakan", "Mashhad Airport"))

RefET_Daily$Variable <- factor(x = RefET_Daily$Variable,
                               levels = c("TX", "TN", "nN", "WS", "DP", "ASCE_PM", "FAO56_PM", "HS", "PT", "MAKK", "TURC", "PRECIP"),
                               labels = c("Maximum Temperature", "Minimum Temperature", "nN", "Wind Speed", "DewPoint Temperature",
                                          "ASCE Penman-Monteith", "FAO 56 Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc",
                                          "Precipitation"))

# Monthly Data

RefET_Monthly <- read.csv(file = "Result/RefET/ETo_RBSN_RefET_Monthly_Shamsi.csv") %>% 
    select(STNID, Shamsi_Year, Shamsi_Month, TX, TN, nN, WS, DP, PRECIP, ASCE_PM, FAO56_PM, HS, PT, MAKK, TURC) %>% 
    filter(STNID %in% ST) %>% 
    filter(Shamsi_Year >= TimeFrame['Start'] & Shamsi_Year <= TimeFrame['End']) %>%
    reshape2::melt(id = c('STNID', 'Shamsi_Year', 'Shamsi_Month'),
                   variable.name = "Variable",
                   value.name = "Value") %>% 
    mutate(Shamsi_Year = factor(x = Shamsi_Year),
           Shamsi_Month = factor(x = Shamsi_Month))

RefET_Monthly$STNID <- factor(x = RefET_Monthly$STNID,
                            levels = c("407440-99999", "407450-99999"),
                            labels = c("Golmakan", "Mashhad Airport"))

RefET_Monthly$Variable <- factor(x = RefET_Monthly$Variable,
                               levels = c("TX", "TN", "nN", "WS", "DP", "ASCE_PM", "FAO56_PM", "HS", "PT", "MAKK", "TURC", "PRECIP"),
                               labels = c("Maximum Temperature", "Minimum Temperature", "nN", "Wind Speed", "DewPoint Temperature",
                                          "ASCE Penman-Monteith", "FAO 56 Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc",
                                          "Precipitation"))

# Plot ----------------------------------------------------------------------------------------

# Plot 01 - BoxPlot - Daily ETo for Each Month

RefET_Daily %>%
    filter(Variable %in% c("ASCE Penman-Monteith", "FAO 56 Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc")) %>% 
    ggplot(mapping = aes(x = Shamsi_Month, y = Value)) +
    geom_boxplot() +
    stat_summary(fun.y = mean,
                 colour = "red",
                 geom = "point",
                 shape = 18,
                 size = 2,
                 show_guide = FALSE) +
    facet_grid(facets = Variable ~ STNID) +
    scale_y_continuous(limit = c(0, 14), breaks = seq.int(from = 0, to = 14, by = 2)) +
    labs(x = "Month", 
         y = "ETo (mm/day)")

# Plot 02 - Comparison Monthly ETo

colors <- c("ASCE Penman-Monteith" = "black",
            "Hargreaves-Samani" = "red",
            "Priestley-Taylor" = "green",
            "Makkink" = "blue",
            "Turc" = "orange")


RefET_Monthly %>%
    filter(Variable %in% c("FAO 56 Penman-Monteith", "ASCE Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc")) %>% 
    group_by(STNID, Variable, Shamsi_Month) %>% 
    summarise(Value = mean(x = Value, na.rm = TRUE)) %>% 
    spread(key = Variable, value = Value) %>% 
    ggplot() +
    geom_bar(mapping = aes(x = Shamsi_Month, y = `FAO 56 Penman-Monteith`), stat = 'identity', color = "black", fill = "grey80", width = 0.8) +
    geom_line(mapping = aes(x = Shamsi_Month, y = `Hargreaves-Samani`, group = 1, color = "Hargreaves-Samani"), size = 0.5) +
    geom_point(mapping = aes(x = Shamsi_Month, y = `Hargreaves-Samani`, color = "Hargreaves-Samani"), size = 2, shape = 19) +
    geom_line(mapping = aes(x = Shamsi_Month, y = `Priestley-Taylor`, group = 1, color = "Priestley-Taylor"), size = 0.5) +
    geom_point(mapping = aes(x = Shamsi_Month, y = `Priestley-Taylor`, color = "Priestley-Taylor"), size = 2, shape = 19) +
    geom_line(mapping = aes(x = Shamsi_Month, y = `Makkink`, group = 1, color = "Makkink"), size = 0.5) +
    geom_point(mapping = aes(x = Shamsi_Month, y = `Makkink`, color = "Makkink"), size = 2, shape = 19) +
    geom_line(mapping = aes(x = Shamsi_Month, y = `Turc`, group = 1, color = "Turc"), size = 0.5) +
    geom_point(mapping = aes(x = Shamsi_Month, y = `Turc`, color = "Turc"), size = 2, shape = 19) +
    geom_line(mapping = aes(x = Shamsi_Month, y = `ASCE Penman-Monteith`, group = 1, color = "ASCE Penman-Monteith"), size = 0.5) +
    geom_point(mapping = aes(x = Shamsi_Month, y = `ASCE Penman-Monteith`, color = "ASCE Penman-Monteith"), size = 2, shape = 19) +
    facet_wrap(facets = ~ STNID) +
    scale_y_continuous(limit = c(0, 300),
                       breaks = seq.int(from = 0, to = 300, by = 50)) +
    labs(x = "Month", 
         y = "ETo (mm/month)",
         color = "ETo Methods") +
    scale_color_manual(values = colors)

# Plot 03 - Comparison Time Series ETo

variable <-  c("FAO 56 Penman-Monteith", "ASCE Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc")

RefET_Monthly_Mean <- RefET_Monthly %>% 
    group_by(STNID, Variable, Shamsi_Month) %>% 
    summarise(Value = mean(x = Value, na.rm = TRUE)) %>% 
    as.data.frame()

for (s in unique(x = RefET_Monthly_Mean$STNID)) {
    
    for (v in unique(x = RefET_Monthly_Mean$Variable)) {
        
        for (m in unique(x = RefET_Monthly_Mean$Shamsi_Month)) {
            
            RefET_Monthly[(is.na(RefET_Monthly[, "Value"]) & RefET_Monthly[, 'STNID'] == s & RefET_Monthly[, 'Variable'] == v & RefET_Monthly[, 'Shamsi_Month'] == m), "Value"] <-
                RefET_Monthly_Mean[(RefET_Monthly_Mean[, 'STNID'] == s & RefET_Monthly_Mean[, 'Variable'] == v & RefET_Monthly_Mean[, 'Shamsi_Month'] == m), "Value"] 
            
        }
        
    }
    
}

RefET_Monthly %>%
    filter(Variable %in% variable) %>% 
    group_by(STNID, Variable, Shamsi_Year) %>% 
    summarise(Value = sum(Value, na.rm = FALSE)) %>% 
    ggplot() +
    geom_line(mapping = aes(x = Shamsi_Year, y = Value, group = Variable, color = Variable)) +
    geom_point(mapping = aes(x = Shamsi_Year, y = Value, group = Variable, color = Variable)) +
    facet_wrap(~ STNID) +
    scale_y_continuous(limit = c(750, 2000),
                       breaks = seq.int(from = 800, to = 2000, by = 400)) +
    scale_x_discrete(breaks = seq.int(from = 1368, to = 1398, by = 2)) + 
    labs(x = "Year", 
         y = "ETo (mm/year)",
         color = "ETo Methods") +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5))
