

# Initial Setting -----------------------------------------------------------------------------

# Remove Objects from a Specified Environment
rm(list = ls())

#  Set Aspects of the Locale
Sys.setlocale(locale = "Persian")

# Reads the Fonttable Database and Registers Those Fonts with R
extrafont::loadfonts(device = "win")

# Load Required Packages
library(package = "dplyr")
library(package = "ggplot2")
library(package = "lubridate")
library(package = "stringr")

library(package = "ggridges")
library(package = "viridis")
library(package = "hrbrthemes")
library(package = "raster")
library(package = "rasterVis")
library(package = "rgdal")
library(package = "leaflet")
library(package = "maps")

# Load Data
error_data <- read.csv(file = "Result/TerraClimate/ETo_Error.csv") %>% 
    filter(STNID == "407450-99999") %>% 
    dplyr::select(-STNID)

data <- read.csv(file = "Result/TerraClimate/Extract_Raster_PET_Region_400m.csv",
                 stringsAsFactors = FALSE) %>% 
    rename("MONTH_ABB" = "MONTH") %>% 
    mutate(MONTH = match(x = MONTH_ABB, table = month.abb)) %>% 
    arrange(Region, YEAR, MONTH) %>% 
    left_join(y = error_data, by = "MONTH") %>% 
    mutate(PET_adj = PET + ETo.Error)

# Plot ----------------------------------------------------------------------------------------

# NOTE: PLot Scale: 1800 x 1200

############################################
# Ridgelines Monthly PET - Regions Mashhad
############################################

YEAR.MIN = 1989
YEAR.MAX = 2018

data %>%
    filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
    ggplot(aes(x = PET_adj, y = as.factor(MONTH), fill = ..x..)) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.0) +
    scale_fill_viridis(name = "", option = "C") +
    theme_classic() +
    xlab(label = "ETo (mm/month)") +
    ylab(label = "") +
    scale_y_discrete(breaks = 1:12, labels = month.abb) +
    facet_wrap(~ Region)


#########################################
# Monthly PET Anomaly - Regions Mashhad
#########################################

data %>%
    filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
    group_by(Region, MONTH) %>%
    summarise(PET = mean(x = PET_adj, na.rm = TRUE)) %>%
    ggplot(mapping = aes(x = MONTH, y = PET)) +
    theme_classic() +
    geom_bar(stat = "identity", color = "black", fill = "black") +
    xlab(label = "Month") +
    ylab(label = "ETo (mm/month)") +
    ylim(c(0,300)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) +
    facet_wrap(~ Region)

#####################################
# Map Monthly PET - Region Mashhad
#####################################

# Load PET Raster Data
data_raw <- readRDS(file = "Dataset/TerraClimate_PET_Mashhad_Raster_1958_2019_400m.rds")

error_data <- read.csv(file = "Result/TerraClimate/ETo_Error.csv") %>% 
    filter(STNID == "407450-99999") %>% 
    dplyr::select(-STNID)

data <- list()

for (n in names(x = data_raw)) {
    
    Year <- as.numeric(x = str_split(string = n, pattern = "_")[[1]][2])
    
    Month <- match(x = str_split(string = n, pattern = "_")[[1]][3],
                   table = month.abb)
    
    if (Year >= 1989 & Year <= 2018) {
        
        data[[paste0(Year, "_", Month)]] <- data_raw[[n]]
        
    }
    
}

anomaly_data <- list()

for (m in 1:12) {
    
    Month <- ifelse(test = m < 10,
                    yes = paste0(0, m),
                    no = paste0(m))
    
    df <- list()
    
    for (n in names(x = data)) {
        
        mm <- as.numeric(x = str_split(string = n, pattern = "_")[[1]][2])
        
        if (mm == m) {
            
            df[[n]] <- data[[n]]
            
        }
        
    }
    
    anomaly_data[[Month]] <- (Reduce(f = "+", x = df) / length(x = df)) + error_data[m, "ETo.Error"]
    
}
    
# Load Shapefile Mashhad
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Mashhad <- rgdal::readOGR(dsn = choose.dir(),
                          layer = "Limit") %>%
    sp::spTransform(CRSobj = sp::CRS(CRS.Original))

Regions <- rgdal::readOGR(dsn = choose.dir(),
                          layer = "Regions") %>%
    sp::spTransform(CRSobj = sp::CRS(CRS.Original))

data_plot <- lapply(X = anomaly_data,
                    FUN = raster::crop, y = raster::extent(x = Mashhad) + 0.01) %>%
    lapply(FUN = raster::mask, mask = Mashhad)


dp <- data_plot$`07`
minValue(dp)
maxValue(dp)

pal <- colorNumeric(palette = c("#2980B9", "#F1C40F", "#C0392B"),
                    domain = c(250, 290),
                    na.color = "transparent")


leaflet() %>%
    addTiles() %>%
    addProviderTiles(provider = "JusticeMap.multi") %>%
    addRasterImage(x = dp,
                   opacity = 0.7,
                   colors = pal) %>%
    addPolygons(data = Mashhad,
                color = "black",
                weight = 2,
                fill = FALSE) %>%
    addPolygons(data = Regions,
                color = "white",
                weight = 2,
                fill = FALSE) %>%
    addLegend(pal = pal,
              values = c(250, 290),
              title = "")






data <- read.csv(file = "Result/TerraClimate/Extract_Raster_PET_adj_Limit_400m.csv",
                 stringsAsFactors = FALSE) %>% 
    arrange(YEAR, MONTH) %>% 
    filter(YEAR >= 1989 & YEAR <= 2018) %>% 
    mutate(MONTH = factor(x = MONTH))

Day_Month <- data.frame(
    MONTH = factor(x = 1:12),
    Day_Month = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
)

data %>%
    left_join(y = Day_Month, by = "MONTH") %>% 
    ggplot(mapping = aes(x = MONTH, y = PET_adj)) +
    theme_classic() +
    geom_boxplot() +
    stat_summary(fun.y = mean,
                 colour = "red",
                 geom = "point",
                 shape = 18,
                 size = 2,
                 show_guide = FALSE) +
    scale_y_continuous(limit = c(0, 310), breaks = seq.int(from = 0, to = 310, by = 50)) +
    labs(x = "", 
         y = "ETo (mm/month)") +
    scale_x_discrete(breaks = 1:12, labels = month.abb) +
    theme(text = element_text(size = 18, color = "black"),
          axis.text = element_text(color = "black"),
          axis.line = element_line(color = "black"),
          axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,
                                     margin = margin(t = 10, r = 0, b = 0, l = 0)),
          axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
          panel.border = element_rect(colour = "black", fill = NA, size = 1))



