
############### Information ###############





############### Initial Setting ###############

#  Set Aspects of the Locale
Sys.setlocale(locale = "Persian")

# Reads the Fonttable Database and Registers Those Fonts with R
extrafont::loadfonts(device = "win")

# Load My Functions
source("Code/func.R")

# Load Required Packages
library(package = "dplyr")
library(package = "ggplot2")
library(package = "lubridate")
library(package = "ggridges")
library(package = "viridis")
library(package = "hrbrthemes")
library(package = "raster")
library(package = "rasterVis")
library(package = "rgdal")
library(package = "leaflet")
library(package = "maps")

# Load Data
data <- read.csv(file = "Dataset/ProcessedData/Extract_Raster_PET_Shapefile_Shamsi_100m.csv",
                 stringsAsFactors = FALSE)

############### Data Cleansing ###############

YEAR.MIN <- min(data$YEAR)
YEAR.MAX <- max(data$YEAR)

############### Plot PET ###############

# NOTE: PLot Scale: 1800 x 1200

#########################################
# Monthly PET Anomaly - Shapefile Mashhad
#########################################

lable.x <- "ماه"
lable.y <- "تبخیرتعرق پتانسیل (میلیمتر در ماه)"
month.name <- c("فروردین",
                "اردیبهشت",
                "خرداد",
                "تیر",
                "مرداد",
                "شهریور",
                "مهر",
                "آبان",
                "آذر",
                "دی",
                "بهمن",
                "اسفند")

data %>%
  filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
  group_by(MONTH) %>%
  summarise(VALUE = mean(x = VALUE)) %>%
  ggplot(mapping = aes(x = MONTH, y = VALUE)) +
  theme_classic() +
  geom_bar(stat = "identity", color = "black", fill = "black") +
  xlab(label = "") +
  ylab(label = lable.y) +
  ylim(c(0,250)) +
  scale_x_continuous(breaks = 1:12, labels = month.name) +
  theme(text = element_text(size = 60, family = "B Zar", color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))



# NOTE: PLot Scale: 1800 x 1200

#################################################
# Monthly PET Anomaly Boxplot - Shapefile Mashhad
#################################################
lable.x <- "ماه"
lable.y <- "تبخیرتعرق پتانسیل (میلیمتر در ماه)"
month.name <- c("فروردین",
                "اردیبهشت",
                "خرداد",
                "تیر",
                "مرداد",
                "شهریور",
                "مهر",
                "آبان",
                "آذر",
                "دی",
                "بهمن",
                "اسفند")

data %>%
  filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
  ggplot(mapping = aes(x = as.factor(MONTH), y = VALUE)) +
  theme_classic() +
  geom_boxplot(color = "black", size = 1.4, outlier.shape = NA) +
  geom_jitter(width = 0.2, color = "black", size = 3) +
  xlab(label = "") +
  ylab(label = lable.y) +
  ylim(c(0,250)) +
  scale_x_discrete(breaks = 1:12, labels = month.name) +
  theme(text = element_text(size = 60, family = "B Zar", color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0,
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))



# NOTE: PLot Scale: 1800 x 1200

###########################################
# Timeserise Yearly PET - Shapefile Mashhad
###########################################

lable.x <- "سال"
lable.y <- "تبخیرتعرق پتانسیل (میلیمتر در سال)"

data %>%
  filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
  group_by(YEAR) %>%
  summarise(VALUE = sum(x = VALUE)) %>%
  ggplot(mapping = aes(x = YEAR, y = VALUE)) +
  theme_classic() +
  geom_line(size = 2) +
  geom_point(size = 10) +
  xlab(label = lable.x) +
  ylab(label = lable.y) +
  ylim(c(1000, 1800)) +
  scale_x_continuous(breaks = seq(YEAR.MIN + 1, YEAR.MAX - 1, 2)) +
  theme(text = element_text(size = 60, family = "B Zar", color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(margin = margin(t = 0, r = 40, b = 0, l = 0)))



# NOTE: PLot Scale: 1800 x 1200

############################################
# Ridgelines Monthly PET - Shapefile Mashhad
############################################
lable.x <- "تبخیرتعرق پتانسیل (میلیمتر در ماه)"
lable.y <- "ماه"
month.name <- c("فروردین",
                "اردیبهشت",
                "خرداد",
                "تیر",
                "مرداد",
                "شهریور",
                "مهر",
                "آبان",
                "آذر",
                "دی",
                "بهمن",
                "اسفند")

data %>%
  filter(YEAR > YEAR.MIN & YEAR < YEAR.MAX) %>%
  ggplot(aes(x = VALUE, y = as.factor(MONTH), fill = ..x..)) +
  geom_density_ridges_gradient(scale = 2, rel_min_height = 0.0) +
  scale_fill_viridis(name = "", option = "C") +
  theme_classic() +
  xlab(label = lable.x) +
  ylab(label = "") +
  xlim(c(0, 250)) +
  scale_y_discrete(breaks = 1:12, labels = month.name) +
  theme(legend.position = "none",
        text = element_text(size = 60, family = "B Zar", color = "black"),
        axis.text = element_text(color = "black"),
        axis.line = element_line(color = "black"),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5,
                                   margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)),
        legend.text = element_text(size = 20, family = "B Zar", color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        plot.margin = unit(c(0.5, 0.5, 1, 0.5),"cm"))


# NOTE: PLot Scale: 1800 x 1200

#####################################
# Map Monthly PET - Shapefile Mashhad
#####################################

# Load PET Raster Data
raw.data <- readRDS(file = "Dataset/ProcessedData/TerraClimate_Mashhad_Raster_1989_2018_100m.rds")

raw.data <- rep(x = raw.data, each = 3) %>%
  lapply(FUN = function(x) x / 3)

names(x = raw.data) <- paste0(names(x = raw.data), "_", c(5, 15, 25))


# Create Name raw.data to Shamsi Dec
shamsi.name.dec <- c()

for (i in 1:length(x = raw.data)) {

  n = strsplit(x = names(x = raw.data)[i], split = "_")[[1]]
  dt = paste0(n[2], "-", match(x = n[3], table = month.abb), "-", n[4])
  dt.persian = Gregorian(dt)$persian_string()
  year = as.numeric(str_split(string = dt.persian, pattern = "-")[[1]][1])
  month = as.numeric(str_split(string = dt.persian, pattern = "-")[[1]][2])
  shamsi.name.dec <- c(shamsi.name.dec, paste0(year, "_", month, ".", n[4]))

}

names(x = raw.data) <- shamsi.name.dec


# Create shamsi.data
shamsi.name.month <- str_remove(string = shamsi.name.dec, pattern = "\\.+\\d+")

shamsi.data <- list()

for (i in unique(x = shamsi.name.month)) {

  shamsi.data[[i]] <- Reduce(f = "+", x = raw.data[shamsi.name.dec[str_starts(string = shamsi.name.dec,
                                                                              pattern = paste0(i, "\\."))]])

}

rm(raw.data)

shamsi.name.month <- names(x = shamsi.data)

anomaly.data <- list()

for (i in 1:12) {

  d <- shamsi.name.month[str_ends(string = shamsi.name.month, pattern = paste0("_", i))]
  if (i < 10) {
    nameList = paste0(0, i)
  } else {
    nameList = paste0(i)
  }
  anomaly.data[[nameList]] <- Reduce(f = "+", x = shamsi.data[d]) / length(x = d)

}

rm(shamsi.data)

# Load Shapefile Mashhad
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Shapefile.Path <- "Dataset/ProcessedData/Mashhad_City_Layers/Limit/ShapeFile"
Shapefile.Name <- "Limit"
Mashhad <- rgdal::readOGR(dsn = Shapefile.Path,
                          layer = Shapefile.Name) %>%
  sp::spTransform(CRSobj = sp::CRS(CRS.Original))

Regions <- rgdal::readOGR(dsn = "Dataset/ProcessedData/Mashhad_City_Layers/Region/ShapeFile",
                          layer = "Regions") %>%
  sp::spTransform(CRSobj = sp::CRS(CRS.Original))

data.plot <- lapply(X = anomaly.data,
                    FUN = raster::crop, y = raster::extent(x = Mashhad) + 0.01) %>%
  lapply(FUN = raster::mask, mask = Mashhad)


dp <- data.plot$`04`

pal <- colorNumeric(palette = c("#2980B9", "#F1C40F", "#C0392B"),
                    domain = values(x = dp),
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
  # addPolygons(data = Regions,
  #             color = "black",
  #             weight = 1,
  #             fill = FALSE) %>%
  addLegend(pal = pal,
            values = values(x = dp),
            title = "")
