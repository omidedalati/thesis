
# Initial Setting -----------------------------------------------------------------------------

rm(list = ls())

library(package = "dplyr")
library(package = "lubridate")
library(package = "stringr")
library(package = "raster")
library(package = "sp")
library(package = "rgdal")
library(package = "pbapply")
library(package = "tcltk")
library(package = "plotrix")


# Load Data -----------------------------------------------------------------------------------

# Load Raster Data
if (exists('utils::choose.dir')) {
  data <- readRDS(file = choose.file(caption = "Select Raster File: "))
  Folder.Output <- choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  data <- readRDS(file = tk_choose.files(caption = "Select Raster File: "))
  Folder.Output <- tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

# Load Shapefile Khorasan Razavi
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Shapefile.Path <- tk_choose.dir(caption = "Select A Folder Contain Limit Shapefile: ")
Shapefile.Name <- "Khorasan_Razavi"
Khorasan_Razavi <- rgdal::readOGR(dsn = Shapefile.Path,
                                  layer = Shapefile.Name) %>%
  sp::spTransform(CRSobj = sp::CRS(CRS.Original))


# Crop Khorasan Razavi ------------------------------------------------------------------------

data <- pbapply::pblapply(X = data,
                          FUN = raster::crop, y = raster::extent(x = Khorasan_Razavi) + 0.2)

# Load Station Info
ST <- read.csv(file = "Code/TerraClimate/Stations_Info.csv",
               colClasses = c("character", "character", "numeric", "numeric", "numeric"))

ST$ID <- as.character(x = c(1:nrow(x = ST)))

# Load RBSN Stations --------------------------------------------------------------------------

# Load Coordinate Reference System
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create Objects of Class SpatialPointsDataFrame
coords <- ST %>% dplyr::select(LON, LAT)
df     <- ST %>% dplyr::select(-one_of(c("LON", "LAT", "NAME")))
crs    <- sp::CRS(CRS.Original)

st <- sp::SpatialPointsDataFrame(coords = coords,
                                 data = df,
                                 proj4string = crs)

# Extract PET Value of Each Stations From Raster ----------------------------------------------

result <- list()

extracted_value <- do.call(what = rbind,
                           args = pbapply::pblapply(X = data,
                                                    FUN = raster::extract, y = st, df = TRUE))

colnames(x = extracted_value) <- c("ID", "PET")

# Result Cleansing
extracted_value <- extracted_value %>%
  dplyr::mutate(DATE = rownames(.)) %>%
  tidyr::separate(col = DATE, into = c("VARIABLE", "YEAR", "MONTH.ID"), sep = "\\_") %>%
  tidyr::separate(col = MONTH.ID, into = c("MONTH", "ID"), sep = "\\.") %>%
  dplyr::select(ID, YEAR, MONTH, PET) %>%
  dplyr::left_join(x = ST %>% dplyr::select(-NAME), y = ., by = "ID") %>%
  dplyr::mutate(MONTH = match(x = .$MONTH, table = month.abb))

extracted_value <- extracted_value %>%
  arrange(STNID, YEAR, MONTH)

extracted_value$YEAR <- as.numeric(x = extracted_value$YEAR)

extracted_value <- extracted_value %>%
  filter(YEAR >= 1959 & YEAR <= 2018)

# Load RBSN ETo -------------------------------------------------------------------------------

RBSN_ETo <- read.csv(file = "Result/RefET/ETo_RBSN_RefET_Monthly.csv") %>% 
  dplyr::select(STNID, YEAR, MONTH, ASCE_PM, FAO56_PM, HS, PT, MAKK, TURC) %>% 
  filter(YEAR >= 1959 & YEAR <= 2018) %>% 
  rename("ASCE Penman-Monteith" = "ASCE_PM",
         "FAO 56 Penman-Monteith" = "FAO56_PM",
         "Hargreaves-Samani" = "HS",
         "Priestley-Taylor" = "PT",
         "Makkink" = "MAKK",
         "Turc" = "TURC")

# Join Two Dataset ----------------------------------------------------------------------------

Dataset <- RBSN_ETo %>% 
  left_join(y = extracted_value, by = c('STNID', 'YEAR', 'MONTH')) %>% 
  mutate(DATE = as.Date(x = paste0(YEAR, "-", MONTH, "-", "15"))) %>% 
  dplyr::select(ID, STNID, LAT, LON, ELEV, DATE, YEAR, MONTH, PET, everything()) %>% 
  rename("TerraClimate PET" = "PET")

Dataset$ID <- factor(x = Dataset$ID)
Dataset$STNID <- factor(x = Dataset$STNID)
Dataset$YEAR <- factor(x = Dataset$YEAR)
Dataset$MONTH <- factor(x = Dataset$MONTH)

# Plot ----------------------------------------------------------------------------------------

# Plot 01 - Comparison Monthly ETo

colors <- c("ASCE Penman-Monteith" = "black",
            "Hargreaves-Samani" = "red",
            "Priestley-Taylor" = "green",
            "Makkink" = "blue",
            "Turc" = "orange",
            "TerraClimate PET" = "darkorchid4")

Dataset %>%
  group_by(STNID, MONTH) %>% 
  summarise_all(mean, na.rm = TRUE) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = MONTH, y = `FAO 56 Penman-Monteith`), stat = 'identity', color = "black", fill = "grey80", width = 0.8) +
  geom_line(mapping = aes(x = MONTH, y = `Hargreaves-Samani`, group = 1, color = "Hargreaves-Samani"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `Hargreaves-Samani`, color = "Hargreaves-Samani"), size = 2, shape = 19) +
  geom_line(mapping = aes(x = MONTH, y = `Priestley-Taylor`, group = 1, color = "Priestley-Taylor"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `Priestley-Taylor`, color = "Priestley-Taylor"), size = 2, shape = 19) +
  geom_line(mapping = aes(x = MONTH, y = `Makkink`, group = 1, color = "Makkink"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `Makkink`, color = "Makkink"), size = 2, shape = 19) +
  geom_line(mapping = aes(x = MONTH, y = `Turc`, group = 1, color = "Turc"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `Turc`, color = "Turc"), size = 2, shape = 19) +
  geom_line(mapping = aes(x = MONTH, y = `ASCE Penman-Monteith`, group = 1, color = "ASCE Penman-Monteith"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `ASCE Penman-Monteith`, color = "ASCE Penman-Monteith"), size = 2, shape = 19) +
  geom_line(mapping = aes(x = MONTH, y = `TerraClimate PET`, group = 1, color = "TerraClimate PET"), size = 0.5) +
  geom_point(mapping = aes(x = MONTH, y = `TerraClimate PET`, color = "TerraClimate PET"), size = 2, shape = 19) +
  facet_wrap(facets = ~ STNID, nrow = 2) +
  scale_y_continuous(limit = c(0, 400),
                     breaks = seq.int(from = 0, to = 400, by = 50)) +
  labs(x = "Month", 
       y = "ETo (mm/month)",
       color = "ETo Methods") +
  scale_color_manual(values = colors)

# Plot 02 - Scatter Plot

colors <- c("FAO 56 Penman-Monteith" = "darkorchid4",
            "Hargreaves-Samani" = "black",
            "TerraClimate PET" = "red")

Dataset %>%
  ggplot() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(mapping = aes(x = `FAO 56 Penman-Monteith`, y = `Hargreaves-Samani`, color = "Hargreaves-Samani"), size = 1, shape = 19) +
  geom_point(mapping = aes(x = `FAO 56 Penman-Monteith`, y = `TerraClimate PET`, color = "TerraClimate PET"), size = 1, shape = 19) +
  facet_wrap(facets = ~ STNID, nrow = 2) +
  scale_y_continuous(limit = c(0, 400),
                     breaks = seq.int(from = 0, to = 400, by = 100)) +
  labs(x = "FAO 56 Penman-Monteith (mm/month)", 
       y = "ETo Methods (mm/month)",
       color = "ETo Methods") +
  scale_color_manual(values = colors) +
  theme(aspect.ratio = 1)

# Plot 03 - Taylor Diagram

df <- Dataset %>% 
  dplyr::filter(STNID == "408060-99999")


taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`ASCE Penman-Monteith`,
               col = "black", main = "", pos.cor = TRUE)

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`Hargreaves-Samani`,
               add = TRUE,
               col = "red")

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`Priestley-Taylor`,
               add = TRUE,
               col = "green")

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$Makkink,
               add = TRUE,
               col = "blue")

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$Turc,
               add = TRUE,
               col = "orange")

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`TerraClimate PET`,
               add = TRUE,
               col = "darkorchid4")

legend(x = 80,
       y = 188,
       legend = c("ASCE Penman-Monteith", "Hargreaves-Samani", "Priestley-Taylor", "Makkink", "Turc", "TerraClimate PET"),
       pch = 19,
       col = c("black", "red", "green", "blue", "orange", "darkorchid4"),
       bty = "n")

# Create Raster Error -------------------------------------------------------------------------

Dataset_Error <- Dataset %>% 
  dplyr::filter(year(x = DATE) >= 1989 & year(x = DATE) <= 2018) %>% 
  dplyr::select(ID, STNID, LAT, LON, ELEV, DATE, YEAR, MONTH, `TerraClimate PET`, `FAO 56 Penman-Monteith`) %>% 
  mutate(`Delta ETo` = `FAO 56 Penman-Monteith` - `TerraClimate PET`)

ETo_Error <- Dataset_Error %>% 
  group_by(STNID, MONTH) %>% 
  summarise(`ETo Error` = mean(`Delta ETo`, na.rm = TRUE))

Dataset_Error <- Dataset_Error %>% 
  left_join(y = ETo_Error, by = c("STNID", "MONTH")) %>% 
  mutate(`TerraClimate PET Modified` = `TerraClimate PET` + `ETo Error`)

# Plot Taylor Diagram

df <- Dataset_Error %>% 
  dplyr::filter(STNID == "408060-99999")

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`TerraClimate PET`,
               col = "black", main = "", pos.cor = TRUE)

taylor.diagram(ref = df$`FAO 56 Penman-Monteith`,
               model = df$`TerraClimate PET Modified`,
               add = TRUE,
               col = "red")

legend(x = 80,
       y = 200,
       legend = c("TerraClimate PET", "TerraClimate PET Modified"),
       pch = 19,
       col = c("black", "red"),
       bty = "n")

write.csv(x = ETo_Error, file = "Result/TerraClimate/ETo_Error.csv", row.names = FALSE)




