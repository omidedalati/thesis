
# Initial Setting -----------------------------------------------------------------------------

# Remove Objects from a Specified Environment
rm(list = ls())

# Load Required Packages
library(package = "extrafont")
library(package = "raster")
library(package = "sp")
library(package = "rgdal")
library(package = "dplyr")
library(package = "tcltk")
library(package = "stringr")
library(package = "pbapply")
library(package = "leaflet")

# Select Folder To Save Output Files
if (exists('utils::choose.dir')) {
  Folder.Output <- choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  Folder.Output <- tcltk::tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

# Load PET Raster Data
DATA <- readRDS(file = tk_choose.files(caption = "Select Raster File: "))

error_data <- read.csv(file = "Result/TerraClimate/ETo_Error.csv") %>% 
  filter(STNID == "407450-99999") %>% 
  dplyr::select(-STNID)

# Load Shapefile Mashhad
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

Shapefile.Path <- tk_choose.dir(caption = "Select A Folder Contain Regions Shapefile: ")
Shapefile.Name <- "Limit"
Mashhad <- rgdal::readOGR(dsn = Shapefile.Path,
                          layer = Shapefile.Name) %>%
  sp::spTransform(CRSobj = sp::CRS(CRS.Original))

# Extract PET - Shapefile ---------------------------------------------------------------------

# Extract Values From Raster
result <- do.call(what = rbind,
                  args = pbapply::pblapply(X = DATA,
                                           FUN = raster::extract, y = Mashhad, fun = mean, df = TRUE, sp = FALSE))



# WARNING: Change Base on Data
Data_Name <- "VALUE"
colnames(x = result)[2] <- Data_Name

# result Cleansing
result <- result %>%
  dplyr::mutate(date = rownames(.)) %>%
  tidyr::separate(col = date, into = c("VARIABLE", "YEAR", "MONTH.IDs"), sep = "\\_") %>%
  tidyr::separate(col = MONTH.IDs, into = c("MONTH", "IDs"), sep = "\\.") %>%
  dplyr::select(-IDs) %>%
  dplyr::select(ID, VARIABLE, YEAR, MONTH, VALUE) %>% 
  mutate(Region = case_when(
    ID == 1  ~ "Region 01",
    ID == 2  ~ "Region 02",
    ID == 3  ~ "Region 03",
    ID == 4  ~ "Region 03",
    ID == 5  ~ "Region 04",
    ID == 6  ~ "Region 05",
    ID == 7  ~ "Region 06",
    ID == 8  ~ "Region 06",
    ID == 9  ~ "Region 07",
    ID == 10 ~ "Region 08",
    ID == 11 ~ "Region 09",
    ID == 12 ~ "Region 10",
    ID == 13 ~ "Region 11",
    ID == 14 ~ "Region 12",
    ID == 15 ~ "Region 12",
    ID == 16 ~ "Region 13",
  )) %>% 
  group_by(Region, YEAR, MONTH) %>% 
  summarise(PET = mean(x = VALUE, na.rm = TRUE)) %>% 
  mutate(MONTH = match(x = MONTH, table = month.abb)) %>% 
  left_join(y = error_data, by = "MONTH") %>% 
  mutate(PET_adj = PET + ETo.Error) %>% 
  arrange(Region, YEAR, MONTH)

output.name <- paste0(Folder.Output, "/", "Extract_Raster_PET_adj_Limit_400m.csv")
write.csv(x = result, file = output.name, row.names = FALSE)
