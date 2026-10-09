
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

# Load My Functions
reticulate::use_python(python = "C:/Users/pooya/anaconda3/python.exe")
source(file = "Code/func.R")

# Select Folder To Save Output Files
if (exists('utils::choose.dir')) {
  Folder.Output <- choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  Folder.Output <- tcltk::tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

############### Select Stations ###############

# Load Station Info - WARNING : Maybe Change
ST <- read.csv(file = "Code/Stations_Info.csv",
               colClasses = c("character", "character",
                              "numeric", "numeric", "numeric"))

# WARNING : Maybe Change
myStations <- c("407400-99999",
                "407410-99999",
                "407430-99999",
                "407440-99999",
                "407450-99999",
                "407460-99999",
                "407620-99999",
                "407630-99999",
                "407920-99999",
                "408060-99999")

if (!is.null(x = myStations)) {

  ST <- ST %>% dplyr::filter(STNID %in% myStations)
  ST$ID <- as.character(x = 1:length(myStations))

}

# Load Coordinate Reference System
CRS.Original <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# Create Objects of Class SpatialPointsDataFrame
coords <- ST %>% dplyr::select(LON.RBSN, LAT.RBSN)
df     <- ST %>% dplyr::select(-one_of(c("LON.RBSN", "LAT.RBSN")))
crs    <- sp::CRS(CRS.Original)

st <- sp::SpatialPointsDataFrame(coords = coords,
                                 data = df,
                                 proj4string = crs)

############### Extract Value of Each Stations ###############

result.gregorian <- list()
result.shamsi <- list()

# Load List Files of Raster Data - WARNING : Maybe Change
List.Files.Rasters <- list.files(path = "D:/HydroTech/Project_01/Reference_Evapotranspiration/Calculations/", 
                                 pattern = "4000m.rds",
                                 full.names = TRUE)

for (LFR in List.Files.Rasters[1]) {

  # Load Raster Data
  data <- readRDS(file = LFR)

  # Extract Values From Raster
  extracted.value <- do.call(what = rbind,
                             args = pbapply::pblapply(X = data,
                                                      FUN = raster::extract, y = st, df = TRUE))

  colnames(x = extracted.value) <- c("ID", "VALUE")

  # result Cleansing
  extracted.value <- extracted.value %>%
    dplyr::mutate(date = rownames(.)) %>%
    tidyr::separate(col = date, into = c("VARIABLE", "YEAR", "MONTH.ID"), sep = "\\_") %>%
    tidyr::separate(col = MONTH.ID, into = c("MONTH", "ID"), sep = "\\.") %>%
    dplyr::select(ID, YEAR, MONTH, VARIABLE, VALUE) %>%
    dplyr::left_join(x = ST, y = ., by = "ID") %>%
    dplyr::mutate(MONTH = match(x = .$MONTH, table = month.abb)) %>% 
    dplyr::select(-X)

  names(extracted.value) <- c("STNID", "LON", "LAT", "ELEV", "ID", "YEAR", "MONTH", "VARIABLE", "VALUE")

  extracted.value.gregorian <- extracted.value

  # Convert Month Gregorian to Shamsi
  Shamsi.to.Gregorian = FALSE

  if (Shamsi.to.Gregorian) {

    extracted.value.shamsi <- list()

    for (station in unique(x = extracted.value$STNID)) {

      df <- extracted.value %>%
        filter(STNID == station)

      ID <- unique(x = df$ID)
      STNID <- unique(x = df$STNID)
      LON <- unique(x = df$LON)
      LAT <- unique(x = df$LAT)
      ELEV <- unique(x = df$ELEV)
      VARIABLE <- unique(x = df$VARIABLE)

      extracted.value.shamsi[[station]] <- df %>%
        select(YEAR, MONTH, VALUE) %>%
        Month.Gregorian.to.Shamsi(MONTH.ABB = FALSE) %>%
        group_by(PERSIAN.YEAR, PERSIAN.MONTH) %>%
        summarise_all(sum) %>%
        rename(YEAR = PERSIAN.YEAR, MONTH = PERSIAN.MONTH) %>%
        mutate(ID = ID, STNID = STNID, LON = LON, LAT = LAT, ELEV = ELEV, VARIABLE = VARIABLE) %>%
        dplyr::select(ID, STNID, LON, LAT, ELEV, YEAR, MONTH, VARIABLE, VALUE)
    }

    extracted.value.shamsi <- do.call(what = rbind,
                                      args = extracted.value.shamsi)

  }

  result.gregorian <- extracted.value.gregorian
  result.shamsi <- extracted.value.shamsi

}

# Clean Result Gregorian
result.gregorian <- do.call(what = rbind,
                            args = result.gregorian)

rownames(result.gregorian) <- NULL

result.gregorian <- result.gregorian %>%
  arrange(ID, VARIABLE, YEAR, MONTH)

saveRDS(object = result.gregorian,
        file = paste0(Folder.Output, "/", "ETo_TerraClimate_Dataset_Monthly_1958_2019.rds"))

# Clean Result Shamsi
result.shamsi <- do.call(what = rbind,
                         args = result.shamsi)

rownames(result.shamsi) <- NULL

result.shamsi <- result.shamsi %>%
  arrange(ID, VARIABLE, YEAR, MONTH)

saveRDS(object = result.shamsi,
        file = paste0(Folder.Output, "/", "ETo_TerraClimate_Dataset_Monthly_1337_1398.rds"))

