
############### Information ###############

# Convert *.nc File to Raster (*.rds)

############### Initial Setting ###############

rm(list = ls())

library(package = "raster")
library(package = "sp")
library(package = "rgdal")
library(package = "dplyr")
library(package = "tcltk")
library(package = "stringr")
library(package = "ncdf4")

if (exists('utils::choose.dir')) {
  Folder.Data <- choose.dir(caption = "Select TerraClimate Dataset Folder: ")
  Folder.Output <- choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  Folder.Data <- tk_choose.dir(caption = "Select TerraClimate Dataset Folder: ")
  Folder.Output <- tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

############### List Files in a Directory ###############

List.Files <- list.files(path = Folder.Data,
                         pattern = "*.nc",
                         full.names = FALSE)

############### Load Data ###############

# TODO: Find Better Way!!!

result <- list()
Vector.Year <- c()

for (i in List.Files) {

  # TEST : (i = List.Files[1])

  # WARNING: Edited Base On Your Files (e.g. TerraClimate_pet_1989.nc)
  year <- str_extract(string = i, pattern = "[[:digit:]]+")
  Vector.Year <- c(Vector.Year, year)

  var <- str_extract(string = i, pattern = "_.+?_") %>% str_sub(start = 2, end = -2)

  if (var != "pet") {
    
    next
    
  }

  # WARNING: Edited Base On Your Files (e.g. Each File Contain 12 Month)
  for (m in 1:12) {

    nc.File <- paste0(Folder.Data, "/", i)

    SubList.Name <- paste0(var, "_", year, "_", month.abb[m])

    Read.Raster <- raster(x = nc.File, band = m) %>%
      readAll()

    Iran <- as(raster::extent(x = c(42, 65, 23, 41)), "SpatialPolygons")

    proj4string(Iran) <- crs(Read.Raster)

    Raster.Iran <- raster::crop(x = Read.Raster,
                                y = Iran)

    result[[SubList.Name]] <- Raster.Iran

    print(SubList.Name)

    }

}

saveRDS(object = result,
        file = paste0(Folder.Output, "/", "TerraClimate_PET_Iran_Raster_",
                      min(Vector.Year), "_", max(Vector.Year), "_4000m.rds"))
