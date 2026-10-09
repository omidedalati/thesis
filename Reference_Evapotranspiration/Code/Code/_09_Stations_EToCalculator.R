

############### Initial Setting ###############

# Remove Objects from a Specified Environment
rm(list = ls())

# Load My Functions
source("Code/func.R")

# Load Required Packages
library(package = "dplyr")
library(package = "lubridate")
library(package = "SPEI")
library(package = "stringr")
library(package = "ggplot2")
library(package = "tcltk")
library(package = "tidyr")

if (exists('utils::choose.dir')) {
  Folder.Output <-
    choose.dir(caption = "Select A Folder To Save Output Files: ")
} else {
  Folder.Output <-
    tcltk::tk_choose.dir(caption = "Select A Folder To Save Output Files: ")
}

############### Data Preparation ###############

data <-
  full_join(
    x = readRDS(file = "Dataset/RBSN_Monthly_Khorasan_Shamsi.rds"),
    y = readRDS(file = "Dataset/GSOD_Monthly_Khorasan_Shamsi.rds"),
    by = c("STNID", "YEAR", "MONTH")
  )

write.csv(
  x = as.data.frame(x = data) %>%
    dplyr::select(
      STNID,
      LON.RBSN,
      LAT.RBSN,
      ELEV.RBSN
    ) %>%
    dplyr::distinct(STNID, .keep_all = TRUE) %>%
    tidyr::drop_na(LON.RBSN),
  file = paste0(Folder.Output, "/", "Stations_Info.csv")
)

############### ETo Calculat ###############

# Calculate Reference Evapotranspiration (ETo) for Each Stations

StationPET <- list()

for (ST in unique(x = data$STNID)) {

  print(ST)

  df <- as.data.frame(x = data %>% filter(STNID == ST))

  lat <- unique(x = df$LAT.RBSN)
  print(lat)

  z <- unique(x = df$ELEV.RBSN)
  print(z)


  if (is.na(x = lat)) {

    next()

  }

  StationPET[[ST]] <- df %>%
    mutate(
      ETo_HS_ST = ETo.Hargreaves.Monthly(
        Tmin = .$TMIN.RBSN,
        Tmax = .$TMAX.RBSN,
        Ra = NA,
        lat = lat[1],
        Pre = .$PRCP.RBSN,
        na.rm = TRUE
      ) %>%
        matrix(ncol = 1, byrow = TRUE) %>%
        round(digits = 1),

      ETo_PM_ST = ETo.PenmanMonteith.Monthly(
        Tmin = .$TMIN.RBSN,
        Tmax = .$TMAX.RBSN,
        U2 = .$WDSP.RBSN,
        Ra = NA,
        lat = lat[1],
        Rs = NA,
        tsun = .$SSHN.RBSN,
        CC = NA,
        ed = NA,
        Tdew = .$DEWP.RBSN,
        RH = .$RH.RBSN,
        P = .$STP.RBSN / 10,
        P0 = .$SLP.RBSN / 10,
        z = z[1],
        crop = "short",
        na.rm = TRUE
      ) %>%
        matrix(ncol = 1, byrow = TRUE) %>%
        round(digits = 1),

      ETo_TW_ST = ETo.Thornthwaite.Monthly(
        Tave = .$TEMP.RBSN,
        lat = lat[1],
        na.rm = TRUE
      )
    )

}

StationPET <- do.call(what = rbind,
                      args = StationPET)

rownames(StationPET) <- NULL


write.csv(
  x = StationPET,
  file = paste0(Folder.Output, "/", "Stations_Final_Variables_Monthly_Shamsi.csv")
)
