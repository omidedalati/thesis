# Dataset Link:
URL: https://wapor.apps.fao.org/home/WAPOR_2/1

# How to Download Datasets?
Run **Datasets_Download.ipynb** from https://colab.research.google.com/ .

# Name: `Precipitation (Dekadal)`
## Code
`L1_PCP_D`

## Description
`Precipitation data is delivered on a daily basis. The source of this dataset is CHIRPS (Climate Hazards Group InfraRed Precipitation with Station) quasi-global rainfall dataset, starting from 1981 up to near present. The value of each pixel represents the average of daily precipitation in the dekad expressed in mm (1mm=1l/m² or 1mm=10m³/ha).`

## Additional Information
Format: `Raster Dataset`  
Unit: `mm (1mm=1l/m² or 1mm=10m³/ha)`  
Data type: `Int16 (16bit Integer)`  
Conversion factor: `the pixel value in the downloaded data must be multiplied by 0.1`  
No data value: `-9999`  
Spatial resolution: `Approximately 5km (0.05 degree)`  
Spatial extent: `Africa and Near East`  
Spatial Reference System (SRS): `EPSG:4326 - WGS84 - Geographic Coordinate System (lat/long)`  
Temporal resolution: `from January 2009 to present`  
Temporal extent: `Dekadal (approximately every 10 days)`  
Near Real-time: `New dekadal data layers are released approximately 5 days after the end of a dekad. A higher quality version of the same data layer is uploaded after 6 dekads have passed. This final version of the dekadal dataset has a higher quality because gap filling and interpolation processes, where needed, have been based on more data observations.`  
Methodology: `See daily precipitation (L1_PCP_E) for further information. The dekadal average is obtained by taking the daily PCP, summing the days of each dekad, and dividing the total by the number of the days in the dekad.`  

# Name: `Reference EvapoTranspiration (Dekadal)`
## Code
`L1_RET_D`

## Description
`Reference EvapoTranspiration (RET) is defined as the evapotranspiration from a hypothetical reference crop and it simulates the behaviour of a well-watered grass surface. The value of each pixel represents the average of the daily reference evapotranspiration for that specific dekad.`

## Additional Information
Format: `Raster Dataset`  
Unit: `mm (1mm=1l/m² or 1mm=10m³/ha)`  
Data type: `Byte (8bit Integer)`  
Conversion factor: `the pixel value in the downloaded data must be multiplied by 0.1`  
No data value: `255`  
Spatial resolution: `Approximately 20km (0.17 degree)`  
Spatial extent: `Africa and Near East`  
Spatial Reference System (SRS): `EPSG:4326 - WGS84 - Geographic Coordinate System (lat/long)`  
Temporal resolution: `from January 2009 to present`  
Temporal extent: `Dekadal (approximately every 10 days)`  
Near Real-time: `New dekadal data layers are released approximately 5 days after the end of a dekad. A higher quality version of the same data layer is uploaded after 6 dekads have passed. This final version of the dekadal dataset has a higher quality because gap filling and interpolation processes, where needed, have been based on more data observations.`  
Methodology: `See daily Reference EvapoTranspiration (L1_RET_E) for further information. The dekadal average is obtained by taking the daily RET, summing the days of each dekad, and dividing the total by the number of the days in the dekad.`  

# Name: `Actual EvapoTranspiration and Interception (Dekadal)`
## Code
`L1_AETI_D`

## Description
`The actual EvapoTranspiration and Interception (ETIa) (dekadal, in mm/day) is the sum of the soil evaporation (E), canopy transpiration (T), and evaporation from rainfall intercepted by leaves (I). The value of each pixel represents the average daily ETIa in a given dekad.`

## Additional Information
Format: `Raster Dataset`  
Unit: `mm (1mm=1l/m² or 1mm=10m³/ha)`  
Data type: `Int16 (16bit Integer)`  
Conversion factor: `the pixel value in the downloaded data must be multiplied by 0.1`  
No data value: `-9999`  
Spatial resolution: `250m (0.00223 degree)`  
Spatial extent: `Africa and Near East`  
Spatial Reference System (SRS): `EPSG:4326 - WGS84 - Geographic Coordinate System (lat/long)`  
Temporal resolution: `from January 2009 to present`  
Temporal extent: `Dekadal (approximately every 10 days)`  
Near Real-time: `New dekadal data layers are released approximately 5 days after the end of a dekad. A higher quality version of the same data layer is uploaded after 6 dekads have passed. This final version of the dekadal dataset has a higher quality because gap filling and interpolation processes, where needed, have been based on more data observations.`  
Methodology: `The calculation of the ETIa is based on the ETLook model described in Bastiaanssen et al. (2012). See the methodology of the three components (E, T and I) for further information.`  