# Dataset Link:
URL: https://developers.google.com/earth-engine/datasets/catalog/ECMWF_ERA5_DAILY#bands

# How to Download Datasets?
Run **Datasets_Download.ipynb** from https://colab.research.google.com/ .

# Name: `ERA5 Daily Aggregates`
## Code
`ECMWF/ERA5/DAILY`

## Description
`ERA5 is the fifth generation ECMWF atmospheric reanalysis of the global climate. Reanalysis combines model data with observations from across the world into a globally complete and consistent dataset. ERA5 replaces its predecessor, the ERA-Interim reanalysis.

ERA5 DAILY provides aggregated values for each day for seven ERA5 climate reanalysis parameters: 2m air temperature, 2m dewpoint temperature, total precipitation, mean sea level pressure, surface pressure, 10m u-component of wind and 10m v-component of wind. Additionally, daily minimum and maximum air temperature at 2m has been calculated based on the hourly 2m air temperature data. Daily total precipitation values are given as daily sums. All other parameters are provided as daily averages.

ERA5 data is available from 1979 to three months from real-time. More information and more ERA5 atmospheric parameters can be found at the Copernicus Climate Data Store.`

## Additional Information
Resolution: `0.25 arc degrees`  
Bands:   
Name	Units	Min	Max	Description
mean_2m_air_temperature	K	223.6*	304*	
Average air temperature at 2m height (daily average)

minimum_2m_air_temperature	K	220.7*	300.8*	
Minimum air temperature at 2m height (daily minimum)

maximum_2m_air_temperature	K	225.8*	310.2*	
Maximum air temperature at 2m height (daily maximum)

dewpoint_2m_temperature	K	219.3*	297.8*	
Dewpoint temperature at 2m height (daily average)

total_precipitation	m	0*	0.02*	
Total precipitation (daily sums)

surface_pressure	Pa	65639*	102595*	
Surface pressure (daily average)

mean_sea_level_pressure	Pa	97657.4*	103861*	
Mean sea level pressure (daily average)

u_component_of_wind_10m	m s-1	-11.4*	11.4*	
10m u-component of wind (daily average)

v_component_of_wind_10m	m s-1	-10.1*	10.1*	
10m v-component of wind (daily average)