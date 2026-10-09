
# Import Required Module
import wget
import os
import tkinter
import tkinter.filedialog


# Select Location for Store Data
root = tkinter.Tk()
root.withdraw()
Downloads_Folder = tkinter.filedialog.askdirectory()

# Define Server URL
SERVER = r"https://climate.northwestknowledge.net/TERRACLIMATE-DATA/"

# Select Years: from 1958 to 2019
Year = range(1958, 1989)

# Select Variables:
# DONE: pet (Potential evapotranspiration)
# DONE: aet (Actual Evapotranspiration)
# DONE: ppt (Precipitation)
# DONE: srad (Downward surface shortwave radiation)
# DONE: tmax (Max Temperature)
# DONE: tmin (Min Temperature)
# DONE: ws (Wind speed)
# TODO: pdsi (Palmer Drought Severity Index)
# TODO: swe (Snow water equivalent - at end of month)
# TODO: q (Runoff)
# TODO: soil (Soil Moisture)
# TODO: vap (Vapor pressure)
# TODO: def   (Climate Water Deficit)
# TODO: vpd (Vapor Pressure Deficit)

Variables = ["pdsi", "swe", "q", "soil", "vap", "def", "vpd"]

print("Try Connect To Server ...")
print("*********** Start Download ************")

for v in Variables:

    for y in Year:

        ListFileDir = os.listdir(Downloads_Folder)

        fileName = "TerraClimate" + "_" + v + "_" + str(y) + ".nc"

        if fileName in ListFileDir:
            continue

        print(f"Downloading 'TerraClimate_{v}_{str(y)}.nc'")

        url = SERVER + "TerraClimate" + "_" + v + "_" + str(y) + ".nc"
        out = Downloads_Folder + "/" + fileName
        wget.download(url=url, out=out)

        print("Saved", "'TerraClimate" + "_" + v + "_" +
              str(y) + ".nc'", "to", Downloads_Folder)
        print("\n")

print("********** Download Finished! **********")
