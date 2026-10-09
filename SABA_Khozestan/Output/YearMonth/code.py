import glob, os

# Define path to directory of your csv files
path_to_csv = "C:/Users/pooya/Dropbox/Work/Hydrotech/SABA_Khozestan/Output/YearMonth/"

# Set current directory to path of csv files
os.chdir(path_to_csv)

# Find each .csv file and load them as vector layers for fname in
for fname in glob.glob("*.csv"):
    uri = "file:///" + path_to_csv + fname + "?type=csv&xField=LONG&yField=LAT&spatialIndex=no&subsetIndex=no&watchFile=no&crs=epsg:4326"
    name = fname.replace('.csv', '')
    lyr = QgsVectorLayer(uri, name, 'delimitedtext')
    QgsProject.instance().addMapLayer(lyr)