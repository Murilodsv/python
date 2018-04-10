#--------------------------------------#
#--- QGIS Scripting Application ---#
#-------------------------------------#

#--- Read a project


#--- Add Layers to (addVectorLayer)
#--- Path and name
loc = "C:\Users\PC-600\Dropbox (Farmers Edge)\MuriloVianna\DB\SoilDB\ISRIC\ISRIC_250m\costumers\centroids\centroids_v1.shp"
lnm= "centroids_v1"

#layer as an object
layer = iface.addVectorLayer(loc, lnm, "ogr")

#--- Acces Vector without adding
vlayer = QgsVectorLayer(loc, "cen1","ogr")

#--- Access CSV files
uri = "C:\Users\PC-600\Dropbox (Farmers Edge)\MuriloVianna\DB\SoilDB\WISE_SOIL_DSSAT_SOL_FILE.csv?delimiter=%s&xField=%s&yField=%s" % (",","LAT","LON")
csvlayer = QgsVectorLayer(uri, "Wise_csv","delimitedtext")

#--- Access to raster
fileName = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/ISRIC/ISRIC_250m/BLDFIE_M_sl1_250m.tif"
fileInfo = QFileInfo(fileName)
baseName = fileInfo.baseName()
rlayer = QgsRasterLayer(fileName, "BLDFIE_M_sl1_250m")
if not rlayer.isValid():
  print("Layer failed to load!")

#--- Add raster to project
iface.addRasterLayer(fileName, "raster")

#--- Add layer to project
QgsProject.instance().addMapLayer(vlayer)

