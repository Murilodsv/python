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

#--- Add layer to project
QgsMapLayerRegistry.instance().addMapLayer(vlayer)


#------------------------------------------------------------------------------------------------
#----------------------------------------#
#--- Load processing tools to console ---#
#----------------------------------------#

#Load processing algorithms
import processing

#List of algorithm within processing
processing. alglist()

#Points related algorithms
processing.alglist("point")

#Processing tool example
processing.alghelp("saga:slopeaspectcurvature")

#Processing especification from polygon centroids
processing.alghelp("saga:polygoncentroids")

#ALGORITHM: Polygon centroids
#   POLYGONS <ParameterVector>
#   METHOD          <ParameterBoolean>
#   CENTROIDS <OutputVector>

processing.algoptions("saga:polygoncentroids")

#--- Load a shapefile
wd = "C:/Users/PC-600/Dropbox (Farmers Edge)/MuriloVianna/DB/SoilDB/ISRIC/ISRIC_250m/costumers"
fn = "BLDFIE_M_sl1_250m_NA_ChapadaodaAtlantica_Subfield1972585_Zoning87346_NIR_NB_20150806.shp"

layer = QgsVectorLayer(wd + "/" + fn, "shp1","ogr")

#--- run centroids
alg = "saga:polygoncentroids"   #algorithms name
out = "c:/Murilo/cen"                #output
cen = processing.runalg("saga:polygoncentroids", layer, 1, out)

#Add to project
cen1 = iface.addVectorLayer(cen['CENTROIDS'], "SHPCEN1", "ogr")
