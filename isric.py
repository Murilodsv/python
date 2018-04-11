#--------------------------------#
#--- ISRIC SOIL DB PROCESSING ---#
#--------------------------------#

#--- Created to extract values from ISRIC 250m soil dabase (.tif) to polygon features and points
#--- Murilo S. Vianna
#--- murilodsv@gmail.com

#--- Load os and processing
import os
import processing
import PyQt4

#--- Working Directories
wd_z = "C:/Murilo/GIS/BRA_Costumers/zoning"			#All zoning shapefiles dir
wd_c = "C:/Murilo/GIS/BRA_Costumers/centroids" 		#Centroids output dir
wd_t = "C:/Murilo/GIS/ISRIC250m"					#Tif images dir
wd_i = "C:/Murilo/GIS/BRA_Costumers/isric_cen"		#ISRIC extracted points dir
wd_izs = "C:/Murilo/GIS/BRA_Costumers/isric_zs"		#ISRIC zonal statistics dir

#--- list of all zoning shapefiles within (wd_z)
input_lys = []
filename = []
for  r, d, f in os.walk(wd_z):
    for file in f:
        if file[len(file)-4:len(file)] == ".shp":
            input_lys.append(os.path.join(r,file))
            filename.append(os.path.join(file))

#--- Total number of shapefiles
print("Total Number of shapefiles: " + str(len(input_lys)))

#--- Run centroids from zoning shapefiles
i = 0
for ly in input_lys:
    processing.runalg("saga:polygoncentroids",      #Algorithm
    ly,                                             #SHAPE
    1,											    #METHOD
    wd_c+"/"+filename[i])                           #OUTPUT
    i = i + 1

#--- list of all centroids shapefiles within (wd_c)
input_lys_c = []
filename_c = []
for  r, d, f in os.walk(wd_c):
    for file in f:
        if file[len(file)-4:len(file)] == ".shp":
            input_lys_c.append(os.path.join(r,file))
            filename_c.append(os.path.join(file))

#--- Total number of centroids shapefiles
print("Total Number of centroid shapefiles: " + str(len(input_lys_c)))

#--- list of all ISRIC 250m raster files (.tif)
input_tif = []
tifname = []
for  r, d, f in os.walk(wd_t):
    for file in f:
        if file[len(file)-4:len(file)] == ".tif":
            input_tif.append(os.path.join(r,file))
            tifname.append(os.path.join(file))

#--- Extract raster values to points
output_isric = []
i = 0
for ly_c in input_lys_c:
    processing.runalg("saga:addgridvaluestopoints",     #Algorithm
    ly_c,                                               #SHAPES
    input_tif,                                          #GRIDS
    0,                                                  #RESAMPLING([0] Nearest Neighbor, [1] Bilinear Interpolation,[2] Inverse Distance Interpolation,[3] Bicubic Spline Interpolation,[4] B-Spline Interpolation)
    3,                                                  #_RESAMPLING([0] Nearest Neighbor, [1] Bilinear Interpolation,[2] Bicubic Spline Interpolation,[3] B-Spline Interpolation)
    wd_i+"/ISRIC250m_"+filename_c[i])                   #RESULT
    i = i + 1

#--- Run isric zonal statistics within each zone
#--- Note: polygons smaller than pixel resolution will be skipped (recommend to use centroids sampling)

i = 0
for ly_zs in input_lys:
    n = 0
    for tif in input_tif:
        processing.runalg("qgis:zonalstatistics",       #Algorithm
        tif,                                            #GRIDS
        1,                                              #METHOD
        ly_zs,                                          #SHAPES
        tifname[n][0:2]+"_"+tifname[n][9:12],
        1,
        wd_izs+"/"+tifname[n][0:12]+"_"+filename[i])
        n = n + 1
    i = i + 1
