#--- Add all BR costumers Shapefile

#--- Load os and processing
import os
import processing
import PyQt4

wd_z = "C:/Users/PC-600/Dropbox (Farmers Edge)/Brazil Project"

#--- list of all shapefiles within (wd_z)
input_shp = []
filename = []
for  r, d, f in os.walk(wd_z):
    for file in f:
        if file[len(file)-4:len(file)] == ".shp":
            input_shp.append(os.path.join(r,file))
            filename.append(os.path.join(file))

i = 1
for f in filename:
    iface.addVectorLayer(input_shp[i], f,"ogr")
    i = i + 1
