#----------------------------------------#
#--- ISRIC SOIL DB PROCESSING ---#
#----------------------------------------#

#--- Load os and processing
import os
import processing

#--- All zoning shapefiles directory
wd_z = "C:/Murilo/GIS/BRA_Costumers/zoning"

#--- list of all shapefiles within (wd_z)
input_lys = []
for  r, d, f in os.walk(wd_z):
    for file in f:
        if ".shp" in file:
            input_lys.append(os.path.join(file))
#            print(os.path.join(z,file))

#--- Total number of shapefiles
print(len(input_lys))
