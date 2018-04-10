#--- Basic files to get intuition about python sytaxis

#--- Set lists
l = range(10);
l = [1,50]

#--- In order to print or "end" your loop you need to press "enter" when in console mode 
l = []
for i in range(9):
    l.append(i)
    print(l)
    
#--- Alternatively you can run it within exec()
l = []
exec("for i in range(9): l.append(i)")
print(l)


#Load listdir to create directory lists
from os import listdir                  #directory list
from os.path import isfile, join    # only files list
import glob
import os

mypath = "C:\Users\PC-600\Dropbox (Farmers Edge)\MuriloVianna\DB\SoilDB\ISRIC\ISRIC_250m\costumers\centroids"
fls = [f for f in listdir(mypath) if isfile(join(mypath, f))]

thisdir = os.getcwd()

pt = mypath + "\*.shp"

ls = os.listdir(mypath)

mypath = "C:\Users\PC-600\Dropbox (Farmers Edge)\MuriloVianna\DB\SoilDB\ISRIC\ISRIC_250m"

for r, d, f in os.walk(mypath):
    for file in f:
        if ".shp" in file:
            print(os.path.join(r, file))


print(glob.glob("*.shp"))

filenames = next(os.walk(mypath))[2]
