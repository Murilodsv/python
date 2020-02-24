#--- Installing missing packages
pkg = c("raster", # For GIS image usage
        "rgdal",  # For GIS image usage
        "ncdf4",  # For NetCDF files manipulations
        "tictoc", # Track runtime 
        "ggplot2",# Plot cool charts 
        "foreach",
        "doParallel",
        "parallel",
        "plyr",
        "grid",
        "hexbin")  
ipkg = pkg %in% rownames(installed.packages())
sapply(pkg[!ipkg],function(x) install.packages(x))

#--- Load installed packages
library(raster)
library(rgdal)
library(ncdf4)
library(tictoc)
library(ggplot2)
library(foreach)
library(plyr)
library(grid)
library(hexbin)
#--- Installing missing packages
pkg = c("raster", # For GIS image usage
        "rgdal",  # For GIS image usage
        "ncdf4",  # For NetCDF files manipulations
        "tictoc", # Track runtime 
        "ggplot2",# Plot cool charts 
        "foreach",
        "doParallel",
        "parallel",
        "rgeos",
        "plyr",
        "grid",
        "hexbin")  
ipkg = pkg %in% rownames(installed.packages())
sapply(pkg[!ipkg],function(x) install.packages(x))

#--- Load installed packages
library(raster)
library(rgdal)
library(ncdf4)
library(tictoc)
library(ggplot2)
library(foreach)
library(doParallel)
library(parallel)
library(rgeos)
library(plyr)
library(grid)
library(hexbin)

