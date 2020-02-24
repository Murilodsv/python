# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

from netCDF4 import Dataset
import pandas as pd
import numpy  as np
import os
import ftplib
import shutil
import gzip

#--- In a preliminar exploratory analysis we found that the dimensions of for this dataset is: ('tstep', 'lat', 'lon')
#--- Therefore, the slicing for all time series and a given coordinate is nc[var_name][:,lat,lon]

save_all = False
out_fn   = 'WATCH_2004_2018_pira'

lat = -22.698734
lon = -47.642707

yr_ini = 2004
yr_end = 2018

mo_ini = 1
mo_end = 12

wd_out = 'C:/Users/earmdo/OneDrive - University of Leeds/CSSP_modeling'
os.chdir(wd_out)

#--- URL to FTP stored WFDEI data
#--- WFDEI is suggested at JULES's Website (https://jules.jchmr.org/content/getting-started#driving-data)
#--- Following ReadMe from ftp://rfdata:forceDATA@ftp.iiasa.ac.at/README-WFDEI.pdf (look into this pdf to find an url for browser)
ftp_nm  = 'ftp.iiasa.ac.at'
un      = 'rfdata'
pw      = 'forceDATA'

#--- login into ftp:
ftp = ftplib.FTP(ftp_nm)
ftp.login(un,pw)

ftp.dir()
ftp.cwd('~/WFDEI/')

#--- FTP's Subfolders
list_subf = ['LWdown_WFDEI',    
             'SWdown_WFDEI',    
             'Tair_WFDEI',      
             'Wind_WFDEI',
             'Qair_WFDEI',
             'PSurf_WFDEI',
             'Rainf_WFDEI_GPCC',
             'Rainf_WFDEI_CRU']

yr  = yr_ini
mo  = mo_ini
frun= True
while frun:
    
    #--- Update time flags
    if yr == yr_ini and mo == mo_ini:
        init_run = True
    elif yr == yr_end and mo == mo_end:
        frun = False
    else:
        init_run = False
            
    for subf in list_subf:
    
        #--- variable name        
        var_nm = subf.replace('_WFDEI','')
        
        print('Extracting '+var_nm+' for '+str(("%02i") % mo)+'/'+str(yr))
        
        #--- get into the corresponding ftp folder
        ftp.cwd('~/WFDEI/'+subf)
        
        #--- Filename to download    
        filename = subf+'_'+str(yr)+str(("%02i") % mo)+'.nc.gz'   
        
        #--- Check if all netcdf needs to be stored
        if save_all:
            nc_nm    = filename.replace('.gz','')
        else:
            nc_nm    = 'temp.nc'
                
        #--- Download the .gz file
        f = open(filename, 'wb')
        ftp.retrbinary('RETR '+filename,f.write)
        f.close()    
        
        #--- unpack
        with gzip.open(filename, 'rb') as f_in:
            with open(nc_nm, 'wb') as f_out:
                shutil.copyfileobj(f_in, f_out)
    
        #--- delete .gz
        os.remove(filename)
        
        #--- Read ncdf
        nc = Dataset(nc_nm, "r", format="NETCDF4")
        
        #--- get netcdf lat/lon arrays
        lat_array = nc.variables['lat'][:]
        lon_array = nc.variables['lon'][:]
        
        #--- find the most proximal coordinate to input lat/lon
        x = np.abs(lon_array - lon).argmin()
        y = np.abs(lat_array - lat).argmin()
    
        #--- extract data from that x/y for all time steps
        var_array = nc[var_nm][:,y,x].ravel().data
        
        if subf == list_subf[0]:            
            #--- init step df
            df_step = pd.DataFrame({'time'  :   nc['time'][:].ravel().data,
                                    'year'  :   [yr]*len(var_array),
                                    'month' :   [mo]*len(var_array),
                                    'lat'   :   [lat]*len(var_array),
                                    'lon'   :   [lon]*len(var_array),
                                    var_nm  :   var_array})
            
        else:            
            #--- bind new column
            df_step = df_step.join(pd.DataFrame({var_nm : var_array}))
    
    #--- append to main df
    if init_run:
        df = df_step
    else:
        df = df.append(df_step)
    
    #--- update time
    if mo == 12:
        
        #--- new year
        mo = 1
        yr+= 1
        
    else:
        
        mo+= 1
    
    #--- update flags 
    if init_run: init_run = False

#--- Save results
if not 'out_fn' in locals():
    out_fn = 'WATCH_'+str(yr_ini)+'_'+str(yr_end)
        
#--- save main df as a csv file
df.to_csv(out_fn+'.csv', index = None, header=True)

