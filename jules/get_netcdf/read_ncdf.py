# -*- coding: utf-8 -*-
"""
#--------------------------------------#
#--- Read netCDF outputs from JULES ---#
#--------------------------------------#

#---> Created on Feb-2020

#---> Required Packages:
#---> netCDF4 (pip install netCDF4)
#---> Source: https://unidata.github.io/netcdf4-python/netCDF4/index.html

#---> Plots: https://python-graph-gallery.com/

#---> @author: Murilo Vianna
"""

from netCDF4 import Dataset
import pandas as pd
import numpy  as np
import os
import argparse

#--- Get scripts arguments
if __name__ == "__main__":
    import sys    
    
    if len(sys.argv) == 1:        
        
        #-----------------------------#
        #--- Use Default Arguments ---#
        #-----------------------------#
        
        #--- First netcdf in current folder
        nc_nm = ''
        for i in range(len(os.listdir())):            
            if '.nc' in os.listdir()[i]:
                nc_nm = os.listdir()[i]
                break
        
        if nc_nm == '':
            #--- No netCDF file found in this directory
            msg = "Error: No netCDF file (.nc) found in this directory.\n Please change directory or specify the file name."
            print(msg+'\nFile not Read.')
            sys.exit(1)
            
        #--- Defaults
        dim_nm  = 'time'            # assuming we are running a single point namelist
        npft    = 3                 # 3 for Maize (doi:10.5194/gmd-8-1139-2015)
        var_l   = ['croplai',       # LAI
                   'cropcanht',     # Canopy Height
                   'cropharvc']     # Harvestable parts dry weight
        fl_melt = False             # Melt data
        out_fn  = nc_nm.replace('.nc','')
        
    elif len(sys.argv) == 6:
        
        #----------------------#
        #--- Read Arguments ---#
        #----------------------#
        
        #--- get boolean argument
        def str2bool(v):
            if isinstance(v, bool):
                return v
            if v.lower() in ('yes', 'true', 't', 'y', '1'):
                return True
            elif v.lower() in ('no', 'false', 'f', 'n', '0'):
                return False
            else:
                raise argparse.ArgumentTypeError('Boolean value expected.')
        
        nc_nm   = str(sys.argv[1])              # netCDF filename
        dim_nm  = str(sys.argv[2])              # dimension name
        npft    = int(sys.argv[3])              # npft number
        var_l   = str(sys.argv[4]).split(',')   # list of variables to be extracted
        fl_melt = str2bool(sys.argv[5])         # flag to melt results
        out_fn  = str(sys.argv[6])              # output csv file
        
    else:
        #--------------------------------#
        #--- Wrong number of argument ---#
        #--------------------------------#
        msg = 'Error: Wrong Number of Arguments Provided. Please Provide:\n 1) netCDF filename (.nc)\n 2) Dimension name\n'+' 3) The npft number\n 4) Variable list\n 5) Flag to melt\n 6) Output CSV filename'
        print(msg)   
        
#--- open ncdf
jules_nc = Dataset(nc_nm, "r", format="NETCDF4")

#--- Check whether dimensions and variables exists within this netCDF
nc_size = []
nc_dim  = []
for k in list(jules_nc.dimensions.keys()):
    nc_dim.append(k)
    nc_size.append(jules_nc.dimensions[k].size)

if not dim_nm in nc_dim:
    msg = "Error: Dimension '"+dim_nm+"' not found on netCDF file '"+nc_nm+"'\nThe dimensions of this file are:"+str(nc_dim)
    print(msg+'\nFile not Read.')
    sys.exit(1)    

#--- Get dimensions attributes
dim_att = jules_nc.variables[dim_nm][:].ravel().data

#--- Initialize df
df = pd.DataFrame({dim_nm: dim_att})

#--- List of variables
var_l_nc = list(jules_nc.variables.keys())

#--- Variables with same dimension
l_v_dim = []
l_v_uni = []
for v in var_l_nc:
    l_v_dim.append(jules_nc[v].dimensions)
    l_v_uni.append(jules_nc[v].units)
    
df_dims = pd.DataFrame({'variable'   : var_l_nc,
                        'dimensions' : l_v_dim ,
                        'units'      : l_v_uni})

#--- Sort by dimensions
df_dims = df_dims.sort_values(by=['dimensions'])

#--- Unique dimensions
uni_dims = df_dims['dimensions'].drop_duplicates().reset_index(drop=True)

for uni_d in uni_dims:
    
    var_l = df_dims['variable'][df_dims['dimensions'] == uni_d]

    #--- Get variables list into this dimension
    init_df = True
    for v in var_l:
    
        if not v in var_l_nc:
            msg = "Error: Variable '"+v+"' not found on netCDF file '"+nc_nm+"'\nThe variables of this file are:"+str(var_l_nc)
            print(msg+'\nFile not Read.')
            sys.exit(1)
        
        #--- Get variable infos
        v_ndims = jules_nc[v][:].ndim
        v_nmdim = jules_nc[v].dimensions
        v_shape = jules_nc[v][:].shape
       
        #--- Index netcdf variables by dimension size
        if init_df:
            
            init_df = False
            init_d  = True
            
            for d in v_nmdim:
                                
                d_rep = np.arange(1,np.array(v_shape)[np.array(v_nmdim) == d][0]+1)                
                
                if init_d:
                    d_vec = np.repeat(d_rep, repeats = np.prod(np.array(v_shape)[np.array(v_nmdim) != d]))
                    df_v  = pd.DataFrame({d : d_vec})
                    init_d = False
                    
                else:                    
                    d_vec = list(d_rep) * np.prod(np.array(v_shape)[np.array(v_nmdim) != d])
                    df_v   = df_v.join(pd.DataFrame({d : d_vec}))
                    
        
        #--- Bind with variable data
        if v in df_v.columns:
            df_v = df_v.join(pd.DataFrame({(v+'_value') : jules_nc[v][:].ravel().data}))
        else:
            df_v = df_v.join(pd.DataFrame({v : jules_nc[v][:].ravel().data}))
            
    #--- Melt it?
    if fl_melt:
        df_v = pd.melt(df_v, id_vars=np.array(v_nmdim))        
        
    #--- Export to CSV
    out_fn  = nc_nm.replace('.nc','')
    out_fn  = out_fn+"_"+"_".join(list(v_nmdim))+'.csv'
    df_v.to_csv(out_fn, index = None, header=True) #Don't forget to add '.csv' at the end of the path

#--- Export dimensions file
out_fn  = nc_nm.replace('.nc','')
df_dims.to_csv(out_fn+'_info.csv', index = None, header=True)

