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

def read_ncdf(nc_nm:str,
              out_fn = None):

    #---------------------------------------------------------------#
    #------------------------ read_ncdf ----------------------------# 
    #---------------------------------------------------------------#
    #--- Goal: 
    #---    Extract data from the output netCDF files of JULES model into CSV files
    #--- Parameters: 
    #---    nc_nm      : Name of input netCDF file
    #---    out_fn     : Output filename                       [optional]
    #--- Author:
    #---    Murilo Vianna (murilodsv@gmail.com)
    #----------------------------------------------------------------#
    
    from netCDF4 import Dataset
    import pandas as pd
    import numpy  as np
    from time import time
    
    #--- Track progression
    start_time = time()
    
    if out_fn == None: 
        gen_out = True
    else:
        gen_out = False
        
    #--- open ncdf
    jules_nc = Dataset(nc_nm, "r", format="NETCDF4")
    
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
                import sys
                msg = "Error: Variable '"+v+"' not found on netCDF file '"+nc_nm+"'\nThe variables of this file are:"+str(var_l_nc)
                print(msg+'\nFile not Read.')
                sys.exit(1)
            
            #--- Get variable infos
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
            
        #--- Export to CSV
        if gen_out:
            out_fn  = nc_nm.replace('.nc','')
            
        out_fn  = out_fn+"_"+"_".join(list(v_nmdim))+'.csv'
        df_v.to_csv(out_fn, index = None, header=True) # Don't forget to add '.csv' at the end of the path
    
    #--- Export dimensions file
    if gen_out:
        out_fn  = nc_nm.replace('.nc','')
            
    df_dims.to_csv(out_fn+'_info.csv', index = None, header=True)
    
    print("Elapsed time: --- %.3f seconds ---" % (time() - start_time))

