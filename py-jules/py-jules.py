# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import os
import pandas as pd
import shutil
from time import gmtime, strftime, time
from subprocess import run

dash_nm = 'dashboard_db.csv'
wd      = os.getcwd()
save_all= False
ext_csv = True
verb    = True
exec_fn = 'jules.exe'

#----------------------#
#--- Load functions ---#
#----------------------#

import gen_nml_defs as gn
import gen_dat_defs as gd
import py_jules_constants as c

#--- Track progress
start_time = time()
    
#--- Logs Warning and Errors
warn_msg   = ['Warning Logs for driving creation at '+strftime("%Y-%m-%d %H:%M:%S", gmtime())+":\n"]
erro_msg   = ['Error Logs for driving creation at '+strftime("%Y-%m-%d %H:%M:%S", gmtime())+":\n"]

#----------------------#
#--- Read dashboard ---#
#----------------------#

#--- Open csv as DataFrame
try:
    dash     = pd.DataFrame(pd.read_csv(wd+'/'+dash_nm))
except UnicodeDecodeError:
    dash     = pd.DataFrame(pd.read_csv(wd+'/'+dash_nm, encoding='latin-1')) # Try latin decode (the most likely codec from Brazilian team)

#--- Filter sites flagged to run
dash_run = dash[:][dash['run_jules']]

for run_id in dash_run['run_id']:
    
    base_nml_fn     = dash_run['sim_base'][dash_run['run_id'] == run_id].values[0]
    driv_id         = dash_run['driv_id'][dash_run['run_id']  == run_id].values[0]
    soil_id         = dash_run['soil_id'][dash_run['run_id']  == run_id].values[0]
    crop_id         = dash_run['crop_id'][dash_run['run_id']  == run_id].values[0]
    crop_nm         = dash_run['crop_nm'][dash_run['run_id']  == run_id].values[0]
        
    #--- Create/Update "jules_run" folder into the provided working directory (wd)
    if verb: print('Updating Running Directory for ID: '+run_id)
    gn.mk_jules_run(wd)
    
    #--------------------------#
    #--- Generate namelists ---#
    #--------------------------#
    
    #--- Update base template run
    if verb: print('Updating Base Template for ID: '+run_id)
    gn.update_nml_setup(wd+'/sim_db/'+base_nml_fn,
                        wd+'/sim_db/driving/meta_'+driv_id+'.csv',
                        wd+'/sim_db/soil/meta_'+soil_id+'.csv',
                        wd+'/sim_db/crop/meta_'+crop_id+'.csv',
                        wd+'/sim_db/meta_'+run_id+'.csv',
                        wd+'/jules_run/nml_setup_'+run_id+'.csv')
    
    #--- Read base nml
    base_nml = pd.DataFrame(pd.read_csv(wd+'/jules_run/nml_setup_'+run_id+'.csv'))
    
    #--- Create Namelists based on updated nml_setup
    if verb: print('Creating Namelists files for ID: '+run_id)
    gn.gen_nml(wd+'/jules_run/nml_setup_'+run_id+'.csv',   # Input namelists setup
               wd+'/templates',                            # Folder where templates are
               wd+'/jules_run/namelists')                  # Output folder for generated namelists files
    
    #---------------------#
    #--- Generate data ---#
    #---------------------#
    
    #--- Get driving meta data from the updated namelists
    datetime_ini = base_nml['val'][(base_nml['namelist'] == 'jules_drive') & (base_nml['variable'] == 'data_start')].values[0]
    dt           = base_nml['val'][(base_nml['namelist'] == 'jules_drive') & (base_nml['variable'] == 'data_period')].values[0]
    
    #--- list of driving variables and formats
    l_driv_data        = base_nml[['array_id','val']][(base_nml['namelist'] == 'jules_drive') & (base_nml['variable'] == 'var')].sort_values(by=['array_id'])    
    l_driv_data['val'] = l_driv_data['val'].str.replace("'","") # Remove "'" used for Fortran namelists
    
    #--- join formats
    l_driv_data = pd.merge(l_driv_data, c.fmt_driv_jules, on='val')
    
    #--- get driving filename
    driv_out_fn = base_nml['val'][(base_nml['namelist'] == 'jules_drive') & (base_nml['variable'] == 'file')].values[0].split('/')[-1]
    driv_out_fn = driv_out_fn.replace("'","")
    
    #--- Check DB consistency
    if driv_out_fn.split('.')[0] != driv_id:
        msg = 'Warning: Driving ID differs between '+dash_nm+' and meta_'+driv_id+'.csv'
        print(msg)
        warn_msg.append(msg)
    
    #--- Generate driving data
    if verb: print('Generating Driving Data for ID: '+run_id)
    gd.gen_driving(wd+'/sim_db/driving/data_'+driv_id+'.csv',
                   datetime_ini,
                   dt,
                   l_driv_data['val'].values.tolist(),
                   l_driv_data['fmt'].values.tolist(),
                   wd+'/jules_run/namelists/data/'+driv_out_fn)
    
    #--- Get soil data
    soil_out_fn = base_nml['val'][(base_nml['namelist'] == 'jules_soil_props') & (base_nml['variable'] == 'file')].values[0].split('/')[-1]
    soil_out_fn = soil_out_fn.replace("'","")
    
    #--- Check DB consistency
    if soil_out_fn.split('.')[0] != soil_id:
        msg = 'Warning: Soil ID differs between '+dash_nm+' and meta_'+soil_id+'.csv'
        print(msg)
        warn_msg.append(msg)
    
    #--- Generate soil data
    if verb: print('Generating Soil Data for ID: '+run_id)
    gd.gen_soil_props(wd+'/sim_db/soil/data_'+soil_id+'.csv',
                      wd+'/jules_run/namelists/data/'+soil_out_fn)

    #--- Read initial soil moisture
    init_shuf    = pd.DataFrame(pd.read_csv(wd+'/sim_db/ancillary/initial_sthuf_'+run_id+'.csv'))
    init_shuf_fn = base_nml['val'][(base_nml['namelist'] == 'jules_initial') & (base_nml['variable'] == 'file')].values[0].split('/')[-1]
    init_shuf_fn = init_shuf_fn.replace("'","")
    
    #--- Create initial soil moisture
    if verb: print('Generating Initial Soil Moisture Data for ID: '+run_id)
    gd.gen_init_wat(init_shuf['init_sthuf'].values,
                    wd+'/jules_run/namelists/data/'+init_shuf_fn)
    
    #--- Get prescribed CO2 data
    co2_data_fn  = base_nml['val'][(base_nml['namelist'] == 'jules_prescribed_dataset') & (base_nml['variable'] == 'file')].values[0].split('/')[-1]        
    co2_data_fn  = co2_data_fn.replace("'","")
    
    #--- Generate CO2 data
    if verb: print('Generating CO2 Data for ID: '+run_id)
    gd.gen_co2_dat(wd+'/sim_db/ancillary/'+co2_data_fn.replace('.dat','.csv'),
                   wd+'/jules_run/namelists/data/'+co2_data_fn,
                   var    = 'co2')
    
    #--- Create tile fraction array
    #--- Assuming only one crop per simulation
    n_tf = len(c.n_tile_frac['crop'])
    tf   = [0] * n_tf   
    tf[c.n_tile_frac['n_id'][c.n_tile_frac['crop'] == crop_nm].values[0]-1] = 1.
    
    #--- tile fraction filename
    tf_nm = base_nml['val'][(base_nml['namelist'] == 'jules_frac') & (base_nml['variable'] == 'file')].values[0].split('/')[-1]
    tf_nm = tf_nm.replace("'","")
    
    #--- Generate tile fraction file
    if verb: print('Generating Tile Fraction Data for ID: '+run_id)
    gd.gen_tile_frac(tf,
                     wd+'/jules_run/namelists/data/'+tf_nm)

    #-----------------#
    #--- Run JULES ---#
    #-----------------#
    
    #--- Copy the executable to running folder
    shutil.copy2(wd+'/sim_db/'+exec_fn, wd+'/jules_run/')
    
    #--- Prepare running args    
    run_args = ['cd '+wd+'/jules_run/namelists/',               # get into run folder
                'chmod +x ../'+exec_fn,                         # allow executable permission
                'module swap PrgEnv-cray PrgEnv-cray/5.2.40',   # swap module for jules
                'module load cray-netcdf-hdf5parallel/4.3.2',   # load module for jules
                'module load cray-snplauncher/7.0.4',           # load module for jules 
                'mpiexec ../'+exec_fn]                          # run jules with mpi
    
    #--- Run JULES
    if verb: print('Running JULES for ID: '+run_id)
    run("; ".join(run_args), shell = True)
   
    #--- Read CSV?
    if ext_csv:
        
