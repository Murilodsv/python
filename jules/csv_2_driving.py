# -*- coding: utf-8 -*-
"""
Created on Sun Feb 23 20:31:09 2020

@author: muril
"""

import pandas as pd

datetime_ini = '2000-01-01 00:00:00'
dt           = 10800
driv_nm      = 'WATCH_2004_2018_pira.csv'
l_driv_data  = [  'SWdown' ,  'LWdown' ,  'Rainf'  ,  'Tair'   ,  'Wind'   ,  'PSurf'  ,   'Qair'  , 'Dif_Rad']
l_driv_fmt   = ['{:16.1f}' ,'{:16.1f}' ,'{:16.5E}' ,'{:16.2f}' ,'{:16.2f}' ,'{:16.1f}' ,'{:16.7f}' ,'{:16.1f}' ]
out_fn       = driv_nm.replace('.csv','.dat')

out =        ['# created by csv_2_driving.py (MV)\n',
              '# first data line is '+datetime_ini+'\n',
              '# timestep in seconds = '+str(dt)+'\n',
              '# sw_down lw_down precip  t wind  pstar q diff_rad'+'\n',
              '# Wm-2 Wm-2 kgm-2s-1 K ms-1 Pa kgkg-1 Wm-2'+'\n']

#--- Open csv as DataFrame
driv_df = pd.DataFrame(pd.read_csv(driv_nm))

#--- format numbers
for i in range(len(l_driv_data)):
    driv_df[l_driv_data[i]] = driv_df[l_driv_data[i]].map(l_driv_fmt[i].format)

#--- write as string
out.append(driv_df[l_driv_data].to_string(justify='left', index=False, header=False))

#--- write on file
with open(out_fn, 'w') as f:
        for item in out:
            f.write("%s" % item)    

