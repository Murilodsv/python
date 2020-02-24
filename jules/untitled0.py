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

out_header = ['# created by point_run_plots.py on 17/11/2015 (MV)',
              '# first data line is '+datetime_ini,
              '# timestep in seconds = '+dt,
              '# sw_down lw_down precip  t wind  pstar q diff_rad',
              '# Wm-2 Wm-2 kgm-2s-1 K ms-1 Pa kgkg-1 Wm-2']

#--- Open csv as DataFrame
driv_df = pd.DataFrame(pd.read_csv(driv_nm))

#--- format numbers
for i in range(len(l_driv_data)):
    driv_df[l_driv_data[i]] = driv_df[l_driv_data[i]].map(l_driv_fmt[i].format)

#--- write as string
out = driv_df[l_driv_data].to_string(justify='left', index=False, header=False)

#--- write on file
with open('test_just.dat', 'w') as f:
        for item in out:
            f.write("%s" % item)    


# created by point_run_plots.py on 17/11/2015 (KW)
# first data line is 2003-01-01 06:00:00 GMT (2003-01-01 00:00:00 local time), second data line is 2003-01-01 07:00:00 GMT (2003-01-01 01:00:00 local time), last data line is 2004-01-01 05:00:00 GMT (2003-12-31 23:00:00 local time)
# precip may include irrigated water
# negative values of sw_down have been set to zero.
# diff_rad is calculated from sw_down*PARdif/(PARdif + PARdir)
# sw_down lw_down precip  t wind  pstar q diff_rad
# Wm-2 Wm-2 kgm-2s-1 K ms-1 Pa kgkg-1 Wm-2
