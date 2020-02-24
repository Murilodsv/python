# -*- coding: utf-8 -*-
"""
#---> Created on Fri Feb 14 16:13:56 2020

#---> @author: Murilo Vianna
"""

#--- Convert the time units of JULES's outputs to dates objects
#---    time_var = 'seconds since 2003-03-01 06:00:00'
#---    Murilo Vianna (murilodsv@gmail.com)

import numpy  as np
import datetime

def get_jules_time (time_var: str):
    
    if len(time_var.split(" ")) > 1:
        
        #--- Check what strings are date/time
        is_date = []
        for t in time_var.split(" "):
            if ("-" in t) or (":" in t):
                is_date.append(True)
            else:
                is_date.append(False)
        
        #--- Remove non-date strings
        time_var = np.array(time_var.split(" "))[is_date]
        
        #--- Check if a concatenation is needed
        if len(time_var) > 1:
            time_var = " ".join(list(time_var))
    
    #--- Assuming the time format of JULES: yyyy-mm-dd hh:mm:ss
    datetime_object = datetime.strptime(time_var, '%Y-%m-%d %H:%M:%S')
    
    return(datetime_object)
