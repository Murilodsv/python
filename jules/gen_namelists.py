# -*- coding: utf-8 -*-
"""
#------------------------------------#
#--- Generate namelists for JULES ---#
#------------------------------------#

#---> Created on Wed Jan 15 15:33:07 2020

#---> @author: Murilo Vianna
"""

#--- Get libraries
import os
import pandas as pd

#--- Current dir
wdnow   = os.getcwd()

laptop = False

if laptop:
    #--- Repository Directory
    wd      = 'C:/Murilo/CSSP-Brazil'
else:
    #--- Repository Directory
    wd      = 'M:/murilo/CSSP/rep/CSSP-Brazil'


#--- Piece of directory within repo pointing to template folder
wdtemp  = '/MONSooN/namelist_template/templates'

#--- Load repo libs
os.chdir(wd+'/code/bin')
from check_s import check_s
import gen_namelist_defs as gd

setup_fn = 'jules_var_setupv2.csv'

#--- Open csv as DataFrame with variables setup
var_setup = pd.DataFrame(pd.read_csv(wd+wdtemp+'/'+setup_fn))

#--- Get list of namelists files
l_nlf   = var_setup['file'].unique().tolist()

#--- Create namelists
for nlf in l_nlf:
    
    print('Creating namelist file:',nlf)
        
    #--- Get list of namelists per file    
    l_nl    = var_setup.loc[var_setup['file'] == nlf,'namelist'].unique().tolist()
    
    #--- open namelist file template and store in array
    finp        = open(wd+wdtemp+'/template_'+nlf, "rt")
    temp_array  = finp.readlines()
    
    #--- close template
    finp.close()
    
    #--- Create each namelist
    for nl in l_nl:
        
        print('Creating namelist :',nl)
                        
        #--- Check if namelist exist in template section
        if not check_s(wd+wdtemp+'/template_'+nlf, '&'+nl):
            
            msg = 'Warning: Namelist "'+nl+'" not found on template file "template_'+nlf+'"'
            print(msg)
            # Call warning log
        else:
            
            #--- Retrieve data from var setup            
            l_fin_nl = var_setup.loc[(var_setup['file'] == nlf) & (var_setup['namelist'] == nl),'temp_var']                       
            l_nnl_nl = var_setup.loc[(var_setup['file'] == nlf) & (var_setup['namelist'] == nl),'n_nl'    ]
            
            #--- find unique (templates will have only unique values per namelist i.e. the repetitions are arrays!)
            l_fin_nl_u = list(set(l_fin_nl))
            
            #--- namelist length 
            len_finp = l_fin_nl_u.__len__()
            
            #--- Check if this namelist should be repeated (e.g. output profiles)
            repeat_nl = len(set(l_nnl_nl)) > 1         
            
            #--- Initialize
            create_nl = True
            found_nl  = False
            start_nl  = False            
            found_var = 0
            n_nl      = 0
            l         = 0
            
            #--- Loop-over the namelist file template
            while create_nl:
                
                #--- flag of namelist begin
                if temp_array[l].find('&'+nl) == 0:
                    start_nl = True
                    found_nl = True
                    l+=1
                    if repeat_nl:
                        #--- Update l_fin_nl_u, namelist length and n_nl 
                        l_fin_nl_u  = list(set(l_fin_nl[l_nnl_nl == n_nl]))
                        len_finp    = l_fin_nl_u.__len__()
                        n_nl+=1                        
                
                #--- flag of namelist end
                if temp_array[l].find('/') == 0 and found_nl:
                    create_nl = False
                    start_nl  = False
                    if repeat_nl and n_nl <= len(set(l_nnl_nl)):
                        #--- Keep looking for repeated nl
                        create_nl = True
                        if n_nl == len(set(l_nnl_nl)): 
                            create_nl = False
                
                #--- We are at namelist, try to find/replace
                if start_nl:
                    for l_i in l_fin_nl_u:                        
                        if temp_array[l].find(l_i) >= 0: # -1 = not found
                            
                            #-----------------#
                            #--- Found Ya! ---#
                            #-----------------#
                            
                            #--- Find Variable
                            fin_var = l_i
                            
                            #--- Extract values
                            rep_var = gd.get_var_setup(var_setup, nlf, nl, n_nl, fin_var)
                                
                            temp_array[l] = temp_array[l].replace(fin_var,rep_var)
                            found_var+=1
                     
                #--- next line
                l+=1
                
                #--- When reach last line check if any namelist were found
                if l > (temp_array.__len__()-1):
                    
                    if not found_nl:
                        msg = 'Warning: Namelist "'+nl+'" not found on template file "template_'+nlf+'"'
                        print(msg)
                    
                    if start_nl:
                        msg = 'Warning: Namelist "'+nl+'" found but could not find the "/" closure on template file "template_'+nlf+'"'
                        print(msg)
                        
                    #--- get out of loop
                    create_nl = False
    
    #----------------------#
    #--- Write Namelist ---#
    #----------------------#
    with open(wd+wdtemp+'/'+nlf, 'w') as f:
        for item in temp_array:
            f.write("%s" % item)    
    
    msg = "Namelist file "+nlf+" created."
    print(msg)