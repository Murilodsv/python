# -*- coding: utf-8 -*-
"""
Created on Thu Feb 13 21:43:57 2020

@author: muril
"""


    
    v_vec =  pd.DataFrame({'var_in' : np.array([0.0]*len(df_v.index))})
    
    df_v = df_v.drop('var_in', axis=1)
    
    df_v = df_v.join(v_vec)
    
    df_slice = df_v[np.array(v_nmdim)[1:len(v_nmdim)]].drop_duplicates()
    
    big_slice = s[0]
            
    for n_slice in range(len(df_slice.index)):
                
        f   = np.array([True]*len(df_v.index))
        s_f = []
        s_f.append(big_slice)
        
        for n_dim in range(len(df_slice.columns)):
            slice_now = slice(df_slice.iloc[n_slice,n_dim]-1,
                              df_slice.iloc[n_slice,n_dim]) 
            s_f.append(slice_now)
            
            f_dim = (df_v.iloc[:,n_dim+1] == df_slice.iloc[n_slice,n_dim])
            f = f * f_dim        
        
        jules_nc[v][s_f].ravel().data
        
        t = pd.DataFrame({'var_in' : jules_nc[v][s_f].ravel().data}).reset_index()
        
        df_v['var_in'][f] = t['var_in']
    
    df_v
    df_v.to_csv('teste_crop_3_'+out_fn, index = None, header=True) #Don't forget to add '.csv' at the end of the path
    
    d = 'time'
    v = 'lai'
    jules_nc[v].ravel().data
    
    type(v_f_data)
    
    v_shape_f = ()
    
    
    s = []
    for ds in v_shape:
        s.append(slice(0,ds))

    
    
    

    



s = [slice(0,303),slice(0,8)]


var_l = ['lai','canht','npp']

s = [slice(0,304),slice(7,8),slice(0,1),slice(0,1)]

jules_nc[v][s].ravel().data

#--- Melt it?
if fl_melt:
    df = pd.melt(df, id_vars=np.array(v_nmdim))
    df = df.join(pd.DataFrame({'seq' : np.arange(0,len(df.x))}))
    
    
    #--- save a plot when melted
    g = sns.FacetGrid(df, col="variable")
    g = g.map(plt.plot, "time", "value")
    g.savefig(out_fn.replace('.csv','.png'), dpi = 300, quality = 90)

#--- Export to CSV
df.to_csv (out_fn, index = None, header=True) #Don't forget to add '.csv' at the end of the path



dir(jules_nc.variables)

jules_nc.variables.values()
dir(jules_nc.variables.get('sw_down'))

jules_nc.dimensions.keys()
jules_nc.variables.keys()

jules_nc['lai'][:].ravel().data


jules_nc['lai'][:].ravel().data





dir(jules_nc['time'])
jules_nc.ncattrs()

jules_nc.variables.get('time_bounds')

dir(jules_nc.variables.get('lai'))
dir(jules_nc.variables.get('lai'))

jules_nc.variables.get('lai').getValue()
jules_nc.variables.get('lai').ndim
jules_nc.variables.get('lai').get_dims()
jules_nc.variables.get('lai').dimensions
jules_nc.variables.get('lai').shape



jules_nc.variables.get('longitude').shape

jules_nc.variables['longitude'][:].ravel().data


len(jules_nc.variables['lai'][:].ravel().data)

type(jules_nc.ravel())

dir(jules_nc.variables.get('lai'))

jules_nc.variables.get('lai')

df_v = pd.DataFrame({v : jules_nc.variables[v][:,:].ravel().data})

s = slice(1,300,4)



#--- https://towardsdatascience.com/handling-netcdf-files-using-xarray-for-absolute-beginners-111a8ab4463f
ds.values()
ds.dims
jules_nc.variables