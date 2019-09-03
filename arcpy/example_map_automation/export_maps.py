#-------------------------------#
#---- Export Maps in ArcMap ----#
#-------------------------------#

#--- Goal: 
#--- Create Multiple Maps from a ARCGIS mxd file

#--- Rationale:
#--- Get properties from pre-existent mxd file
#--- List all data layers contained in ~/data/ for map creation
#--- Add layers to the mxd project
#--- Classify layers with a template
#--- Modify text in layout view
#--- Export maps as image (e.g. PNG)

#--- Required Data:
#--- mxd project file with layout view and background layers (e.g. borders, rivers, ocean, etc...)
#--- Place all files to be exported into ~/data/

#--- Contact:
#--- murilodsv@gmail.com
#-------------------------------#

import arcpy
import pandas
import os

#--- Get project path and names
mxd  = arcpy.mapping.MapDocument("current")
mxdp = mxd.filePath.split("\\")
mxdn = mxdp[len(mxdp)-1]
mxdp = mxd.filePath.replace(mxdn, "")
mxddf= arcpy.mapping.ListDataFrames(mxd, "Layers")[0]

#--- csv control file
csv_ctrl_fn = mxdp+"export_map.csv"

#--- read csv control file
csv_ctrl = pandas.read_csv(csv_ctrl_fn)

#--- Number of maps
nmaps = range(len(csv_ctrl))

#--- Loop
for i in nmaps:
	#--- Get iteration info:
	id_i 	= csv_ctrl['ID'][i]
	fn_i 	= csv_ctrl['filedata'][i]
	tn_i 	= csv_ctrl['template'][i]
	t1_i 	= csv_ctrl['texto 1'][i]
	t2_i 	= csv_ctrl['texto 2'][i]
	t3_i 	= csv_ctrl['texto 3'][i]
	t4_i 	= csv_ctrl['texto 4'][i]
	t5_i 	= csv_ctrl['texto 5'][i]
	t6_i 	= csv_ctrl['texto 6'][i]
	on_i 	= csv_ctrl['fileout'][i]
	
	#--- New Layer
	nlyr = arcpy.mapping.Layer(mxdp+"data\\"+fn_i)
	
	#--- Read Template layer
	tlyr= arcpy.mapping.Layer(mxdp+tn_i)
	
	#--- Classify layer with template
	arcpy.mapping.UpdateLayer(mxddf,nlyr,tlyr, symbology_only = True)
	
	#--- Add Layer to mxd project
	arcpy.mapping.AddLayer(mxddf, nlyr)
	
	#--- List texts
	l_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")
	
	#--- Update texts	
	txt_1_obj = next(t for t in l_txt if t.text == "texto 1")
	txt_1_obj.text = t1_i
	
	txt_2_obj = next(t for t in l_txt if t.text == "texto 2")
	txt_2_obj.text = t2_i
	
	txt_3_obj = next(t for t in l_txt if t.text == "texto 3")
	txt_3_obj.text = t3_i
	
	txt_4_obj = next(t for t in l_txt if t.text == "texto 4")
	txt_4_obj.text = t4_i
	
	txt_5_obj = next(t for t in l_txt if t.text == "texto 5")
	txt_5_obj.text = t5_i
	
	txt_6_obj = next(t for t in l_txt if t.text == "texto 6")
	txt_6_obj.text = t6_i	
	
	#--- Refresh LayoutView
	arcpy.RefreshTOC()
	arcpy.RefreshActiveView()
	
	#--- Export Map in PNG format
	arcpy.mapping.ExportToPNG(mxd, mxdp+"maps\\"+on_i)
	
	#--- Remove layer
	arcpy.mapping.RemoveLayer(mxddf, nlyr)
	
	#--- Return original texts
	txt_1_obj.text = "texto 1"
	txt_2_obj.text = "texto 2"
	txt_3_obj.text = "texto 3"
	txt_4_obj.text = "texto 4"
	txt_5_obj.text = "texto 5"
	txt_6_obj.text = "texto 6"
	
	for df in arcpy.mapping.ListDataFrames(mxd):
		for lyr in arcpy.mapping.ListLayers(mxd, "", df):
			if lyr.name == nlyr.name:
				arcpy.mapping.RemoveLayer(df, lyr)
	
#--- end script