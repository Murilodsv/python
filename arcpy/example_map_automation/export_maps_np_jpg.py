#-------------------------------#
#---- Export Maps in ArcMap ----#
#-------------------------------#

#--- Goal: 
#--- Create Multiple Maps from a ARCGIS mxd file

#--- Rationale:
#--- Get properties from pre-existent mxd file
#--- Read CSV file with automation control
#--- Add layers to the mxd project
#--- Classify layers with a template
#--- Modify text in layout view
#--- Export maps as image (e.g. PNG) to folder ~/maps/

#--- Required Data:
#--- mxd project file with layout view and background layers (e.g. borders, rivers, ocean, etc...)
#--- CSV file "export_map.csv" with paramters of automation
#--- Place all files to be exported into ~/data/

#--- Contact:
#--- murilodsv@gmail.com
#-------------------------------#

import arcpy
import csv
import os

#--- Get project path and names
mxd  = arcpy.mapping.MapDocument("current")
mxdp = mxd.filePath.split("\\")
mxdn = mxdp[len(mxdp)-1]
mxdp = mxd.filePath.replace(mxdn, "")
mxddf= arcpy.mapping.ListDataFrames(mxd, "Layers")[0]

#--- csv control file
csv_ctrl_fn = mxdp+"export_map.csv"

fn_i 	= []
tn_i 	= []
t1_i 	= []
t2_i 	= []
t3_i 	= []
t4_i 	= []
t5_i 	= []
t6_i 	= []
on_i 	= []

skip_head = True

#--- read csv control file
with open(csv_ctrl_fn) as csvfile:
    readCSV = csv.reader(csvfile, delimiter=',')
	
    for row in readCSV:
		#--- read line-by-line		
		fn_i_r 	= row[0]
		tn_i_r 	= row[1]
		t1_i_r 	= row[2]
		t2_i_r 	= row[3]
		t3_i_r 	= row[4]
		t4_i_r 	= row[5]
		t5_i_r 	= row[6]
		t6_i_r 	= row[7]
		on_i_r 	= row[8]
		
		if skip_head:
			#--- Skip header
			skip_head = False
		else:
			#--- append					
			fn_i.append(fn_i_r)
			tn_i.append(tn_i_r)
			t1_i.append(t1_i_r)
			t2_i.append(t2_i_r)
			t3_i.append(t3_i_r)
			t4_i.append(t4_i_r)
			t5_i.append(t5_i_r)
			t6_i.append(t6_i_r)
			on_i.append(on_i_r)

#--- Number of maps
nmaps = range(len(fn_i))

#--- Loop over CSV lines
for i in nmaps:

	#--- Get iteration info:	
	fn = fn_i[i]
	tn = tn_i[i]
	t1 = t1_i[i]
	t2 = t2_i[i]
	t3 = t3_i[i]
	t4 = t4_i[i]
	t5 = t5_i[i]
	t6 = t6_i[i]
	on = on_i[i]
	
	#--- New Layer
	nlyr = arcpy.mapping.Layer(mxdp+"data\\"+fn)
	
	#--- Read Template layer
	tlyr= arcpy.mapping.Layer(mxdp+tn)
	
	#--- Classify layer with template
	arcpy.mapping.UpdateLayer(mxddf,nlyr,tlyr, symbology_only = True)
	
	#--- Add Layer to mxd project
	arcpy.mapping.AddLayer(mxddf, nlyr)
	
	#--- List texts
	l_txt = arcpy.mapping.ListLayoutElements(mxd,"TEXT_ELEMENT")
	
	#--- Update texts	
	txt_1_obj = next(t for t in l_txt if t.text == "texto 1")
	if t1 <> '': txt_1_obj.text = t1
	
	txt_2_obj = next(t for t in l_txt if t.text == "texto 2")
	if t2 <> '': txt_2_obj.text = t2
	
	txt_3_obj = next(t for t in l_txt if t.text == "texto 3")
	if t3 <> '': txt_3_obj.text = t3
	
	txt_4_obj = next(t for t in l_txt if t.text == "texto 4")
	if t4 <> '': txt_4_obj.text = t4
	
	txt_5_obj = next(t for t in l_txt if t.text == "texto 5")
	if t5 <> '': txt_5_obj.text = t5
	
	txt_6_obj = next(t for t in l_txt if t.text == "texto 6")
	if t6 <> '': txt_6_obj.text = t6	
	
	#--- Refresh LayoutView
	arcpy.RefreshTOC()
	arcpy.RefreshActiveView()
	
	#--- Export Map in JPEG format
	arcpy.mapping.ExportToJPEG(mxd, mxdp+"maps\\"+on)
	
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