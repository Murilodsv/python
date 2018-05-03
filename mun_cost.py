or  r, d, f in os.walk(wd_z):
    for file in f:
        if file[len(file)-4:len(file)] == ".shp":
            input_shp.append(os.path.join(r,file))
            filename.append(os.path.join(file))

mun = QgsVectorLayer("C:/Murilo/GIS/BR_LIMITES/BR_MUN_WGS84.shp", "mun","ogr")
i = 0
for shp in input_shp:
    layer = iface.addVectorLayer(shp, "","ogr")
    if(layer.geometryType() == 2):
        processing.runalg("saga:intersect",
        mun,
        layer,
        1,
        "C:/Murilo/GIS/Zoning/Brazil_Project/Rename/MUN/MUN_"+filename[i]
        )
    i = i +1
