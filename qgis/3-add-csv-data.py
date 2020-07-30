# get the path to the csv file
path_to_mammal_data_csv = "file:/home/ines/tcc-ccma/R/data-clean.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % ("UTF-8",",", "decimalLongitude", "decimalLatitude","epsg:4326")

#Make a vector layer
mammal_data_layer=QgsVectorLayer(path_to_mammal_data_csv,"mammal_data","delimitedtext")

#Check if layer is valid
if not mammal_data_layer.isValid():
    print ("Layer not loaded")

#Add CSV data    
QgsProject.instance().addMapLayer(mammal_data_layer)
