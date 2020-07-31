# Get the path to the csv file
path_to_mammal_data_csv = "file:/home/ines/tcc-ccma/R/data-clean.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % ("UTF-8",",", "decimalLongitude", "decimalLatitude","epsg:4326")

# Make a vector layer
mammal_data_vlayer = QgsVectorLayer(path_to_mammal_data_csv,"raw_mammal_data","delimitedtext")

# Check if layer is valid
if not mammal_data_vlayer.isValid():
    print ("Layer not loaded")

# Add raw_mammal_data layer
QgsProject.instance().addMapLayer(mammal_data_vlayer)

# Path to the clipped output
path_to_mammal_data_clipped_output = "/home/ines/tcc-ccma/outputs/mammal-data-clipped.shp"

# Overlay layer to clip
ccma_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]

# Input layer to clip
raw_mammal_data_layer = QgsProject.instance().mapLayersByName("raw_mammal_data")[0]

# Clip funtion
processing.run("qgis:clip", {'INPUT': raw_mammal_data_layer,'OVERLAY': ccma_layer, 'OUTPUT': path_to_mammal_data_clipped_output})

# Clipped layer
mammal_data_layer = QgsVectorLayer(path_to_mammal_data_clipped_output, "mammal_data", "ogr")

# Add clipped layer
QgsProject.instance().addMapLayer(mammal_data_layer)

# Remove raw_mammal_data
to_be_deleted = QgsProject.instance().mapLayersByName('raw_mammal_data')[0]
QgsProject.instance().removeMapLayer(to_be_deleted.id())
