# Path to csv file
path_to_csv = "file:/home/ines/tcc-ccma/data/data-all-clean.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % ("UTF-8",",", "decimalLongitude", "decimalLatitude","epsg:4326")

# Make a vector layer
vlayer = QgsVectorLayer(path_to_csv,"raw_mammal_data","delimitedtext")

# Check if layer is valid
if not vlayer.isValid():
    print ("Layer not loaded")
QgsProject.instance().addMapLayer(vlayer)

# Input layer to clip
layer = QgsProject.instance().mapLayersByName("raw_mammal_data")[0]

# Overlay layer to clip
overlay_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]

# Path to output clipped
output = "/home/ines/tcc-ccma/outputs/mammal-data-clipped.shp"

params = {'INPUT': layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': output}

processing.run("qgis:clip", params)

# Add clipped layer
layer_clipped = QgsVectorLayer(output, "mammal_data", "ogr")
QgsProject.instance().addMapLayer(layer_clipped)

# Remove raw_mammal_data
to_be_deleted = QgsProject.instance().mapLayersByName('raw_mammal_data')[0]
QgsProject.instance().removeMapLayer(to_be_deleted.id())

# Export attribute table
QgsVectorFileWriter.writeAsVectorFormat(layer_clipped, "/home/ines/tcc-ccma/data/data-ccma.csv", "utf-8", layer_clipped.crs(),"CSV", layerOptions = ['GEOMETRY=AS_XYZ'])

