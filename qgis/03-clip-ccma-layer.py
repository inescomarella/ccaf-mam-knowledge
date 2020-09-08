# Select the layer - Corredores
corridors_vlayer = QgsProject.instance().mapLayersByName("corridors_map")
corridors_layer = corridors_vlayer[0]
corridors_feats = [feat for feat in corridors_layer.getFeatures()]

# Duplicate the layer, so you can edit the memory layer
corridors_mem_layer = QgsVectorLayer("Polygon?crs=epsg:4326", "ccma", "memory")
corridors_mem_layer_data = corridors_mem_layer.dataProvider()
corridors_attr = corridors_layer.dataProvider().fields().toList()
corridors_mem_layer_data.addAttributes(corridors_attr)
corridors_mem_layer.updateFields()
corridors_mem_layer_data.addFeatures(corridors_feats)

QgsProject.instance().addMapLayer(corridors_mem_layer)

# Remove Corredor Central da Amazonia
corridors_mem_layer.startEditing()
res = corridors_mem_layer.dataProvider().deleteFeatures([1])

# Input layer to clip
ccma_layer = QgsProject.instance().mapLayersByName("ccma")[0]

# Overlay layer to clip
brazil_layer = QgsProject.instance().mapLayersByName("brazil_map")[0]

# Path to the clipped output
output = "/home/ines/tcc-ccma/outputs/ccma-clipped.shp"

params = {'INPUT': ccma_layer,
            'OVERLAY': brazil_layer,
            'OUTPUT': output}

print("Running the fix clip algorithm...")
# Clip funtion
processing.run("qgis:clip", params)

# Clipped layer
ccma_clipped_layer = QgsVectorLayer(output, "ccma_clipped", "ogr")

# Add clipped layer
QgsProject.instance().addMapLayer(ccma_clipped_layer)

print("Removing previous layers!")
# Remove old layer
QgsProject.instance().removeMapLayer(corridors_mem_layer)
QgsProject.instance().removeMapLayer(corridors_layer)

print("Done!")