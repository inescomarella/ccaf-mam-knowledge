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

# Path to the clipped output
path_to_ccma_clipped_output = "/home/ines/tcc-ccma/outputs/ccma-clipped.shp"

# Input layer to clip
ccma_layer = QgsProject.instance().mapLayersByName("ccma")[0]

# Overlay layer to clip
brazil_layer = QgsProject.instance().mapLayersByName("brazil_map")[0]

# Clip funtion
processing.run("qgis:clip", {'INPUT': ccma_layer,'OVERLAY': brazil_layer, 'OUTPUT': path_to_ccma_clipped_output})

# Clipped layer
ccma_clipped_layer = QgsVectorLayer(path_to_ccma_clipped_output, "ccma_clipped", "ogr")

# Add clipped layer
QgsProject.instance().addMapLayer(ccma_clipped_layer)

# Remove old layer
QgsProject.instance().removeMapLayer(corridors_mem_layer)
QgsProject.instance().removeMapLayer(corridors_layer)

#to_be_deleted = QgsProject.instance().mapLayersByName('corridors_map')[0]
#QgsProject.instance().removeMapLayer(to_be_deleted.id())

#to_be_deleted = QgsProject.instance().mapLayersByName('ccma')[0]
#QgsProject.instance().removeMapLayer(to_be_deleted.id())