# Select the layer - Corredores
layers = QgsProject.instance().mapLayersByName("corridors_map")
layer = layers[0]
feats = [feat for feat in layer.getFeatures()]

# Duplicate the layer, so you can edit
mem_layer = QgsVectorLayer("Polygon?crs=epsg:4326", "ccma", "memory")

mem_layer_data = mem_layer.dataProvider()
attr = layer.dataProvider().fields().toList()
mem_layer_data.addAttributes(attr)
mem_layer.updateFields()
mem_layer_data.addFeatures(feats)

QgsProject.instance().addMapLayer(mem_layer)

# Remove Corredor Central da Amazonia
mem_layer.startEditing()
res = mem_layer.dataProvider().deleteFeatures([1])

# Clip CCMA to keep just continental area
path_to_ccma_clipped_output = "/home/ines/tcc-ccma/outputs/ccma-clipped.shp"

ccma_layer = QgsProject.instance().mapLayersByName("ccma")[0]
brazil_layer = QgsProject.instance().mapLayersByName("brazil_map")[0]

QgsProject.instance().addMapLayer(ccma_layer)
processing.run("qgis:clip", {'INPUT': ccma_layer,'OVERLAY': brazil_layer, 'OUTPUT': path_to_ccma_clipped_output})
ccma_clipped_layer = QgsVectorLayer(path_to_ccma_clipped_output, "ccma_clipped", "ogr")
QgsProject.instance().addMapLayer(ccma_clipped_layer)


# Remove old layer
to_be_deleted = QgsProject.instance().mapLayersByName('corridors_map')[0]
QgsProject.instance().removeMapLayer(to_be_deleted.id())

to_be_deleted = QgsProject.instance().mapLayersByName('ccma')[0]
QgsProject.instance().removeMapLayer(to_be_deleted.id())