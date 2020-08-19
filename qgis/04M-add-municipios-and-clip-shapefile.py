from qgis.core import *
from qgis.gui import *
import os # This is is needed in the pyqgis console also

# get the path to the shapefile e.g. /home/project/data/ports.shp
path_to_ba_layer = "/home/ines/tcc-ccma/maps/IBGE/ba_municipios/29MUE250GC_SIR.shp"
path_to_es_layer = "/home/ines/tcc-ccma/maps/IBGE/es_municipios/32MUE250GC_SIR.shp"

# The format is:
# vlayer = QgsVectorLayer(data_source, layer_name, provider_name)
ba_layer = QgsVectorLayer(path_to_ba_layer, "municipios_ba", "ogr")
if not ba_layer.isValid():
    print("Layer failed to load!")
QgsProject.instance().addMapLayer(ba_layer)

es_layer = QgsVectorLayer(path_to_es_layer, "municipios_es", "ogr")
if not es_layer.isValid():
    print("Layer failed to load!")
QgsProject.instance().addMapLayer(es_layer)

ccma_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]
output = "/home/ines/tcc-ccma/outputs/municipios-ba-clipped.shp"

params = {'INPUT': ba_layer,
            'OVERLAY': ccma_layer,
            'OUTPUT': output}
# Clip funtion
processing.run("qgis:clip", params)

# Clipped layer
ba_clipped_layer = QgsVectorLayer(output, "municipios_ba", "ogr")

# Add clipped layer
QgsProject.instance().addMapLayer(ba_clipped_layer)

# Remove old layer
QgsProject.instance().removeMapLayer(ba_layer)


