## Loading layer
import os # This is is needed in the pyqgis console also
from qgis.core import (
    QgsVectorLayer
)

path_to_mammal_data_clipped_output = "/home/ines/tcc-ccma/outputs/mammal-data-clipped.shp"

# The format is:
# vlayer = QgsVectorLayer(data_source, layer_name, provider_name)
mammal_heat_layer = QgsVectorLayer(path_to_mammal_data_clipped_output, "mammal_heat", "ogr")
if not mammal_heat_layer.isValid():
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(mammal_heat_layer)

 
heatmap = QgsHeatmapRenderer()
heatmap.setRadius(5)

ramp = QgsStyle().defaultStyle().colorRamp('TransGreenRed')

heatmap.setColorRamp(ramp)
mammal_heat_layer.setRenderer(heatmap)

mammal_heat_layer.triggerRepaint()
mammal_heat_layer.loadNamedStyle("/home/ines/tcc-ccma/qgis/TransGreenRed.qml")

