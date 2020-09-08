


from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

layer1 = QgsProject.instance().mapLayersByName("municipios_ba")[0]
layer2 = QgsProject.instance().mapLayersByName("municipios_es")[0]
layers = []
layers.append(layer1)
layers.append(layer2)

output = "/home/ines/tcc-ccma/outputs/municipios-ccma.shp"


processing.run("qgis:mergevectorlayers", {'LAYERS': layers, 'OUTPUT': output})
output_layer = QgsVectorLayer(output, "municipios_ccma", "ogr")
QgsProject.instance().addMapLayer(output_layer)

# Remove layers
QgsProject.instance().removeMapLayer(layer1)
QgsProject.instance().removeMapLayer(layer2)