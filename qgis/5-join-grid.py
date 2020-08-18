from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

layer1 = QgsProject.instance().mapLayersByName("grid_clipped")[0]
layer2 = QgsProject.instance().mapLayersByName("mammal_data")[0]
output = "/home/ines/tcc-ccma/outputs/grid_joined.shp"

params = {
    'INPUT': layer1,
    'JOIN': layer2,
    'PREDICATE': 0,
    "SUMMARIES": [0, 1],
    'OUTPUT': output
    }

processing.run("qgis:joinbylocationsummary", params)
output_layer = QgsVectorLayer(output, "grid_joined_unique", "ogr")
QgsProject.instance().addMapLayer(output_layer)