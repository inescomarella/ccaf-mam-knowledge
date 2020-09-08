from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Select the layer
layer1 = QgsProject.instance().mapLayersByName("grid_clipped")[0]
layer2 = QgsProject.instance().mapLayersByName("mammal_data")[0]

# Get output path
output = "/home/ines/tcc-ccma/outputs/grid_joined.shp"

# Running the algorithm
params = {
    'INPUT': layer1,
    'JOIN': layer2,
    'PREDICATE': 0,
    "SUMMARIES": [0, 1],
    'OUTPUT': output
    }

print("Running the join algorithm...")    
processing.run("qgis:joinbylocationsummary", params)

# Joined layer
output_layer = QgsVectorLayer(output, "grid_joined_unique", "ogr")
QgsProject.instance().addMapLayer(output_layer)
