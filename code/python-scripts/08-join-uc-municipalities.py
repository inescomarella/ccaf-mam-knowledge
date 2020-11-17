from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Select the layer
layer1 = QgsProject.instance().mapLayersByName("municipios_ccma")[0]
layer2 = QgsProject.instance().mapLayersByName("ucs_ccma_merged")[0]

# Get output path
output = "/home/ines/tcc-ccma/data/processed-data/municipalities-joined.shp"

# Running the algorithm
params = {
   'INPUT': layer1,
   'JOIN': layer2,
   'PREDICATE': 0,
   'SUMMARIES': 1,
   'OUTPUT': output
   } 
   
print("Joining municipalities and institutions layer...")
res = processing.run("qgis:joinbylocationsummary", params, feedback = feedback)

output_layer = QgsVectorLayer(output, "municipalities_joined", "ogr")
QgsProject.instance().addMapLayer(output_layer)

print("Done!")
