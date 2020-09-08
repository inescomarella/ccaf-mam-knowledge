from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Select the layer
layer1 = QgsProject.instance().mapLayersByName("municipios_ccma")[0]
layer2 = QgsProject.instance().mapLayersByName("mammal_data")[0]
layer3 = QgsProject.instance().mapLayersByName("ucs_ccma_merged")[0]

# Get output path
output = "/home/ines/tcc-ccma/outputs/municipios_joined.shp"

# Running the algorithm
params1 = {
    'INPUT': layer1,
    'JOIN': layer2,
    'PREDICATE': 0,
    'SUMMARIES': [0, 1],
    'OUTPUT': 'memory:'
    }
    
params2 = {
   'INPUT': res1['OUTPUT'],
   'JOIN': layer3,
   'PREDICATE': 0,
   'SUMMARIES': 1,
   'OUTPUT': output
   } 

print("Joining municipalities and mammmal data...")
res1 = processing.run("qgis:joinbylocationsummary", params1, feedback = feedback)
QgsProject.instance().addMapLayer(res1['OUTPUT'])

print("Joining municipalities and UCs layer...")
res2 = processing.run("qgis:joinbylocationsummary", params2, feedback = feedback)

output_layer = QgsVectorLayer(output, "municipalities_joined", "ogr")
QgsProject.instance().addMapLayer(output_layer)

print("Done!")