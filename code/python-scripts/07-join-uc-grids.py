from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Select the layer
g025_layer = QgsProject.instance().mapLayersByName("grid_025_clipped")[0]
g050_layer = QgsProject.instance().mapLayersByName("grid_050_clipped")[0]
uc_layer = QgsProject.instance().mapLayersByName("ucs_ccma_merged")[0]

# Get output path
g_025_output = "/home/ines/tcc-ccma/data/processed-data/grid-025-ucs-joined.shp"
g_050_output = "/home/ines/tcc-ccma/data/processed-data/grid-050-ucs-joined.shp"

# Running the algorithm
params1 = {
   'INPUT': g025_layer,
   'JOIN': uc_layer,
   'PREDICATE': 0,
   'SUMMARIES': 1,
   'OUTPUT': g_025_output
   } 
params2 = {
   'INPUT': g050_layer,
   'JOIN': uc_layer,
   'PREDICATE': 0,
   'SUMMARIES': 1,
   'OUTPUT': g_050_output
} 

print("Joining grid and UCs layer...")
feedback = QgsProcessingFeedback()

res1 = processing.run("qgis:joinbylocationsummary", params1, feedback = feedback)
res2 = processing.run("qgis:joinbylocationsummary", params2, feedback = feedback)

output_g25_layer = QgsVectorLayer(g_025_output, "grid_025_Ucs_joined", "ogr")
output_g50_layer = QgsVectorLayer(g_050_output, "grid_050_Ucs_joined", "ogr")

QgsProject.instance().addMapLayer(output_g25_layer)
QgsProject.instance().addMapLayer(output_g50_layer)
print("Done!")