from qgis.analysis import QgsNativeAlgorithms
import processing
from processing.core.Processing import Processing
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Select the layer
mun_layer1 = QgsProject.instance().mapLayersByName("municipios_ba_clipped")[0]
mun_layer2 = QgsProject.instance().mapLayersByName("municipios_es")[0]

uc_layer1 = QgsProject.instance().mapLayersByName("ucs_ma_clipped")[0]
uc_layer2 = QgsProject.instance().mapLayersByName("ucs_federais_clipped")[0]
uc_layer3 = QgsProject.instance().mapLayersByName("ucs_todas_clipped")[0]
uc_layer4 = QgsProject.instance().mapLayersByName("ucs_ba_clipped")[0]
uc_layer5 = QgsProject.instance().mapLayersByName("ucs_es")[0]
uc_layer6 = QgsProject.instance().mapLayersByName("ucs_estaduais_es")[0]

# Create list
mun_layers = []
mun_layers.append(mun_layer1)
mun_layers.append(mun_layer2)

uc_layers = []
uc_layers.append(uc_layer1)
uc_layers.append(uc_layer2)
uc_layers.append(uc_layer3)
uc_layers.append(uc_layer4)
uc_layers.append(uc_layer5)
uc_layers.append(uc_layer6)

# Get output path
mun_output = "/home/ines/tcc-ccma/data/processed-data/map-municipios-ccma.shp"
uc_output = "/home/ines/tcc-ccma/data/processed-data/map-ucs-ccma.shp"

# Running the algorithm
mun_params = {
    'LAYERS': mun_layers, 
    'OUTPUT': mun_output
    }
    
uc_params = {
    'LAYERS': uc_layers,
    'OUTPUT': uc_output
    }

print("Running the fix geometries algorithm...")    
processing.run("qgis:mergevectorlayers", mun_params)
processing.run("qgis:mergevectorlayers", uc_params)

# Merged layer
mun_merged_layer = QgsVectorLayer(mun_output, "municipios_ccma", "ogr")
uc_merged_layer = QgsVectorLayer(uc_output, "ucs_ccma_merged", "ogr")

QgsProject.instance().addMapLayer(mun_merged_layer)
QgsProject.instance().addMapLayer(uc_merged_layer)

print("Done!")