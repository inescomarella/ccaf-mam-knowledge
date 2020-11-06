import sys
sys.path.append('/usr/share/qgis/python/plugins')

from processing.core.Processing import Processing
import processing
from qgis.core import (
    QgsApplication,
    QgsProcessingFeedback,
    QgsVectorLayer
)
from qgis.analysis import QgsNativeAlgorithms


print("Initializing QGIS...")
qgs = QgsApplication([], False)
qgs.initQgis()
Processing.initialize()
QgsApplication.processingRegistry().addProvider(QgsNativeAlgorithms())

# Getting the file paths
ccma_path = "/home/ines/tcc-ccma/outputs/ccma-clipped.shp"
aves_path = "file:/home/ines/tcc-ccma/data/gbif-aves-to-clip.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % ("UTF-8",",", "decimalLongitude", "decimalLatitude","epsg:4326")

# Make a vector layer
aves_vlayer = QgsVectorLayer(aves_path,"aves_data","delimitedtext")
if not aves_vlayer.isValid():
    print ("Layer aves_vlayer failed to load!")
QgsProject.instance().addMapLayer(aves_vlayer)

ccma_vlayer = QgsVectorLayer(ccma_path, "ccma_clipped", "ogr")
if not ccma_vlayer.isValid():
    print("Layer ccma_vlayer failed to load!")
QgsProject.instance().addMapLayer(ccma_vlayer)

# Getting the overlay layer
overlay_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]

# Getting the input layers
aves_layer = QgsProject.instance().mapLayersByName("aves_data")[0]

# Output file paths
output = "/home/ines/tcc-ccma/outputs/aves-clipped.shp"

# Running the algorithm
params = {'INPUT': aves_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': output}

print("Running the clip algorithm...")
processing.run("qgis:clip", params)

# Clipped layer
aves_clipped_layer = QgsVectorLayer(output, "aves_clipped", "ogr")

QgsProject.instance().addMapLayer(aves_clipped_layer)

# Export attribute table
QgsVectorFileWriter.writeAsVectorFormat(aves_clipped_layer, "/home/ines/tcc-ccma/data/aves-clean.csv", "utf-8", layer_clipped.crs(),"CSV", layerOptions = ['GEOMETRY=AS_XYZ'])


print("Done!")
