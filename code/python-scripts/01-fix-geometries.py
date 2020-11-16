"""
Small script to fix geometries of the first file argument
using the native QGIS processing algorithm. You may need
to adjust the path to you installation.
"""

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
in_file_ucst = "/home/ines/tcc-ccma/data/raw-data/maps/MMA/ucstodas/ucstodas.shp"
out_file_ucst = "/home/ines/tcc-ccma/data/processed-data/MMA-ucstodas-geom-fixed.shp"

in_file_ucba = "/home/ines/tcc-ccma/data/raw-data/maps/ICMBio/BA/BA.shp"
out_file_ucba = "/home/ines/tcc-ccma/data/processed-data/ICMBio-BA-geom-fixed.shp"

in_file_ucsma = "/home/ines/tcc-ccma/data/raw-data/maps/Dani/ucs_ma/ucs_ma.shp"
out_file_ucsma = "/home/ines/tcc-ccma/data/processed-data/Dani-ucs_ma-geom-fixed.shp"

# Running the algorithm
params_ucst = {
    'INPUT': QgsVectorLayer(in_file_ucst, 'layer1', 'ogr'),
    'OUTPUT': out_file_ucst
    }

params_ucba = {
    'INPUT': QgsVectorLayer(in_file_ucba, 'layer2', 'ogr'),
    'OUTPUT': out_file_ucba
}

params_ucsma = {
    'INPUT': QgsVectorLayer(in_file_ucsma, 'layer3', 'ogr'),
    'OUTPUT': out_file_ucsma
}

feedback = QgsProcessingFeedback()

print("Running the fix geometries algorithm...")
res = processing.run("native:fixgeometries", params_ucst, feedback=feedback)
res = processing.run("native:fixgeometries", params_ucba, feedback=feedback)
res = processing.run("native:fixgeometries", params_ucsma, feedback=feedback)

print("Done!")