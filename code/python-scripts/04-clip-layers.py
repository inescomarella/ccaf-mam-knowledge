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

# Getting the overlay layer
overlay_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]

# Getting the input layers
mun_ba_layer = QgsProject.instance().mapLayersByName("municipios_ba")[0]
ucba_layer = QgsProject.instance().mapLayersByName("ucs_ba")[0]
ucst_layer = QgsProject.instance().mapLayersByName("ucs_todas")[0]
ucsf_layer = QgsProject.instance().mapLayersByName("ucs_federais")[0]
ucsma_layer = QgsProject.instance().mapLayersByName("ucs_ma")[0]

# Output file paths
mun_ba_output = "/home/ines/tcc-ccma/data/processed-data/map-municipios-ba-clipped.shp"
ucba_output = "/home/ines/tcc-ccma/data/processed-data/map-ucs-ba-clipped.shp"
ucst_output = "/home/ines/tcc-ccma/data/processed-data/map-ucs-todas-clipped.shp"
ucsf_output = "/home/ines/tcc-ccma/data/processed-data/map-ucs-federais-clipped.shp"
ucsma_output = "/home/ines/tcc-ccma/data/processed-data/map-ucs-ma-clipped.shp"

# Running the algorithm
mun_ba_params = {'INPUT': mun_ba_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': mun_ba_output}

ucba_params = {'INPUT': ucba_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': ucba_output}

ucst_params = {'INPUT': ucst_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': ucst_output}

ucsf_params = {'INPUT': ucsf_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': ucsf_output}

ucsma_params = {'INPUT': ucsma_layer,
            'OVERLAY': overlay_layer,
            'OUTPUT': ucsma_output}

print("Running the clip algorithm...")
processing.run("qgis:clip", mun_ba_params)
processing.run("qgis:clip", ucba_params)
processing.run("qgis:clip", ucst_params)
processing.run("qgis:clip", ucsf_params)
processing.run("qgis:clip", ucsma_params)

# Clipped layer
mun_ba_clipped_layer = QgsVectorLayer(mun_ba_output, "municipios_ba_clipped", "ogr")
ucba_clipped_layer = QgsVectorLayer(ucba_output, "ucs_ba_clipped", "ogr")
ucst_clipped_layer = QgsVectorLayer(ucst_output, "ucs_todas_clipped", "ogr")
ucsf_clipped_layer = QgsVectorLayer(ucsf_output, "ucs_federais_clipped", "ogr")
ucsma_clipped_layer = QgsVectorLayer(ucsma_output, "ucs_ma_clipped", "ogr")

QgsProject.instance().addMapLayer(mun_ba_clipped_layer)
QgsProject.instance().addMapLayer(ucba_clipped_layer)
QgsProject.instance().addMapLayer(ucst_clipped_layer)
QgsProject.instance().addMapLayer(ucsf_clipped_layer)
QgsProject.instance().addMapLayer(ucsma_clipped_layer)

print("Done!")
