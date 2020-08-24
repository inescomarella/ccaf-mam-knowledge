import os # This is is needed in the pyqgis console too
from qgis.core import *
from qgis.gui import *

# Select the layer
overlay_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]
ucba_layer = QgsProject.instance().mapLayersByName("ucs_ba")[0]
ucst_layer = QgsProject.instance().mapLayersByName("ucs_todas")[0]
ucsf_layer = QgsProject.instance().mapLayersByName("ucs_federais")[0]
ucsma_layer = QgsProject.instance().mapLayersByName("ucs_ma")[0]

# Path to the clipped output
ucba_output = "/home/ines/tcc-ccma/outputs/ucs-ba-clipped.shp"
ucst_output = "/home/ines/tcc-ccma/outputs/ucs-todas-clipped.shp"
ucsf_output = "/home/ines/tcc-ccma/outputs/ucs-federais-clipped.shp"
ucsma_output = "/home/ines/tcc-ccma/outputs/ucs-ma-clipped.shp"

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

# Clip funtion
processing.run("qgis:clip", ucba_params)
processing.run("qgis:clip", ucst_params)
processing.run("qgis:clip", ucsf_params)
processing.run("qgis:clip", ucsma_params)

# Clipped layer
ucba_clipped_layer = QgsVectorLayer(ucba_output, "ucs_ba_clipped", "ogr")
ucst_clipped_layer = QgsVectorLayer(ucst_output, "ucs_todas_clipped", "ogr")
ucsf_clipped_layer = QgsVectorLayer(ucsf_output, "ucs_federais_clipped", "ogr")
ucsma_clipped_layer = QgsVectorLayer(ucsma_output, "ucs_ma_clipped", "ogr")

# Add clipped layer
QgsProject.instance().addMapLayer(ucba_clipped_layer)
QgsProject.instance().addMapLayer(ucst_clipped_layer)
QgsProject.instance().addMapLayer(ucsf_clipped_layer)
QgsProject.instance().addMapLayer(ucsma_clipped_layer)

# Remove old layer
QgsProject.instance().removeMapLayer(ucba_layer)
QgsProject.instance().removeMapLayer(ucst_layer)
QgsProject.instance().removeMapLayer(ucsf_layer)
QgsProject.instance().removeMapLayer(ucsma_layer)
