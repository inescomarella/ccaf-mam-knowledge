import os # This is is needed in the pyqgis console also
from qgis.core import *
from qgis.gui import *

# get the path to the shapefile e.g. /home/project/data/ports.shp
path_to_ucf_layer = "/home/ines/tcc-ccma/maps/ICMBio/UC_fed_julho_2019/UC_fed_julho_2019.shp"
path_to_uce_es_layer = "/home/ines/tcc-ccma/maps/IEMA/20190510_UCs_estaduais090519shp/UCs_Estaduais190418.shp"
path_to_uc_ba_layer = "/home/ines/tcc-ccma/maps/ICMBio/BA/BA-geometry-fixed.shp"
path_to_uc_es_layer = "/home/ines/tcc-ccma/maps/ICMBio/ES/ES.shp"
path_to_ucs_layer = "/home/ines/tcc-ccma/maps/MMA/ucstodas/ucstodas-geometry-fixed.shp"

# The format is:
# vlayer = QgsVectorLayer(data_source, layer_name, provider_name)
ucf_layer = QgsVectorLayer(path_to_ucf_layer, "ucs_federais", "ogr")
if not ucf_layer.isValid():
    print("Layer failed to load ucf!")
QgsProject.instance().addMapLayer(ucf_layer)

uce_layer = QgsVectorLayer(path_to_uce_es_layer, "ucs_estaduais_es", "ogr")
if not uce_layer.isValid():
    print("Layer failed to load uce_es!")
QgsProject.instance().addMapLayer(uce_layer)

uc_ba_layer = QgsVectorLayer(path_to_uc_ba_layer, "ucs_ba", "ogr")
if not uc_ba_layer.isValid():
    print("Layer failed to load uc_ba_layer!")
QgsProject.instance().addMapLayer(uc_ba_layer)

uc_es_layer = QgsVectorLayer(path_to_uc_es_layer, "ucs_es", "ogr")
if not uc_es_layer.isValid():
    print("Layer failed to load uc_es_layer!")
QgsProject.instance().addMapLayer(uc_es_layer)

ucs_layer = QgsVectorLayer(path_to_ucs_layer, "ucs_todas", "ogr")
if not ucs_layer.isValid():
    print("Layer failed to load ucs_layer!")
QgsProject.instance().addMapLayer(ucs_layer)

# Setting Coordinate Reference System do cooredores_layer
crs = ucs_layer.crs()
crs.createFromId(4326)
ucs_layer.setCrs(crs)
QgsProject.instance().addMapLayer(ucs_layer)
