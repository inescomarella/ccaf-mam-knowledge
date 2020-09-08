from qgis.core import *
from qgis.gui import *
import os # This is is needed in the pyqgis console also

# Getting the file paths
path_to_brasil_uf_layer = "/home/ines/tcc-ccma/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp"
path_to_corredores_layer = "/home/ines/tcc-ccma/maps/MMA/corredores_ppg7/corredores_ppg7.shp"
path_to_ba_layer = "/home/ines/tcc-ccma/maps/IBGE/ba_municipios/29MUE250GC_SIR.shp"
path_to_es_layer = "/home/ines/tcc-ccma/maps/IBGE/es_municipios/32MUE250GC_SIR.shp"
path_to_ucf_layer = "/home/ines/tcc-ccma/maps/ICMBio/UC_fed_julho_2019/UC_fed_julho_2019.shp"
path_to_uce_es_layer = "/home/ines/tcc-ccma/maps/IEMA/20190510_UCs_estaduais090519shp/UCs_Estaduais190418.shp"
path_to_uc_ba_layer = "/home/ines/tcc-ccma/maps/ICMBio/BA/BA-geometry-fixed.shp"
path_to_uc_es_layer = "/home/ines/tcc-ccma/maps/ICMBio/ES/ES.shp"
path_to_ucs_layer = "/home/ines/tcc-ccma/maps/MMA/ucstodas/ucstodas-geometry-fixed.shp"
path_to_ucs_ma_layer = "/home/ines/tcc-ccma/maps/Dani/ucs_ma/ucs_ma-geometry-fixed.shp"
path_to_csv = "file:/home/ines/tcc-ccma/data/data-all-clean.csv?encoding=%s&delimiter=%s&xField=%s&yField=%s&crs=%s" % ("UTF-8",",", "decimalLongitude", "decimalLatitude","epsg:4326")

# Make a vector layer
brasil_uf_layer = QgsVectorLayer(path_to_brasil_uf_layer, "brazil_map", "ogr")
if not brasil_uf_layer.isValid():
    print("Layer brasil_uf_layer failed to load!")
QgsProject.instance().addMapLayer(brasil_uf_layer)

corredores_layer = QgsVectorLayer(path_to_corredores_layer, "corridors_map", "ogr")
if not corredores_layer.isValid():
    print("Layer corredores_layer failed to load!")
QgsProject.instance().addMapLayer(corredores_layer)

ba_layer = QgsVectorLayer(path_to_ba_layer, "municipios_ba", "ogr")
if not ba_layer.isValid():
    print("Layer ba_layer failed to load!")
QgsProject.instance().addMapLayer(ba_layer)

es_layer = QgsVectorLayer(path_to_es_layer, "municipios_es", "ogr")
if not es_layer.isValid():
    print("Layer es_layer failed to load!")
QgsProject.instance().addMapLayer(es_layer)

ucf_layer = QgsVectorLayer(path_to_ucf_layer, "ucs_federais", "ogr")
if not ucf_layer.isValid():
    print("Layer ucf_layer failed to load!")
QgsProject.instance().addMapLayer(ucf_layer)

uce_layer = QgsVectorLayer(path_to_uce_es_layer, "ucs_estaduais_es", "ogr")
if not uce_layer.isValid():
    print("Layer uce_layer failed to load uce_es!")
QgsProject.instance().addMapLayer(uce_layer)

uc_ba_layer = QgsVectorLayer(path_to_uc_ba_layer, "ucs_ba", "ogr")
if not uc_ba_layer.isValid():
    print("Layer uc_ba_layer failed to load uc_ba_layer!")
QgsProject.instance().addMapLayer(uc_ba_layer)

uc_es_layer = QgsVectorLayer(path_to_uc_es_layer, "ucs_es", "ogr")
if not uc_es_layer.isValid():
    print("Layer uc_es_layer failed to load uc_es_layer!")
QgsProject.instance().addMapLayer(uc_es_layer)

ucs_layer = QgsVectorLayer(path_to_ucs_layer, "ucs_todas", "ogr")
if not ucs_layer.isValid():
    print("Layer ucs_layer failed to load ucs_layer!")
QgsProject.instance().addMapLayer(ucs_layer)

ucsma_layer = QgsVectorLayer(path_to_ucs_ma_layer, "ucs_ma", "ogr")
if not ucsma_layer.isValid():
    print("Layer ucsma_layer failed to load ucsma_layer!")
QgsProject.instance().addMapLayer(ucsma_layer)

vlayer = QgsVectorLayer(path_to_csv,"raw_mammal_data","delimitedtext")
if not vlayer.isValid():
    print ("Layer vlayer not valid")
QgsProject.instance().addMapLayer(vlayer)

# Setting Coordinate Reference System do cooredores_layer
crs = ucs_layer.crs()
crs.createFromId(4326)
ucs_layer.setCrs(crs)
QgsProject.instance().addMapLayer(ucs_layer)

crs = corredores_layer.crs()
crs.createFromId(4326)
corredores_layer.setCrs(crs)
QgsProject.instance().addMapLayer(corredores_layer)

