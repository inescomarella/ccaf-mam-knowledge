from qgis.core import *
from qgis.gui import *
import os # This is is needed in the pyqgis console also

print("Reading file paths...")
# Getting the file paths
path_to_brasil_uf_layer = "/home/ines/tcc-ccma/data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp"
path_to_corredores_layer = "/home/ines/tcc-ccma/data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp"

print("Making vectors...")
# Make a vector layer
brasil_uf_layer = QgsVectorLayer(path_to_brasil_uf_layer, "brazil_map", "ogr")
if not brasil_uf_layer.isValid():
    print("Layer brasil_uf_layer failed to load!")
QgsProject.instance().addMapLayer(brasil_uf_layer)

corredores_layer = QgsVectorLayer(path_to_corredores_layer, "corridors_map", "ogr")
if not corredores_layer.isValid():
    print("Layer corredores_layer failed to load!")
QgsProject.instance().addMapLayer(corredores_layer)

print("Setting crs..")
# Setting Coordinate Reference System do cooredores_layer
crs = corredores_layer.crs()
crs.createFromId(4326)
corredores_layer.setCrs(crs)
QgsProject.instance().addMapLayer(corredores_layer)

print("Done!")
