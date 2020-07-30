
## Loading layer
import os # This is is needed in the pyqgis console also
from qgis.core import (
    QgsVectorLayer
)

# get the path to the shapefile e.g. /home/project/data/ports.shp
path_to_brasil_uf_layer = "/home/ines/tcc-ccma/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp"
path_to_corredores_layer = "/home/ines/tcc-ccma/maps/MMA/corredores_ppg7/corredores_ppg7.shp"

# The format is:
# vlayer = QgsVectorLayer(data_source, layer_name, provider_name)
brasil_uf_layer = QgsVectorLayer(path_to_brasil_uf_layer, "Brazil map", "ogr")
if not brasil_uf_layer.isValid():
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(brasil_uf_layer)

corredores_layer = QgsVectorLayer(path_to_corredores_layer, "Corridors map", "ogr")
if not corredores_layer.isValid():
    print("Layer failed to load!")
else:
    QgsProject.instance().addMapLayer(corredores_layer)


