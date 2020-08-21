from collections import OrderedDict
from PyQt5.QtGui import QColor
from qgis.core import *

grid_layer = "/home/ines/tcc-ccma/outputs/municipios_joined.shp"
layer = QgsVectorLayer(grid_layer, "municipios_joined_unique", "ogr")
QgsProject.instance().addMapLayer(layer)

# define ranges: label, lower value, upper value, color name
# in the field named 'random' (attributes table) 
idx = layer.fields().indexFromName('species_co')
max = layer.maximumValue(idx)
int = max/7
int2 = 2*int
int3 = 3*int
int4 = 4*int
int5 = 5*int
int6 = 6*int

values = (
    ('10-235', 10, int, '#FFCDD2'),
    ('236-470', int+1, int2, '#EF9A9A'),
    ('471-706', int2+1, int3, '#EF5350'),
    ('707-941', int3+1, int4, '#F44226'),
    ('942-1177', int4+1, int5, '#E53935'),
    ('1178-1412', int5+1, int6, '#C62828'),
    ('1413-1650', int6+1, max, '#B71C1C')
)

# create a category for each item in values
ranges = []
for label, lower, upper, color in values:
    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(color))
    rng = QgsRendererRange(lower, upper, symbol, label)
    ranges.append(rng)

# create the renderer and assign it to a layer
expression = 'species_co' # field name
renderer = QgsGraduatedSymbolRenderer(expression, ranges)
layer.setRenderer(renderer)

# Rename layer
layer.setName("register_hotspots_mun")

iface.mapCanvas().refresh() 