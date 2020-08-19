from collections import OrderedDict
from PyQt5.QtGui import QColor
from qgis.core import *

grid_layer = "/home/ines/tcc-ccma/outputs/grid_joined.shp"
layer = QgsVectorLayer(grid_layer, "grid_joined_unique", "ogr")
QgsProject.instance().addMapLayer(layer)

# define ranges: label, lower value, upper value, color name
# in the field named 'random' (attributes table) 
idx = layer.fields().indexFromName('acceptedNa')
max = layer.maximumValue(idx)
int = max/7
int2 = 2*int
int3 = 3*int
int4 = 4*int
int5 = 5*int
int6 = 6*int

values = (
    ('10-182', 10, int, '#FFCDD2'),
    ('183-364', int+1, int2, '#EF9A9A'),
    ('365-546', int2+1, int3, '#EF5350'),
    ('547-728', int3+1, int4, '#F44226'),
    ('729-910', int4+1, int5, '#E53935'),
    ('911-1092', int5+1, int6, '#C62828'),
    ('193-1275', int6+1, max, '#B71C1C')
)

# create a category for each item in values
ranges = []
for label, lower, upper, color in values:
    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(color))
    rng = QgsRendererRange(lower, upper, symbol, label)
    ranges.append(rng)

# create the renderer and assign it to a layer
expression = 'acceptedNa' # field name
renderer = QgsGraduatedSymbolRenderer(expression, ranges)
layer.setRenderer(renderer)

# Rename layer
layer.setName("register_hotspots")

iface.mapCanvas().refresh() 