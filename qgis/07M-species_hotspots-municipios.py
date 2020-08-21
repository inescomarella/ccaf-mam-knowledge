from collections import OrderedDict
from PyQt5.QtGui import QColor
from qgis.core import *

layer = QgsProject.instance().mapLayersByName("municipios_joined_unique")[0]

# define ranges: label, lower value, upper value, color name
# in the field named 'random' (attributes table) 
idx = layer.fields().indexFromName('species_un')
max = layer.maximumValue(idx)
int = max/7
int2 = 2*int
int3 = 3*int
int4 = 4*int
int5 = 5*int
int6 = 6*int

values = (
    ('1-19', 1, int, '#FFCDD2'),
    ('20-39', int+1, int2, '#EF9A9A'),
    ('40-58', int2+1, int3, '#EF5350'),
    ('59-78', int3+1, int4, '#F44226'),
    ('79-97', int4+1, int5, '#E53935'),
    ('98-117', int5+1, int6, '#C62828'),
    ('118-137', int6+1, max, '#B71C1C')
)

# create a category for each item in values
ranges = []
for label, lower, upper, color in values:
    symbol = QgsSymbol.defaultSymbol(layer.geometryType())
    symbol.setColor(QColor(color))
    rng = QgsRendererRange(lower, upper, symbol, label)
    ranges.append(rng)

# create the renderer and assign it to a layer
expression = 'species_un' # field name
renderer = QgsGraduatedSymbolRenderer(expression, ranges)
layer.setRenderer(renderer)

# Rename layer
layer.setName("species_hotspots_mun")

iface.mapCanvas().refresh() 