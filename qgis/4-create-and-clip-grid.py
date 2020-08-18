from qgis import processing

# parameters do create grid
layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]
crs = layer.crs()
crs.createFromId(4326)
grid= "/home/ines/tcc-ccma/outputs/grid.shp"

# create grid algorithm
processing.run("qgis:creategrid", {'TYPE': 4,
    'HSPACING': 0.25,
    'VSPACING': 0.25,
    'EXTENT': layer,
    'CRS': crs,
    'OUTPUT': grid})

# Load vector grid
grid_layer = QgsVectorLayer(grid, "grid", "ogr")
QgsProject.instance().addMapLayer(grid_layer)

# Clip vector grid
path_to_clipped_output = "/home/ines/tcc-ccma/outputs/grid-clipped.shp"
overlay_layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]

processing.run("qgis:clip", {'INPUT': grid_layer,
    'OVERLAY': overlay_layer,
    'OUTPUT': path_to_clipped_output})

# Add clipped layer
clipped_layer = QgsVectorLayer(path_to_clipped_output, "grid_clipped", "ogr")
QgsProject.instance().addMapLayer(clipped_layer)

# Remove old layer
QgsProject.instance().removeMapLayer(grid_layer)
