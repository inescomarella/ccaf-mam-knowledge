from qgis import processing

# Parameters do create grid
layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]
crs = layer.crs()
crs.createFromId(4326)
grid = "/home/ines/tcc-ccma/outputs/grid.shp"

# Creating grid
grid_params = {
    'TYPE': 4,
    'HSPACING': 0.25,
    'VSPACING': 0.25,
    'EXTENT': layer,
    'CRS': crs,
    'OUTPUT': grid
    }
    
print("Running the creategrid algorithm...")  
processing.run("qgis:creategrid", grid_params)

# Load vector grid
grid_layer = QgsVectorLayer(grid, "grid", "ogr")
QgsProject.instance().addMapLayer(grid_layer)

# Clipping vector grid
path_to_clipped_output = "/home/ines/tcc-ccma/outputs/grid-clipped.shp"

clip_params = {
    'INPUT': grid_layer,
    'OVERLAY': layer,
    'OUTPUT': path_to_clipped_output
    }

print("Running the clip algorithm...")  
processing.run("qgis:clip", clip_params)

# Add clipped layer
clipped_layer = QgsVectorLayer(path_to_clipped_output, "grid_clipped", "ogr")
QgsProject.instance().addMapLayer(clipped_layer)

# Remove unclipped layer
QgsProject.instance().removeMapLayer(grid_layer)

print("Done!")

