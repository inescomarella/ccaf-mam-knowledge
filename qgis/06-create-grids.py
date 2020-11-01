from qgis import processing

# Parameters do create grid
layer = QgsProject.instance().mapLayersByName("ccma_clipped")[0]
crs = layer.crs()
crs.createFromId(4326)
grid_025 = "/home/ines/tcc-ccma/outputs/grid-025.shp"
grid_050 = "/home/ines/tcc-ccma/outputs/grid-050.shp"

# Creating grid
grid_025_params = {
    'TYPE': 4,
    'HSPACING': 0.25,
    'VSPACING': 0.25,
    'EXTENT': layer,
    'CRS': crs,
    'OUTPUT': grid_025
    }
grid_050_params = {
    'TYPE': 4,
    'HSPACING': 0.50,
    'VSPACING': 0.50,
    'EXTENT': layer,
    'CRS': crs,
    'OUTPUT': grid_050
    }
    
print("Running the creategrid algorithm...")  
processing.run("qgis:creategrid", grid_025_params)
processing.run("qgis:creategrid", grid_050_params)

# Load vector grid
grid_025_layer = QgsVectorLayer(grid_025, "grid_025", "ogr")
grid_050_layer = QgsVectorLayer(grid_050, "grid_050", "ogr")
QgsProject.instance().addMapLayer(grid_025_layer)
QgsProject.instance().addMapLayer(grid_050_layer)

# Clipping vector grid
grid_025_output = "/home/ines/tcc-ccma/outputs/grid-025-clipped.shp"
grid_050_output = "/home/ines/tcc-ccma/outputs/grid-050-clipped.shp"

clip_params_025 = {
    'INPUT': grid_025_layer,
    'OVERLAY': layer,
    'OUTPUT': grid_025_output
    }
clip_params_050 = {
    'INPUT': grid_050_layer,
    'OVERLAY': layer,
    'OUTPUT': grid_050_output
    }

print("Running the clip algorithm...")  
processing.run("qgis:clip", clip_params_025)
processing.run("qgis:clip", clip_params_050)

# Add clipped layer
clipped_layer_025 = QgsVectorLayer(grid_025_output, "grid_025_clipped", "ogr")
clipped_layer_050 = QgsVectorLayer(grid_050_output, "grid_050_clipped", "ogr")
QgsProject.instance().addMapLayer(clipped_layer_025)
QgsProject.instance().addMapLayer(clipped_layer_050)

# Remove unclipped layer
QgsProject.instance().removeMapLayer(grid_025_layer)
QgsProject.instance().removeMapLayer(grid_050_layer)

print("Done!")

