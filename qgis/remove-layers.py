print("Removing layers...")

# Select the layer
mun_ba_layer = QgsProject.instance().mapLayersByName("municipios_ba")[0]
ucba_layer = QgsProject.instance().mapLayersByName("ucs_ba")[0]
ucst_layer = QgsProject.instance().mapLayersByName("ucs_todas")[0]
ucsf_layer = QgsProject.instance().mapLayersByName("ucs_federais")[0]
ucsma_layer = QgsProject.instance().mapLayersByName("ucs_ma")[0]
mammal_layer = QgsProject.instance().mapLayersByName("raw_mammal_data")[0]
corridor_layer = QgsProject.instance().mapLayersByName("corridors_map")[0]
mun_layer1 = QgsProject.instance().mapLayersByName("municipios_ba_clipped")[0]
mun_layer2 = QgsProject.instance().mapLayersByName("municipios_es")[0]
uc_layer1 = QgsProject.instance().mapLayersByName("ucs_ma_clipped")[0]
uc_layer2 = QgsProject.instance().mapLayersByName("ucs_federais_clipped")[0]
uc_layer3 = QgsProject.instance().mapLayersByName("ucs_todas_clipped")[0]
uc_layer4 = QgsProject.instance().mapLayersByName("ucs_ba_clipped")[0]
uc_layer5 = QgsProject.instance().mapLayersByName("ucs_es")[0]
uc_layer6 = QgsProject.instance().mapLayersByName("ucs_estaduais_es")[0]
output_mem_layer = QgsProject.instance().mapLayersByName("output")[0]
grid_layer = QgsProject.instance().mapLayersByName("grid_clipped")[0]

# Remove layer
QgsProject.instance().removeMapLayer(mun_ba_layer)
QgsProject.instance().removeMapLayer(ucba_layer)
QgsProject.instance().removeMapLayer(ucst_layer)
QgsProject.instance().removeMapLayer(ucsf_layer)
QgsProject.instance().removeMapLayer(ucsma_layer)
QgsProject.instance().removeMapLayer(mammal_layer)
QgsProject.instance().removeMapLayer(corridor_layer)
QgsProject.instance().removeMapLayer(mun_layer1)
QgsProject.instance().removeMapLayer(mun_layer2)
QgsProject.instance().removeMapLayer(uc_layer1)
QgsProject.instance().removeMapLayer(uc_layer2)
QgsProject.instance().removeMapLayer(uc_layer3)
QgsProject.instance().removeMapLayer(uc_layer4)
QgsProject.instance().removeMapLayer(uc_layer5)
QgsProject.instance().removeMapLayer(uc_layer6)
QgsProject.instance().removeMapLayer(output_mem_layer)
QgsProject.instance().removeMapLayer(grid_layer)

print("Done!")