    
## Delete features (remove Corredor central da Amazonia)
# Select the layer
layers = QgsProject.instance().mapLayersByName("Corridors map")
layer = layers[0]

caps = layer.dataProvider().capabilities()
feats = layer.getFeatures()
dfeats = []

for feat in feats:
        if feat['GID0'] == 1:
            dfeats.append(feat.id())
res = layer.dataProvider().deleteFeatures(dfeats)
layer.triggerRepaint()