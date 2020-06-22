# 15/03/2020 Ines Comarella

# Carregando os pacotes ----
x <- c("rgdal", "raster", "rgeos", "tidyverse", "spatialEco")
lapply(x, library, character.only = TRUE)
options(stringsAsFactors = FALSE)

# Função find_offending_character para crs NA ----
find_offending_character <- function(x, maxStringLength = 256) {
  print(x)
  for (c in 1:maxStringLength) {
    offendingChar <- substr(x, c, c)
    #print(offendingChar) #uncomment if you want the indiv characters printed
    #the next character is the offending multibyte Character
  }
}


# Importando os mapas ----
brasil <-
  readOGR(dsn = "../maps/IBGE/br_unidades_da_federacao", layer = "BRUFE250GC_SIR")
ba <- readOGR(dsn = "../maps/IBGE/BA", layer = "29UFE250GC_SIR")
es <- readOGR(dsn = "../maps/IBGE/ES", layer = "32UFE250GC_SIR")
corredores <-
  readOGR(dsn = "../maps/MMA/corredores_ppg7", layer = "corredores_ppg7")
ucs <- readOGR(dsn = "../maps/MMA/ucstodas", layer = "ucstodas")
florestas_publicas <-
  readOGR(dsn = "../maps/MMA/florestas_publicas", layer = "florestaspublicas")
rppn_es <- readOGR(dsn = "../maps/ICMBio/ES", layer = "ES")
rppn_ba <- readOGR(dsn = "../maps/ICMBio/BA", layer = "BA")

# Classe dos mapas ----
maps <-
  c(brasil,
    ba,
    es,
    corredores,
    ucs,
    florestas_publicas,
    rppn_ba,
    rppn_es)
lapply(maps, class)

# Coordinate Reference Systems ----
lapply(maps, crs)

# Corrigindo projeção dos mapas do MMA -----
# Projection:         longlat
# EPSG code:          5527
# PROJ string:        '+proj=longlat +ellps=aust_SA +towgs84=-67.35,3.88,-38.22'
# PROJ string (ntv2): '+proj=longlat +ellps=aust_SA +nadgrids=sad96_003.gsb +nodefs'
# Reference:          Resolução 01/2005 de 25 de fevereiro de 2005 - IBGE
# Note:               Previously EPSG code 4618 was listed here, but that is actually the older version,
# acording to EPSG database (www.epsg.org).

SAD69 <- CRS("+proj=longlat +ellps=aust_SA +towgs84=-67.35,3.88,-38.22")
crs(corredores) <- SAD69
crs(ucs) <- SAD69
crs(florestas_publicas) <- SAD69


# Padronizando a projeção para Albers Equal Area ----
# Classification: Conic
# Available forms: Forward and inverse, spherical and ellipsoidal
# Defined area: Global
# Alias: aea
# Domain: 2D
# Input type: Geodetic coordinates
# Output type: Projected coordinates
# proj-string: +proj=aea +lat_1=29.5 +lat_2=42.5

albers <- CRS("+proj=aea +lat_1=29.5 +lat_2=42.5")

brasil_alb <- spTransform(brasil, albers)
ba_alb <- spTransform(ba, albers)
es_alb <- spTransform(es, albers)
corredores_alb <- spTransform(corredores, albers)
rppn_es_alb <- spTransform(rppn_es, albers)
rppn_ba_alb <- spTransform(rppn_ba, albers)
florestas_publicas_alb <- spTransform(florestas_publicas, albers)
ucs_alb <- spTransform(ucs, albers)


maps_alb <-
  c(brasil_alb,
    ba_alb,
    es_alb,
    corredores_alb,
    ucs_alb,
    florestas_publicas_alb,
    rppn_ba_alb,
    rppn_es_alb)
lapply(maps_alb, crs)

# Plotando pontos no mapa -----
data_points <- read.csv('../data/data_clean.csv')
plot(corredores)
points(data_points$decimalLongitude, data_points$decimalLatitude, col = "red", cex = .6)

# Tentando remover pontos fora do CCMA (tentar no QGis) -----
coordenadas <- data_points %>% dplyr::select(decimalLatitude, decimalLongitude)

sp::coordinates(coordenadas) = ~decimalLatitude+decimalLongitude


j <- erase.point(coordenadas, corredores, inside = T)

j
p <- coordinates(j)
p <- as.data.frame(p)

plot(corredores)
points(p$decimalLongitude, p$decimalLatitude, col = "red", cex = .6)
  


