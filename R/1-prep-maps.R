# 15/03/2020 Ines Comarella

# Carregando os pacotes ----
x <- c("rgdal", "raster", "rgeos")
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
  readOGR(dsn = "./maps/IBGE/br_unidades_da_federacao", layer = "BRUFE250GC_SIR")
ba <- readOGR(dsn = "./maps/IBGE/BA", layer = "29UFE250GC_SIR")
es <- readOGR(dsn = "./maps/IBGE/ES", layer = "32UFE250GC_SIR")
corredores <-
  readOGR(dsn = "./maps/MMA/corredores_ppg7", layer = "corredores_ppg7")
ucs <- readOGR(dsn = "./maps/MMA/ucstodas", layer = "ucstodas")
florestas.publicas <-
  readOGR(dsn = "./maps/MMA/florestas_publicas", layer = "florestaspublicas")
rppn.es <- readOGR(dsn = "./maps/ICMBio/ES", layer = "ES")
rppn.ba <- readOGR(dsn = "./maps/ICMBio/BA", layer = "BA")

# Classe dos mapas
maps <-
  c(brasil,
    ba,
    es,
    corredores,
    ucs,
    florestas.publicas,
    rppn.ba,
    rppn.es)
lapply(maps, class)

# Coordinate Reference Systems ----
lapply(maps, crs)

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
brasil.alb <- spTransform(brasil, albers)
ba.alb <- spTransform(ba, albers)
es.alb <- spTransform(es, albers)


###### corrigir os mapas do mma e do icmbio para posteriormente fazer a padronização da projeção 