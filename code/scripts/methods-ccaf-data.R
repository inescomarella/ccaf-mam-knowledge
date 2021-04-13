
#http://forest-gis.com/2015/10/classificacao-climatica-de-koppen-geiger-em-shapefile.html/

xfun::pkg_attach(c("tidyverse", "sf", "viridis", "brazilmaps", "raster"))

conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

# Projections
longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load data --------------------------------------------------
ccaf_total_longlat <- 
  read_sf("../data/raw-data/maps/MMA/corredores_ppg7/corredores_ppg7.shp") %>%
  filter(str_detect(NOME1, "Mata")) %>%
  mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica") %>%
  st_set_crs(longlat)

br_longlat <-
  read_sf("../data/raw-data/maps/IBGE/br_unidades_da_federacao/BRUFE250GC_SIR.shp") %>%
  filter(CD_GEOCUF == "32" | CD_GEOCUF == "29") %>%
  st_transform(longlat) %>%
  st_combine()

ccaf_land_longlat <-
  ccaf_total_longlat %>%
  st_intersection(br_longlat) %>%
  st_crop(xmax = -38.7, xmin = -41.87851, ymax = -13.00164, ymin = -21.30178)

koppen_climates_sf_utm <-
  read_sf(
    "../data/raw-data/maps/Alves2013/koppen_2013_vector_/koppen_vector_.shp"
  )  %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf_land_longlat)  %>%
  st_transform(utm)

land_use_spdf <-
  raster(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-2019.tif"
  ) %>%
  crop(as(ccaf_land_longlat, "Spatial")) %>%
  mask(as(ccaf_land_longlat, "Spatial")) %>%
  as(., "SpatialPixelsDataFrame") %>%
  as.data.frame()

land_use_table_df <-
  read.csv(
    "../data/raw-data/maps/mapbiomas/mapbiomas-brazil-collection-50-mataatlantica-area.csv"
  ) %>%
  mutate(id = ifelse(class == 0,
                     "Non observed",
                     ifelse(
                       class < 10,
                       "Forest",
                       ifelse(
                         class > 10 & class < 14 | class == 29 | class == 32,
                         "Non Forest Natural Formation",
                         ifelse(
                           class > 14 & class < 22 | class == 41 | class == 39 | class == 36,
                           "Farming",
                           ifelse(
                             class > 22 & class < 26 | class == 30,
                             "Non vegetated area",
                             ifelse(class == 31 | class == 33,
                                    "Water",
                                    ""
                             )
                           )
                         )
                       )
                     )
  )) %>%
  mutate(value = class) %>%
  select(value, id, class_name)

municipalities_br_IBGE <-
  st_read(
    dsn = "../data/raw-data/maps/IBGE/br_municipios",
    layer = "BR_Municipios_2019-geometry-fixed"
  )

census_IBGE <- read.csv("../data/raw-data/POP2020_20201030_IBGE.csv")

census_2010_es_IBGE <- read.csv("../data/raw-data/total_populacao_espirito_santo.csv")

census_2010_ba_IBGE <- read.csv("../data/raw-data/total_populacao_bahia.csv")

cus_sf <-
  read_sf("../data/processed-data/CUs-map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf_land_longlat)

# Area ----------------------------------------------------------
ccaf_land_area <- 
  ccaf_land_longlat %>%
  st_transform(utm) %>%
  st_area()

ccaf_total_area <- 
  ccaf_total_longlat %>%
  st_transform(utm) %>%
  st_area()

cus_area <- 
  cus_std %>%
  st_transform(longlat) %>%
  st_intersection(ccaf_land_longlat) %>%
  st_combine() %>%
  st_transform(utm) %>%
  st_area()

cus_integral_area <-
  cus_std %>%
  st_transform(longlat) %>%
  st_intersection(ccaf_land_longlat) %>%
  filter(CU_type == "Integral Protection Units") %>%
  st_combine() %>%
  st_transform(utm) %>%
  st_area()

cus_sustainable_area <-
  cus_std %>%
  st_transform(longlat) %>%
  st_intersection(ccaf_land_longlat) %>%
  filter(CU_type == "Sustainable Use Units") %>%
  st_combine() %>%
  st_transform(utm) %>%
  st_area()

cus_rppn_area <-
  cus_std %>%
  st_transform(longlat) %>%
  st_intersection(ccaf_land_longlat) %>%
  filter(acronym == "RPPN") %>%
  st_combine() %>%
  st_transform(utm) %>%
  st_area()

units(ccaf_total_area) <- "hectares"
units(ccaf_land_area) <- "hectares"
units(cus_area) <- "hectares"
units(cus_integral_area) <- "hectares"
units(cus_sustainable_area) <- "hectares"
units(cus_rppn_area) <- "hectares"

cus_integral_area/ccaf_land_area
cus_sustainable_area/ccaf_land_area
cus_area/ccaf_land_area
cus_rppn_area/cus_area

# Number of RPPNs
cus_std %>%
  st_drop_geometry() %>%
  filter(acronym == "RPPN") %>%
  select(name_cu) %>%
  unique() %>%
  nrow()

# Land use  ------------------------------------------------------
colnames(land_use_spdf) <- c("value", "x", "y")

land_use_spdf <-
  merge(land_use_spdf, land_use_table_df)

table <-
  land_use_spdf %>%
  group_by(class_name) %>%
  tally() %>%
  mutate(perc = 100*n/sum(n)) %>%
  arrange(desc(perc))
table

# Climate ----------------------------------------------------------
classes <-
  koppen_climates_sf_utm %>%
  st_drop_geometry() %>%
  select(Classe) %>%
  unique()

areas <-
  koppen_climates_sf_utm %>%
  st_area()

tibble(classes, areas) %>%
  mutate(perc = round(100*areas/sum(areas), 1)) %>%
  arrange(desc(perc))

# Census ----------------------------------------------------------
municipalities_br_IBGE_utm <- 
  st_transform(municipalities_br_IBGE, utm)

municipalities_br_IBGE_clipped <-
  municipalities_br_IBGE_utm %>%
  st_intersection(st_transform(ccaf_land_longlat, utm)) %>%
  filter(SIGLA_UF == "BA" | SIGLA_UF == "ES")


municipalities_br_IBGE_clipped %>% 
  filter(SIGLA_UF == "BA") %>% 
  nrow()

st_drop_geometry(municipalities_br_IBGE_clipped) %>% 
  filter(SIGLA_UF == "BA") %>%
  select(NM_MUN)

census_IBGE %>% 
  select(NOME_MUNICIPIO)

# Estimated population for 2020
census_IBGE %>% 
  filter(UF == "ES" | UF == "BA") %>% 
  filter(NOME_MUNICIPIO %in% municipalities_br_IBGE_clipped$NM_MUN) %>% 
  mutate(POPULACAO_ESTIMADA = as.numeric(POPULACAO_ESTIMADA)) %>%
  select(POPULACAO_ESTIMADA) %>%
  sum()

# Census population in 2010 ES
pop_es <-
  census_2010_es_IBGE %>% 
  mutate(Total.da.população.2010 = as.numeric(Total.da.população.2010)) %>%
  select(Total.da.população.2010) %>%
  sum()

# Census population in 2010 BA
pop_ba <-
  census_2010_ba_IBGE %>% 
  filter(Nome.do.município %in% municipalities_br_IBGE_clipped$NM_MUN) %>% 
  mutate(Total.da.população.2010 = as.numeric(Total.da.população.2010)) %>%
  select(Total.da.população.2010) %>%
  sum()

# Census population in 2010 CCAF
pop_es + pop_ba

