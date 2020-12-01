# File purpose: prepare a clean CU layer
# Date: 01/12/2020

library(sf)
library(tidyverse)
library(sp)

ucs_ba_ICMBio <-
  st_read(
    dsn = "../data/processed-data",
    layer = "ICMBio-BA-geom-fixed"
  )
ucs_ma_MMA <-
  st_read(
    dsn = "../data/processed-data",
    layer = "MMA-ucstodas-geom-fixed"
  )
ucs_ma_Dani <-
  st_read(
    dsn = "../data/processed-data",
    layer = "Dani-ucs_ma-geom-fixed"
  )
ucs_es_ICMBio <-
  st_read(
    dsn = "../data/raw-data/maps/ICMBio/ES",
    layer = "ES"
  )
ucs_es_IEMA <-
  st_read(
    dsn = "../data/raw-data/maps/IEMA/20190510_UCs_estaduais090519shp",
    layer = "UCs_Estaduais190418"
  )
ucs_br_ICMBio <-
  st_read(
    dsn = "../data/raw-data/maps/ICMBio/UC_fed_julho_2019",
    layer = "UC_fed_julho_2019"
  )
ccma <- st_read(
  dsn = "../data/processed-data",
  layer = "ccma-clipped",
  check_ring_dir = TRUE
)
brasil <- st_read(
  dsn = "../data/raw-data/maps/IBGE/br_unidades_da_federacao",
  layer = "BRUFE250GC_SIR",
  check_ring_dir = TRUE
)

# Set CRS ---------------------------------------------------------------------------------------
# The map is in longlat, so first set the crs, then transform to UTM
ucs_ma_MMA_longlat <- st_set_crs(ucs_ma_MMA, CRS("+proj=longlat +datum=WGS84"))

# Set CRS = UTM
utm <-
  CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ucs_ma_MMA_utm <- st_transform(ucs_ma_MMA_longlat, utm)
ucs_ba_ICMBio_utm <- st_transform(ucs_ba_ICMBio, utm)
ucs_ma_Dani_utm <- st_transform(ucs_ma_Dani, utm)
ucs_es_ICMBio_utm <- st_transform(ucs_es_ICMBio, utm)
ucs_es_IEMA_utm <- st_transform(ucs_es_IEMA, utm)
ucs_br_ICMBio_utm <- st_transform(ucs_br_ICMBio, utm)
ccma_utm <- st_transform(ccma, utm)

# Clip CUs ---------------------------------------------------------------------------------------
ucs_ba_ICMBio_clipped <-
  st_intersection(st_make_valid(ucs_ba_ICMBio_utm), ccma_utm)
ucs_ma_MMA_clipped <-
  st_intersection(ucs_ma_MMA_utm, ccma_utm)
ucs_ma_Dani_clipped <-
  st_intersection(ucs_ma_Dani_utm, ccma_utm)
ucs_es_ICMBio_clipped <-
  st_intersection(ucs_es_ICMBio_utm, ccma_utm)
ucs_es_IEMA_clipped <-
  st_intersection(ucs_es_IEMA_utm, ccma_utm)
ucs_br_ICMBio_clipped <-
  st_intersection(ucs_br_ICMBio_utm, ccma_utm)

# Standardize CU siglas and names -----------------------------------------------------------------
ucs_br_ICMBio_clipped <-
  ucs_br_ICMBio_clipped %>%
  mutate(nome_uc = str_to_upper(nome))

ucs_ma_Dani_clipped <-
  ucs_ma_Dani_clipped %>%
  mutate(nome_uc = str_to_upper(NOME_UC)) %>%
  mutate(sigla = CATEGORIA)

ucs_es_ICMBio_clipped <-
  ucs_es_ICMBio_clipped %>%
  mutate(nome_uc = str_to_upper(nome)) %>%
  mutate(sigla = "RPPN")

ucs_ba_ICMBio_clipped <-
  ucs_ba_ICMBio_clipped %>%
  mutate(nome_uc = str_to_upper(nome)) %>%
  mutate(sigla = "RPPN")

ucs_ma_MMA_clipped <-
  ucs_ma_MMA_clipped %>%
  mutate(nome_uc = str_to_upper(NOME_UC1)) %>%
  mutate(sigla = ifelse(
    str_detect(CATEGORI3, "Particular"),
    "RPPN",
    ifelse(
      str_detect(CATEGORI3, "Monumento"),
      "MONA",
      ifelse(
        str_detect(CATEGORI3, " Biol"),
        "REBIO",
        ifelse(
          str_detect(CATEGORI3, "Desenvolvimento"),
          "RDS",
          ifelse(
            str_detect(CATEGORI3, "Ambiental"),
            "APA",
            ifelse(
              str_detect(CATEGORI3, "Vida"),
              "REVIS",
              ifelse(
                str_detect(CATEGORI3, "o Ecol"),
                "ESEC",
                ifelse(
                  str_detect(CATEGORI3, "Extrativista"),
                  "RESEX",
                  ifelse(
                    str_detect(CATEGORI3, "Interesse"),
                    "ARIE",
                    ifelse(
                      str_detect(NOME_UC1, "FLORESTA NACIONAL"),
                      "FLONA",
                      ifelse(
                        str_detect(NOME_UC1, "PARQUE ESTADUAL"),
                        "PARES",
                        ifelse(
                          str_detect(NOME_UC1, "PARQUE NACIONAL"),
                          "PARNA",
                          ifelse(
                            str_detect(NOME_UC1, "PARQUE MUNIC"),
                            "PARMU",
                            ifelse(
                              str_detect(NOME_UC1, "PARQUE NATURAL MUNIC"),
                              "PARNAMU",
                              NOME_UC1
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  ))
ucs_es_IEMA_clipped <-
  ucs_es_IEMA_clipped %>%
  mutate(nome_uc = str_to_upper(nom_unidad)) %>%
  mutate(sigla = ifelse(
    str_detect(tip_unidad, "Proteção"),
    "APA",
    ifelse(
      str_detect(tip_unidad, "Relevante"),
      "ARIE",
      ifelse(
        str_detect(tip_unidad, "Desenvolvimento"),
        "RDS",
        ifelse(
          str_detect(tip_unidad, "Estadual"),
          "PARES",
          ifelse(
            str_detect(tip_unidad, "Monumento"),
            "MONA",
            ifelse(str_detect(tip_unidad, "Biológica"),
              "REBIO",
              tip_unidad
            )
          )
        )
      )
    )
  ))

# Bind all in a single map
ucs_binded <-
  bind_rows(
    ucs_br_ICMBio_clipped,
    ucs_ma_MMA_clipped,
    ucs_ma_Dani_clipped,
    ucs_es_IEMA_clipped,
    ucs_es_ICMBio_clipped,
    ucs_ba_ICMBio_clipped
  )

# Standardize CU names --------------------------------------------------------------------------
ucs_std <- 
  ucs_binded %>%
  mutate(nome_uc = sub(pattern = "PARNA", replacement = "PARQUE NACIONAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "REBIO", replacement = "RESERVA BIOLÓGICA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "MONA", replacement = "MONUMENTO NACIONAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "FLONA", replacement = "FLORESTA NACIONAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "REVIS", replacement = "RESERVA DE VIDA SILVESTRE", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "RESEX", replacement = "RESERVA EXTRATIVISTA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "APA ", replacement = "ÁREA DE PROTEÇÃO AMBIENTAL ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "RPPN", replacement = "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PARNAMU", replacement = "PARQUE NATURAL MUNICIPAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PARES ", replacement = "PARQUE ESTADUAL ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "RDS", replacement = "RESERVA DE DESENVOLVIMENTO SUSTENTÁVEL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PARMU", replacement = "PARQUE MUNICIPAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ESEC", replacement = "ESTAÇÃO ECOLÓGICA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ARIE", replacement = "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PES ", replacement = "PARQUE ESTADUAL ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "APP", replacement = "ÁREA DE PRESERVAÇÃO PERMANENTE", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "  ", replacement = " ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "�REA", replacement = "ÁREA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PROTE��O", replacement = "PROTEÇÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ANT�NIO", replacement = "ANTÔNIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PATRIM�NIO", replacement = "PATRIMÔNIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "LE�O", replacement = "LEÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "S�O JOS�", replacement = "SÃO JOSÉ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ANDR�", replacement = "ANDRÉ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "�GUIA", replacement = "ÁGUIA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "VOV�", replacement = "VOVÓ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "�GUA", replacement = "ÁGUA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "TR�S", replacement = "TRÊS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "TRI�NGULO", replacement = "TRIÂNGULO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PONT�ES", replacement = "PONTÕES", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ON�A", replacement = "ONÇA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CAPIT�O", replacement = "CAPITÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "VIT�RIA", replacement = "VITÓRIA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CANTASSUR�", replacement = "CANTASSURÍ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ECOL�GICA", replacement = "ECOLÓGICA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "REF�GIO", replacement = "REFÚGIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "GUIG�", replacement = "GUIGÓ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CHAPAD�O", replacement = "CHAPADÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CHAPADAO", replacement = "CHAPADÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PARA�SO", replacement = "PARAÍSO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "OL�VIO", replacement = "OLÍVIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PREGUI�A", replacement = "PREGUIÇA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "M�E", replacement = "MÃE", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CALIF�RNIA", replacement = "CALIFÓRNIA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "LEMBRAN�A", replacement = "LEMBRANÇA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "JATOB�", replacement = "JATOBÁ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "C�RREGO", replacement = "CÓRREGO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "IRM�OS", replacement = "IRMÃOS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "FUMA�A", replacement = "FUMAÇA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "UNI�O", replacement = "UNIÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = " F�", replacement = " FÉ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "URU�U", replacement = "URUÇÚ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CASSURUB�", replacement = "CASSURUBÁ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "SUSTENT�VEL", replacement = "SUSTENTÁVEL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "PIRAQUE-A��", replacement = "PIRAQUE-AÇÚ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "BIOL�GICA", replacement = "BIOLÓGICA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CONCEI��O", replacement = "CONCEIÇÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "MUNIC�PAL", replacement = "MUNICIPAL", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ESPERAN�A", replacement = "ESPERANÇA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "SABI�-", replacement = "SABIÁ-", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "C�SAR", replacement = "CÉSAR", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ITA�NAS", replacement = "ITAÚNAS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ECOL�GICO", replacement = "ECOLÓGICO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ESTA��O", replacement = "ESTAÇÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "GUIMAR�ES", replacement = "GUIMARÃES", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "�LVARO", replacement = "ÁLVARO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "BA�A", replacement = "BAÍA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "FAL�SIAS", replacement = "FALÉSIAS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "MARATA�ZES", replacement = "MARATAÍZES", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "LAMEIR�O", replacement = "LAMEIRÃO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ITACAR�", replacement = "ITACARÉ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "MACI�O", replacement = "MACIÇO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "JACUN�M", replacement = "JACUNÉM", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CARA�VA", replacement = "CARAÍVA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ITANGU�", replacement = "ITANGUÁ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "MULEMB�", replacement = "MULEMBÁ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CREJO�", replacement = "CREJOÁ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "D�OSTRA", replacement = "D'OSTRA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "GOIAPABA-A�U", replacement = "GOIAPABA-AÇÚ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "REFUGIO", replacement = "REFÚGIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CAPARAO", replacement = "CAPARAÓ", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CESAR", replacement = "CÉSAR", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CÉSAR VINHAS", replacement = "CÉSAR VINHA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CARAÍVA/", replacement = "CARAÍVA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "CARAIVA", replacement = "CARAÍVA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "AMBIENTAL PONTA DA BALEIA", replacement = "AMBIENTAL DA PONTA DA BALEIA", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "BALEIA/ABROLHO", replacement = "BALEIA / ABROLHOS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ANTONIO", replacement = "ANTÔNIO", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ESTADUAL FORNO GRANDE", replacement = "ESTADUAL DO FORNO GRANDE", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "ITAUNAS", replacement = "ITAÚNAS", nome_uc)) %>%
  mutate(nome_uc = sub(pattern = "DE RIO PRETO", replacement = "DO RIO PRETO", nome_uc)) %>%
  filter(sigla != "<NA>", sigla != "APP", sigla != "BEM TOMB", )

# Plot ------------------------------------------------------------------------------------------

ccma_longlat <- st_transform(ccma_utm, CRS("+proj=longlat +datum=WGS84"))
ucs_std_longlat <- st_transform(ucs_std, CRS("+proj=longlat +datum=WGS84"))
brasil_longlat <- st_transform(brasil, CRS("+proj=longlat +datum=WGS84"))

ccma_plot <-
  ggplot() +
  geom_sf(data = ccma_longlat) +
  geom_sf(data = ucs_std_longlat) +
  theme_light() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 0.75)
  )

brasil_plot <- 
  ggplot(data = brasil_longlat) +
  geom_sf() +
  geom_rect(xmin = -41.87851, xmax = -38.62885, ymin = -21.30178, ymax = -13.00164, 
            fill = NA, colour = "black", size = 0.05)


library(rworldmap)
sPDF <- getMap()[-which(getMap()$ADMIN=='Antarctica'),]
#transform to robin for the Robinson projection
sPDF <- spTransform(sPDF, CRS=CRS("+proj=longlat +datum=WGS84"))
mapCountryData(sPDF)

sPDF_sf <- st_as_sf(sPDF)

sa_rect <- sPDF_sf %>%
  filter(Stern== "South America") %>%
  ggplot() +
  geom_sf() +
  geom_rect(xmin = -41.87851, xmax = -38.62885, ymin = -21.30178, ymax = -13.00164, 
            fill = NA, colour = "black", size = 0.05)

cowplot::plot_grid(ccma_plot, sa_rect, nrow = 2, rel_widths = c(1, 1))
