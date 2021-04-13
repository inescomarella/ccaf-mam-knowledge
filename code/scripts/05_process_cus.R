# File purpose: prepare a clean CU layer
# Date: 01/12/2020

# Load libraries
xfun::pkg_attach(c(
  "tidyverse",
  "sf",
  "openxlsx",
  "ggspatial",
  "qgisprocess"
))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

longlat <- sp::CRS("+proj=longlat +datum=WGS84")
utm <-
  sp::CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Load and pre-process maps ----------------------------------

ccaf_utm <-
  read_sf("data/processed/maps/ccaf_map.shp") %>%
  st_transform(utm)

cus_es_ICMBio_utm <-
  read_sf("data/raw/maps/ICMBio/ES/ES.shp") %>%
  st_transform(utm) %>%
  st_intersection(ccaf_utm)

cus_es_IEMA_utm <-
  read_sf("data/raw/maps/IEMA/20190510_UCs_estaduais090519shp/UCs_Estaduais190418.shp") %>%
  st_transform(utm) %>%
  st_intersection(ccaf_utm)

cus_br_ICMBio_utm <-
  read_sf("data/raw/maps/ICMBio/UC_fed_julho_2019/UC_fed_julho_2019.shp") %>%
  st_transform(utm) %>%
  st_intersection(ccaf_utm)

cus_ba_ICMBio <- read_sf("data/raw/maps/ICMBio/BA/BA.shp")

cus_af_MMA <- read_sf("data/raw/maps/MMA/ucstodas/ucstodas.shp")

# Fix geometries ----------------------------------------------

qgis_configure()

cus_ba_ICMBio_fixed <- 
  qgis_run_algorithm(
  "native:fixgeometries",
  INPUT =  cus_ba_ICMBio,
  OUTPUT = qgis_tmp_vector()
)

cus_af_MMA_fixed <- 
  qgis_run_algorithm(
    "native:fixgeometries",
    INPUT =  cus_af_MMA,
    OUTPUT = qgis_tmp_vector()
  )

cus_ba_ICMBio_utm <- 
  read_sf(qgis_output(cus_ba_ICMBio_fixed, "OUTPUT")) %>%
  st_transform(utm) %>%
  st_make_valid() %>%
  st_intersection(ccaf_utm)

cus_af_MMA_utm <-
  read_sf(qgis_output(cus_af_MMA_fixed, "OUTPUT")) %>%
  st_set_crs(longlat) %>%
  st_transform(utm) %>%
  st_intersection(ccaf_utm)

# Process CU maps --------------------------------------------

# Standardize CU acronyms and names
cus_br_ICMBio_utm <-
  cus_br_ICMBio_utm %>%
  mutate(name_cu = str_to_upper(nome)) %>%
  mutate(acronym = sigla)

cus_es_ICMBio_utm <-
  cus_es_ICMBio_utm %>%
  mutate(name_cu = str_to_upper(nome)) %>%
  mutate(acronym = "RPPN")

cus_ba_ICMBio_utm <-
  cus_ba_ICMBio_utm %>%
  mutate(geometry = geom) %>%
  mutate(name_cu = str_to_upper(nome)) %>%
  mutate(acronym = "RPPN")

cus_af_MMA_utm <-
  cus_af_MMA_utm %>%
  mutate(geometry = geom) %>%
  mutate(name_cu = str_to_upper(NOME_UC1)) %>%
  mutate(acronym = ifelse(
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
                        "PES",
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

cus_es_IEMA_utm <-
  cus_es_IEMA_utm %>%
  mutate(name_cu = str_to_upper(nom_unidad)) %>%
  mutate(acronym = ifelse(
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
          "PES",
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
cus_binded <-
  bind_rows(
    cus_br_ICMBio_utm,
    cus_es_IEMA_utm,
    cus_es_ICMBio_utm,
    cus_af_MMA_utm,
    cus_ba_ICMBio_utm
  )

# Standardize CU names
cus_std <-
  cus_binded %>%
  mutate(name_cu = sub(
    pattern = "PARNA",
    replacement = "PARQUE NACIONAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "REBIO",
    replacement = "RESERVA BIOLÓGICA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "MONA",
    replacement = "MONUMENTO NACIONAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "FLONA",
    replacement = "FLORESTA NACIONAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "REVIS",
    replacement = "RESERVA DE VIDA SILVESTRE",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "RESEX",
    replacement = "RESERVA EXTRATIVISTA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "APA ",
    replacement = "ÁREA DE PROTEÇÃO AMBIENTAL ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "RPPN",
    replacement = "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PARNAMU",
    replacement = "PARQUE NATURAL MUNICIPAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PARES ",
    replacement = "PARQUE ESTADUAL ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "RDS",
    replacement = "RESERVA DE DESENVOLVIMENTO SUSTENTÁVEL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PARMU",
    replacement = "PARQUE MUNICIPAL",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ESEC",
    replacement = "ESTAÇÃO ECOLÓGICA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ARIE",
    replacement = "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PES ",
    replacement = "PARQUE ESTADUAL ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "APP",
    replacement = "ÁREA DE PRESERVAÇÃO PERMANENTE",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "  ",
    replacement = " ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "�REA",
    replacement = "ÁREA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PROTE��O",
    replacement = "PROTEÇÃO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ANT�NIO",
    replacement = "ANTÔNIO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PATRIM�NIO",
    replacement = "PATRIMÔNIO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "LE�O",
    replacement = "LEÃO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "S�O JOS�",
    replacement = "SÃO JOSÉ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ANDR�",
    replacement = "ANDRÉ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "�GUIA",
    replacement = "ÁGUIA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "VOV�",
    replacement = "VOVÓ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "�GUA",
    replacement = "ÁGUA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "TR�S",
    replacement = "TRÊS",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "TRI�NGULO",
    replacement = "TRIÂNGULO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "PONT�ES",
    replacement = "PONTÕES",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ON�A",
    replacement = "ONÇA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "CAPIT�O",
    replacement = "CAPITÃO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "VIT�RIA",
    replacement = "VITÓRIA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "CANTASSUR�",
    replacement = "CANTASSURÍ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "ECOL�GICA",
    replacement = "ECOLÓGICA",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "REF�GIO",
    replacement = "REFÚGIO",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "GUIG�",
    replacement = "GUIGÓ",
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "CHAPAD�O",
    replacement = "CHAPADÃO", 
    name_cu
  )) %>%
  mutate(name_cu = sub(
    pattern = "CHAPADAO", 
    replacement = "CHAPADÃO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "PARA�SO", 
    replacement = "PARAÍSO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "OL�VIO", 
    replacement = "OLÍVIO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "PREGUI�A", 
    replacement = "PREGUIÇA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "M�E", 
    replacement = "MÃE", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CALIF�RNIA", 
    replacement = "CALIFÓRNIA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "LEMBRAN�A", 
    replacement = "LEMBRANÇA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "JATOB�", 
    replacement = "JATOBÁ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "C�RREGO", 
    replacement = "CÓRREGO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "IRM�OS", 
    replacement = "IRMÃOS", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "FUMA�A", 
    replacement = "FUMAÇA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "UNI�O", 
    replacement = "UNIÃO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = " F�", 
    replacement = " FÉ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "URU�U", 
    replacement = "URUÇÚ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CASSURUB�", 
    replacement = "CASSURUBÁ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "SUSTENT�VEL", 
    replacement = "SUSTENTÁVEL", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "PIRAQUE-A��", 
    replacement = "PIRAQUE-AÇÚ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "BIOL�GICA", 
    replacement = "BIOLÓGICA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CONCEI��O", 
    replacement = "CONCEIÇÃO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "MUNIC�PAL", 
    replacement = "MUNICIPAL", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ESPERAN�A", 
    replacement = "ESPERANÇA", 
    name_cu)
    ) %>%
  mutate(name_cu = sub(
    pattern = "SABI�-", 
    replacement = "SABIÁ-", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "C�SAR", 
    replacement = "CÉSAR", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ITA�NAS", 
    replacement = "ITAÚNAS", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ECOL�GICO", 
    replacement = "ECOLÓGICO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ESTA��O", 
    replacement = "ESTAÇÃO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "GUIMAR�ES", 
    replacement = "GUIMARÃES", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "�LVARO", 
    replacement = "ÁLVARO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "BA�A", 
    replacement = "BAÍA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "FAL�SIAS", 
    replacement = "FALÉSIAS", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "MARATA�ZES", 
    replacement = "MARATAÍZES", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "LAMEIR�O", 
    replacement = "LAMEIRÃO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ITACAR�", 
    replacement = "ITACARÉ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "MACI�O", 
    replacement = "MACIÇO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "JACUN�M", 
    replacement = "JACUNÉM", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CARA�VA", 
    replacement = "CARAÍVA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ITANGU�", 
    replacement = "ITANGUÁ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "MULEMB�", 
    replacement = "MULEMBÁ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CREJO�", 
    replacement = "CREJOÁ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "D�OSTRA", 
    replacement = "D'OSTRA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "GOIAPABA-A�U", 
    replacement = "GOIAPABA-AÇÚ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "REFUGIO", 
    replacement = "REFÚGIO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CAPARAO", 
    replacement = "CAPARAÓ", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CESAR", 
    replacement = "CÉSAR", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CÉSAR VINHAS", 
    replacement = "CÉSAR VINHA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CARAÍVA/", 
    replacement = "CARAÍVA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "CARAIVA", 
    replacement = "CARAÍVA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "AMBIENTAL PONTA DA BALEIA", 
    replacement = "AMBIENTAL DA PONTA DA BALEIA", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "BALEIA/ABROLHO", 
    replacement = "BALEIA / ABROLHOS", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ANTONIO", 
    replacement = "ANTÔNIO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ESTADUAL FORNO GRANDE", 
    replacement = "ESTADUAL DO FORNO GRANDE", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "ITAUNAS", 
    replacement = "ITAÚNAS", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "DE RIO PRETO", 
    replacement = "DO RIO PRETO", 
    name_cu
    )) %>%
  mutate(name_cu = sub(
    pattern = "FLORESTA NACIONAL FLORESTA NACIONAL ",
    replacement = "FLORESTA NACIONAL ", 
    name_cu
    )) %>%
  filter(
    acronym != "<NA>",
    acronym != "APP",
    acronym != "BEM TOMB",
    acronym != "PAREC"
  )

cus_std <-
  cus_std %>%
  mutate(
    CU_type = ifelse(
      str_detect(acronym, "ESEC") |
        str_detect(acronym, "REBIO") |
        str_detect(acronym, "PARNA") |
        str_detect(acronym, "MONA") |
        str_detect(acronym, "REVIS") |
        str_detect(acronym, "PES") |
        str_detect(acronym, "PARMU"),
      "Integral Protection Units",
      ifelse(
        str_detect(acronym, "APA") |
          str_detect(acronym, "RESEX") |
          str_detect(acronym, "FLONA") |
          str_detect(acronym, "ARIE") |
          str_detect(acronym, "RDF") |
          str_detect(acronym, "RDS") |
          str_detect(acronym, "RPPN"),
        "Sustainable Use Units",
        ""
      )
    )
  )

# Save -------------------------------------------------------

# CUs map
# Dissolve internal lines to write shapefile
st_write(
  st_union(cus_std),
  paste0("data/processed/maps/", "/", "CUs_map.shp"),
  delete_layer = TRUE
)
