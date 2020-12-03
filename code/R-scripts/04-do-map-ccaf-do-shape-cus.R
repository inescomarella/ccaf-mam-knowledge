# File purpose: prepare a clean CU layer
# Date: 01/12/2020

# Load libraries
x <-
  c(
    "sf",
    "tidyverse",
    "sp",
    "rworldmap",
    "cowplot",
    "ggspatial",
    "openxlsx"
  )
lapply(x, library, character.only = TRUE)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Load maps -----------------------------------------------------------------
ucs_ba_ICMBio <-
  st_read(
    dsn = "../data/processed-data",
    layer = "ICMBio-BA-geom-fixed"
  )
ucs_af_MMA <-
  st_read(
    dsn = "../data/processed-data",
    layer = "MMA-ucstodas-geom-fixed"
  )
ucs_af_Dani <-
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
ccaf <- st_read(
  dsn = "../data/processed-data",
  layer = "ccma-clipped",
  check_ring_dir = TRUE
)
institute_pts <-
  st_read(
    dsn = "../data/raw-data/research-institutes.csv",
    crs = CRS("+proj=longlat +datum=WGS84"),
    options = c(
      "X_POSSIBLE_NAMES=longitude",
      "Y_POSSIBLE_NAMES=latitude"
    )
  )

# Process CU maps -----------------------------------------------------------

# CRS missing, first set longlat, then transform to UTM
ucs_af_MMA_longlat <-
  st_set_crs(ucs_af_MMA, CRS("+proj=longlat +datum=WGS84"))


# Set UTM
utm <-
  CRS("+proj=utm +zone=24 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

ucs_af_MMA_utm <- st_transform(ucs_af_MMA_longlat, utm)
ucs_ba_ICMBio_utm <- st_transform(ucs_ba_ICMBio, utm)
ucs_af_Dani_utm <- st_transform(ucs_af_Dani, utm)
ucs_es_ICMBio_utm <- st_transform(ucs_es_ICMBio, utm)
ucs_es_IEMA_utm <- st_transform(ucs_es_IEMA, utm)
ucs_br_ICMBio_utm <- st_transform(ucs_br_ICMBio, utm)
ccaf_utm <- st_transform(ccaf, utm)


# Clip CUs maps
ucs_ba_ICMBio_clipped <-
  st_intersection(st_make_valid(ucs_ba_ICMBio_utm), ccaf_utm)

ucs_af_MMA_clipped <-
  st_intersection(ucs_af_MMA_utm, ccaf_utm)

ucs_af_Dani_clipped <-
  st_intersection(ucs_af_Dani_utm, ccaf_utm)

ucs_es_ICMBio_clipped <-
  st_intersection(ucs_es_ICMBio_utm, ccaf_utm)

ucs_es_IEMA_clipped <-
  st_intersection(ucs_es_IEMA_utm, ccaf_utm)

ucs_br_ICMBio_clipped <-
  st_intersection(ucs_br_ICMBio_utm, ccaf_utm)


# Standardize CU acronyms and names
ucs_br_ICMBio_clipped <-
  ucs_br_ICMBio_clipped %>%
  mutate(name_cu = str_to_upper(nome))

ucs_af_Dani_clipped <-
  ucs_af_Dani_clipped %>%
  mutate(name_cu = str_to_upper(NOME_UC)) %>%
  mutate(acronym = CATEGORIA)

ucs_es_ICMBio_clipped <-
  ucs_es_ICMBio_clipped %>%
  mutate(name_cu = str_to_upper(nome)) %>%
  mutate(acronym = "RPPN")

ucs_ba_ICMBio_clipped <-
  ucs_ba_ICMBio_clipped %>%
  mutate(name_cu = str_to_upper(nome)) %>%
  mutate(acronym = "RPPN")

ucs_af_MMA_clipped <-
  ucs_af_MMA_clipped %>%
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

ucs_es_IEMA_clipped <-
  ucs_es_IEMA_clipped %>%
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
ucs_binded <-
  bind_rows(
    ucs_br_ICMBio_clipped,
    ucs_af_MMA_clipped,
    ucs_af_Dani_clipped,
    ucs_es_IEMA_clipped,
    ucs_es_ICMBio_clipped,
    ucs_ba_ICMBio_clipped
  )


# Standardize CU names
ucs_std <-
  ucs_binded %>%
  mutate(name_cu = sub(pattern = "PARNA", replacement = "PARQUE NACIONAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "REBIO", replacement = "RESERVA BIOLÓGICA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "MONA", replacement = "MONUMENTO NACIONAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "FLONA", replacement = "FLORESTA NACIONAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "REVIS", replacement = "RESERVA DE VIDA SILVESTRE", name_cu)) %>%
  mutate(name_cu = sub(pattern = "RESEX", replacement = "RESERVA EXTRATIVISTA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "APA ", replacement = "ÁREA DE PROTEÇÃO AMBIENTAL ", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "RPPN", replacement = "RESERVA PARTICULAR DO PATRIMÔNIO NATURAL", name_cu)
  ) %>%
  mutate(name_cu = sub(pattern = "PARNAMU", replacement = "PARQUE NATURAL MUNICIPAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PARES ", replacement = "PARQUE ESTADUAL ", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "RDS", replacement = "RESERVA DE DESENVOLVIMENTO SUSTENTÁVEL", name_cu)
  ) %>%
  mutate(name_cu = sub(pattern = "PARMU", replacement = "PARQUE MUNICIPAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ESEC", replacement = "ESTAÇÃO ECOLÓGICA", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "ARIE", replacement = "ÁREA DE RELEVANTE INTERESSE ECOLÓGICO", name_cu)
  ) %>%
  mutate(name_cu = sub(pattern = "PES ", replacement = "PARQUE ESTADUAL ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "APP", replacement = "ÁREA DE PRESERVAÇÃO PERMANENTE", name_cu)) %>%
  mutate(name_cu = sub(pattern = "  ", replacement = " ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "�REA", replacement = "ÁREA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PROTE��O", replacement = "PROTEÇÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ANT�NIO", replacement = "ANTÔNIO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PATRIM�NIO", replacement = "PATRIMÔNIO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "LE�O", replacement = "LEÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "S�O JOS�", replacement = "SÃO JOSÉ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ANDR�", replacement = "ANDRÉ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "�GUIA", replacement = "ÁGUIA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "VOV�", replacement = "VOVÓ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "�GUA", replacement = "ÁGUA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "TR�S", replacement = "TRÊS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "TRI�NGULO", replacement = "TRIÂNGULO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PONT�ES", replacement = "PONTÕES", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ON�A", replacement = "ONÇA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CAPIT�O", replacement = "CAPITÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "VIT�RIA", replacement = "VITÓRIA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CANTASSUR�", replacement = "CANTASSURÍ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ECOL�GICA", replacement = "ECOLÓGICA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "REF�GIO", replacement = "REFÚGIO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "GUIG�", replacement = "GUIGÓ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CHAPAD�O", replacement = "CHAPADÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CHAPADAO", replacement = "CHAPADÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PARA�SO", replacement = "PARAÍSO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "OL�VIO", replacement = "OLÍVIO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PREGUI�A", replacement = "PREGUIÇA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "M�E", replacement = "MÃE", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CALIF�RNIA", replacement = "CALIFÓRNIA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "LEMBRAN�A", replacement = "LEMBRANÇA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "JATOB�", replacement = "JATOBÁ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "C�RREGO", replacement = "CÓRREGO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "IRM�OS", replacement = "IRMÃOS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "FUMA�A", replacement = "FUMAÇA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "UNI�O", replacement = "UNIÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = " F�", replacement = " FÉ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "URU�U", replacement = "URUÇÚ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CASSURUB�", replacement = "CASSURUBÁ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "SUSTENT�VEL", replacement = "SUSTENTÁVEL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "PIRAQUE-A��", replacement = "PIRAQUE-AÇÚ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "BIOL�GICA", replacement = "BIOLÓGICA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CONCEI��O", replacement = "CONCEIÇÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "MUNIC�PAL", replacement = "MUNICIPAL", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ESPERAN�A", replacement = "ESPERANÇA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "SABI�-", replacement = "SABIÁ-", name_cu)) %>%
  mutate(name_cu = sub(pattern = "C�SAR", replacement = "CÉSAR", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ITA�NAS", replacement = "ITAÚNAS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ECOL�GICO", replacement = "ECOLÓGICO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ESTA��O", replacement = "ESTAÇÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "GUIMAR�ES", replacement = "GUIMARÃES", name_cu)) %>%
  mutate(name_cu = sub(pattern = "�LVARO", replacement = "ÁLVARO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "BA�A", replacement = "BAÍA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "FAL�SIAS", replacement = "FALÉSIAS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "MARATA�ZES", replacement = "MARATAÍZES", name_cu)) %>%
  mutate(name_cu = sub(pattern = "LAMEIR�O", replacement = "LAMEIRÃO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ITACAR�", replacement = "ITACARÉ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "MACI�O", replacement = "MACIÇO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "JACUN�M", replacement = "JACUNÉM", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CARA�VA", replacement = "CARAÍVA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ITANGU�", replacement = "ITANGUÁ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "MULEMB�", replacement = "MULEMBÁ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CREJO�", replacement = "CREJOÁ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "D�OSTRA", replacement = "D'OSTRA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "GOIAPABA-A�U", replacement = "GOIAPABA-AÇÚ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "REFUGIO", replacement = "REFÚGIO", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CAPARAO", replacement = "CAPARAÓ", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CESAR", replacement = "CÉSAR", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CÉSAR VINHAS", replacement = "CÉSAR VINHA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CARAÍVA/", replacement = "CARAÍVA", name_cu)) %>%
  mutate(name_cu = sub(pattern = "CARAIVA", replacement = "CARAÍVA", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "AMBIENTAL PONTA DA BALEIA", replacement = "AMBIENTAL DA PONTA DA BALEIA", name_cu)
  ) %>%
  mutate(name_cu = sub(pattern = "BALEIA/ABROLHO", replacement = "BALEIA / ABROLHOS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "ANTONIO", replacement = "ANTÔNIO", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "ESTADUAL FORNO GRANDE", replacement = "ESTADUAL DO FORNO GRANDE", name_cu)
  ) %>%
  mutate(name_cu = sub(pattern = "ITAUNAS", replacement = "ITAÚNAS", name_cu)) %>%
  mutate(name_cu = sub(pattern = "DE RIO PRETO", replacement = "DO RIO PRETO", name_cu)) %>%
  mutate(
    name_cu = sub(pattern = "FLORESTA NACIONAL FLORESTA NACIONAL ", replacement = "FLORESTA NACIONAL ", name_cu)
  ) %>%
  filter(acronym != "<NA>", acronym != "APP", acronym != "BEM TOMB", )

# Plot CCAF map -------------------------------------------------------------

# Transform to longlat to facilitate cuts
ccaf_longlat <-
  st_transform(ccaf_utm, CRS("+proj=longlat +datum=WGS84"))
ucs_std_longlat <-
  st_transform(ucs_std, CRS("+proj=longlat +datum=WGS84"))


# Import world map from rworldmap package
# Remove Antarctica, cause it's an open polygon, and it's very problematic
sPDF <- getMap()[-which(getMap()$ADMIN == "Antarctica"), ]
sPDF <- spTransform(sPDF, CRS = CRS("+proj=longlat +datum=WGS84"))
sPDF_sf <- st_as_sf(sPDF)


# Plot CCAF with the CUs colored by the CU type
ccaf_plot <-
  ggplot() +
  geom_sf(data = ccaf_longlat) +
  geom_sf(data = ucs_std_longlat, aes(fill = acronym), size = 0.3) +
  geom_sf(data = institute_pts) +
  theme_light() +

  # Scale bar in the bottom right
  annotation_scale(location = "br", width_hint = 0.2) +

  # North arrow in the bottom right above scale bar
  annotation_north_arrow(
    location = "br",
    style = north_arrow_fancy_orienteering(text_size = 8),
    height = unit(0.9, "cm"),
    width = unit(0.9, "cm"),
    pad_y = unit(0.26, "in")
  ) +
  theme(
    legend.title = element_blank(),
    legend.key.size = unit(3, "mm"),
    axis.text.x = element_text(angle = 45, hjust = 0.75),
  ) +
  coord_sf(
    # Limits of the ccaf bbox
    xlim = c(-41.87851, -38.62885),
    ylim = c(-21.30178, -13.00164),
    expand = FALSE,

    # Plot axis y in the right
    label_graticule = "SE"
  )


# Plot South America map with a rectangle indicating the ccaf area
sa_rect <-
  sPDF_sf %>%
  filter(Stern == "South America") %>%
  ggplot() +
  geom_sf() +
  geom_rect(
    xmin = -41.87851,
    xmax = -38.62885,
    ymin = -21.30178,
    ymax = -13.00164,
    fill = NA,
    colour = "black",
    size = 0.1
  ) +
  theme_light() +

  # Remove external grid
  theme_minimal_grid() +

  # Remove internal grid
  background_grid("none") +

  # Remove axis
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  )


# Plot maps together
ccaf_final_plot <-
  ggdraw(xlim = c(0, 2), ylim = c(0, 1)) +
  draw_plot(ccaf_plot,
    scale = 1.55,
    hjust = -0.75
  ) +
  draw_plot(sa_rect,
    scale = 0.6,
    vjust = -0.24,
    hjust = 0.23
  )

# Save -----------------------------------------------------------------------

# CUs map
# Dissolve internal lines to write shapefile
st_write(
  st_combine(ucs_std),
  paste0("../data/processed-data/", "/", "CUs-map.shp"),
  delete_layer = TRUE
)

# CCAF map
save_plot(
  "../data/results/CCAF-map.pdf",
  ccaf_final_plot,
  base_width = 5,
  base_height = 7
)

# List of CUs in CCAF
cu_list <-
  st_drop_geometry(ucs_std) %>%
  select(name_cu, acronym) %>%
  unique() %>%
  arrange(name_cu) %>%
  tibble()

OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = cu_list)
saveWorkbook(OUT, "../data/results/CUs-list.xlsx", overwrite = TRUE)
