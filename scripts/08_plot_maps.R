
# Load libraries
xfun::pkg_attach2(
  c(
    "tidyverse",
    "sf",
    "sp",
    "patchwork",
    "cowplot",
    "fishualize",
    "ggpubr"
  )
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select", "dplyr")
conflicted::conflict_prefer("get_legend", "cowplot")

longlat <- CRS("+proj=longlat +datum=WGS84")

# Load in data -------------------------------------------

grid_data <- read_sf("data/processed/maps/grid_data.shp")

ccaf <- read_sf("data/processed/maps/ccaf_map.shp")

cus <-
  read_sf("data/processed/maps/CUs_map.shp") %>%
  st_transform(longlat) %>%
  st_make_valid() %>%
  st_intersection(ccaf)

# Map variables -------------------------------------------

class <- factor(c("Very high", "High", "Medium", "Low", "Very low"))

KL_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(KL_clss, levels = class)), color = NA) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  theme_light() +
  labs(fill = "Knowledge level")

KG_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(KG_clss, levels = class)), color = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    discrete = TRUE,
    direction = -1
  ) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  theme_light() +
  labs(fill = "Study priority level")

nrec_map <- grid_data %>%
  filter(nrec > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = nrec)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(1, max(pretty(grid_data$nrec))),
    breaks = pretty(grid_data$nrec),
    labels = pretty(grid_data$nrec)
  ) +
  theme_light() +
  labs(fill = "Number of\nRecords")

c_map <- grid_data %>%
  filter(!is.na(c)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = c)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(0, 1),
    breaks = pretty(grid_data$c),
    labels = pretty(grid_data$c)
  ) +
  theme_light() +
  labs(fill = "Completeness")

Sobs_map <- grid_data %>%
  filter(!is.na(Sobs)) %>%
  filter(Sobs > 1) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sobs)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(pretty(grid_data$Sobs)), 
               max(pretty(grid_data$Sobs))),
    breaks = pretty(grid_data$Sobs),
    labels = pretty(grid_data$Sobs)
  ) +
  theme_light() +
  labs(fill = "Species richness\nobserved")

Sest_map <- grid_data %>%
  filter(!is.na(Sest)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = Sest)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(pretty(grid_data$Sest)), 
               max(pretty(grid_data$Sest))),
    breaks = pretty(grid_data$Sest),
    labels = pretty(grid_data$Sest)
  ) +
  theme_light() +
  labs(fill = "Species richness\nestimated")

#BIO3 = Isothermality (BIO2/BIO7) (×100)
bio3_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio3)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio3)),
      max(pretty(grid_data$bio3))
    ),
    breaks = pretty(grid_data$bio3),
    labels = pretty(grid_data$bio3)
  ) +
  theme_light() +
  labs(fill = "Isothermality\n(×100)")

#BIO4 = Temperature Seasonality (standard deviation ×100)
bio4_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio4)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio4)),
      max(pretty(grid_data$bio4))
    ),
    breaks = pretty(grid_data$bio4),
    labels = pretty(grid_data$bio4)
  ) +
  theme_light() +
  labs(fill = "Temperature\nSeasonality")

#BIO8 = Mean Temperature of Wettest Quarter
grid_data <- grid_data %>%
  mutate(bio8 = bio8/10)

bio8_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio8)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio8)),
      max(pretty(grid_data$bio8))
    ),
    breaks = pretty(grid_data$bio8),
    labels = pretty(grid_data$bio8)
  ) +
  theme_light() +
  labs(fill = "Mean Temperature of\nWettest Quarter (ºC)")


#BIO12 = Annual Precipitation
bio12_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio12)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio12)),
      max(pretty(grid_data$bio12))
    ),
    breaks = pretty(grid_data$bio12),
    labels = pretty(grid_data$bio12)
  ) +
  theme_light() +
  labs(fill = "Annual Precipitation\n(mm)")

#BIO13 = Precipitation of Wettest Month
bio13_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio13)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio13)),
      max(pretty(grid_data$bio13))
    ),
    breaks = pretty(grid_data$bio13),
    labels = pretty(grid_data$bio13)
  ) +
  theme_light() +
  labs(fill = "Precipitation of\nWettest Month (mm)")

#BIO18 = Precipitation of Warmest Quarter
bio18_map <- grid_data %>%
  filter(!is.na(KG)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = bio18)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$bio18)),
      max(pretty(grid_data$bio18))
    ),
    breaks = pretty(grid_data$bio18),
    labels = pretty(grid_data$bio18)
  ) +
  theme_light() +
  labs(fill = "Precipitation of\nWarmest Month (mm)")

# Save results -------------------------------------------
KG_map + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
ggsave("figs/07_KG_map.png",
       width = 8,
       height = 6
)

p1 <- KL_map  + theme_void()  +
  labs(fill = "Knowledge\nlevel") + 
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p2 <- nrec_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p3 <- c_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

plot_grid(p1, p2, p3, ncol = 3, labels = c("(a)", "(b)", "(c)"), label_size = 15)
ggsave("figs/07_bio_vars_map.png",
       width = 13,
       height = 8.27
)

colnames(grid_data)

p1 <- bio3_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p2 <- bio4_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p3 <- bio8_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p4 <- bio12_map  + theme_void() + 
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p5 <- bio13_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p6 <- bio18_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))

plot_grid(p1, p2, p3, p4, p5, p6,
          ncol = 3, 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)", "(f)"), 
          label_size = 15)
ggsave("figs/07_envi_vars_map.png",
       width = 15,
       height = 10
)

