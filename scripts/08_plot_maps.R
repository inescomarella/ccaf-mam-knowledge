
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

# Map variables ----
class <- factor(c("Very high", "High", "Medium", "Low", "Very low"))

KL_map <- grid_data %>%
  filter(!is.na(AP)) %>%
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
  filter(!is.na(AP)) %>%
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

forest_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = frst_cv)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(pretty(grid_data$frst_cv)), 
               max(pretty(grid_data$frst_cv))),
    breaks = c(min(pretty(grid_data$frst_cv)), 
               max(pretty(grid_data$frst_cv))),
    labels = c("Low", "High")
  ) +
  theme_light() +
  labs(fill = "Relative forest coverage")

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

elev_map <- grid_data %>%
  filter(!is.na(elev)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elev)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(pretty(grid_data$elev)), 
               max(pretty(grid_data$elev))),
    breaks = pretty(grid_data$elev),
    labels = pretty(grid_data$elev)
  ) +
  theme_light() +
  labs(fill = "Elevation (m)")

grid_data <- grid_data %>%
  mutate(MTWM = MTWM * 10,
         MTCM = MTCM * 10)

MTWM_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = MTWM)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$MTWM)),
      max(pretty(grid_data$MTWM))
    ),
    breaks = pretty(grid_data$MTWM),
    labels = pretty(grid_data$MTWM)
  ) +
  theme_light() +
  labs(fill = "Max. Temp.\nWarm. Month(ºC)")

MTCM_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = MTCM)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$MTCM)),
      max(pretty(grid_data$MTCM))
    ),
    breaks = pretty(grid_data$MTCM),
    labels = pretty(grid_data$MTCM)
  ) +
  theme_light() +
  labs(fill = "Min. Temp.\nCold. Month(ºC)")

AP_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      min(pretty(grid_data$AP)),
      max(pretty(grid_data$AP))
    ),
    breaks = pretty(grid_data$AP),
    labels = pretty(grid_data$AP)
  ) +
  theme_light() +
  labs(fill = "Annual precipitation (mm)")

# Save results ------------
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

p1 <- forest_map  + theme_void() + 
  labs(fill = "Relative forest\ncover") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p2 <- elev_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p3 <- MTWM_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p4 <- MTCM_map  + theme_void() +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p5 <- AP_map  + theme_void() + 
  labs(fill = "Annual\nPrecipation (mm)") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
plot_grid(p1, p2, p3, p4, p5,
          ncol = 3, 
          labels = c("(a)", "(b)", "(c)", "(d)", "(e)"), 
          label_size = 15)
ggsave("figs/07_envi_vars_map.png",
       width = 15,
       height = 10
)

