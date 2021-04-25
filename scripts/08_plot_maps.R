
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

source("functions/break_5points.R")

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
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(KL_class, levels = class)), color = NA) +
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
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(aes(fill = factor(KG_class, levels = class)), color = NA) +
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
    limits = c(1, max(grid_data$nrec)),
    breaks = break_points(st_drop_geometry(grid_data), nrec, 0, 1),
    labels = break_points(st_drop_geometry(grid_data), nrec, 0, 1)
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
    limits = c(0, max(grid_data$c, na.rm = TRUE)),
    breaks = break_points(st_drop_geometry(grid_data), c, 1, 0),
    labels = break_points(st_drop_geometry(grid_data), c, 1, 0)
  ) +
  theme_light() +
  labs(fill = "Completeness")

CU_map <- grid_data %>%
  filter(!is.na(AMT)) %>%
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggplot() +
  geom_sf(color = NA, aes(fill = CU)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus", discrete = TRUE) +
  theme_light() +
  labs(fill = "Conservation\nUnit")

forest_map <- grid_data %>%
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = forest_cov)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(min(grid_data$forest_cov, na.rm = TRUE), max(grid_data$forest_cov, na.rm = TRUE)),
    breaks = c(min(grid_data$forest_cov, na.rm = TRUE), max(grid_data$forest_cov, na.rm = TRUE)),
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
    limits = c(1, max(grid_data$Sobs, na.rm = TRUE)),
    breaks = break_points(st_drop_geometry(grid_data), Sobs, 0, 1),
    labels = break_points(st_drop_geometry(grid_data), Sobs, 0, 1)
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
    limits = c(min(grid_data$Sest, na.rm = TRUE), 
               max(grid_data$Sest, na.rm = TRUE)),
    breaks = break_points(st_drop_geometry(grid_data), Sest, 0, min(grid_data$Sest, na.rm = TRUE)),
    labels = break_points(st_drop_geometry(grid_data), Sest, 0, min(grid_data$Sest, na.rm = TRUE))
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
    limits = c(round(min(grid_data$elev, na.rm = TRUE), 0), 
               max(grid_data$elev, na.rm = TRUE)),
    breaks = break_points(st_drop_geometry(grid_data), elev, 0, round(min(grid_data$elev, na.rm = TRUE), 0)),
    labels = break_points(st_drop_geometry(grid_data), elev, 0, round(min(grid_data$elev, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Elevation (m)")

elev_distance_map <- grid_data %>%
  filter(!is.na(elev)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = elevd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light() +
  labs(fill = "Elevd")

grid_data <- grid_data %>%
  mutate(AMT = AMT / 10)

AMT_map <- grid_data %>%
  filter(!is.na(AMT)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMT)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      round(min(grid_data$AMT, na.rm = TRUE), 1),
      round(max(grid_data$AMT, na.rm = TRUE), 1)
    ),
    breaks = break_points(st_drop_geometry(grid_data), AMT, 1, round(min(grid_data$AMT, na.rm = TRUE), 1)),
    labels = break_points(st_drop_geometry(grid_data), AMT, 1, round(min(grid_data$AMT, na.rm = TRUE), 1))
  ) +
  theme_light() +
  labs(fill = "Annual Mean\nTemperature (ºC)")

AMT_distance_map <- grid_data %>%
  filter(!is.na(AMTd)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AMTd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

AP_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = AP)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(
    option = "Hypsypops_rubicundus",
    limits = c(
      round(min(grid_data$AP, na.rm = TRUE), 0),
      round(max(grid_data$AP, na.rm = TRUE), 0)
    ),
    breaks = break_points(st_drop_geometry(grid_data), AP, 0, round(min(grid_data$AP, na.rm = TRUE), 0)),
    labels = break_points(st_drop_geometry(grid_data), AP, 0, round(min(grid_data$AP, na.rm = TRUE), 0))
  ) +
  theme_light() +
  labs(fill = "Annual precipitation (mm)")

AP_distance_map <- grid_data %>%
  filter(!is.na(AP)) %>%
  ggplot() +
  geom_sf(size = NA, aes(fill = APd)) +
  geom_sf(data = cus, fill = NA) +
  geom_sf(data = ccaf, fill = NA) +
  scale_fill_fish(option = "Hypsypops_rubicundus") +
  theme_light()

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

plot_grid(p1, p2, p3, ncol = 3, labels = c("(a)", "(b)", "(b)"), label_size = 15)
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
p3 <- AMT_map  + theme_void() +
  labs(fill = "Annual Mean\nTemperature (ºC)") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
p4 <- AP_map  + theme_void() + 
  labs(fill = "Annual\nPrecipation (mm)") +
  theme(legend.text = element_text(size = 15),
        legend.title = element_text(size = 15))
plot_grid(p1, p2, p3, p4, 
          ncol = 4, 
          labels = c("(a)", "(b)", "(c)", "(d)"), 
          label_size = 15)
ggsave("figs/07_envi_vars_map.png",
       width = 15,
       height = 5
)

