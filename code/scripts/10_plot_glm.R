library(tidyverse)
library(patchwork)

# Load in data ----------------------------
df <- read.csv("outputs/model_table_glm.csv")

# Make plots ----------------------------
cu_graph <- df %>% 
  mutate(CU = ifelse(CU == 1, "Present", "Absent")) %>%
  ggplot(aes(x = CU, y = fit)) +
  geom_point(aes(x = CU, y = nrec), size = 0.25) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr), alpha = 0.2, fill = "red") +
  labs(y = "Number of records",
       x = "Conservation Unit") +
  theme_light()

proximity_graph <- df %>% 
  ggplot(aes(x = proximity, y = fit)) +
  geom_point(aes(x = proximity, y = nrec), size = 0.25) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr), alpha = 0.2, fill = "red") +
  labs(y = "Number of records",
       x = "Proximity to collection") +
  theme_light()

forest_graph <- df %>% 
  ggplot(aes(x = forestw, y = fit)) +
  geom_point(aes(x = forestw, y = nrec), size = 0.25) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = lwr,
                  ymax = upr), alpha = 0.2, fill = "red") +
  labs(y = "Number of records",
       x = "Relative forest cover") +
  theme_light()

# Export map -------------------------------------------
cu_graph + proximity_graph + forest_graph
ggsave("figs/model_predict.pdf", 
       width = 9,
       height = 3.5)
