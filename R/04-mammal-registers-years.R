setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

x <- c("tidyverse", "rgbif", "dplyr", "openxlsx", "ggplot2")
lapply(x, library, character.only = TRUE)

# Input
data <- read.csv("../data/mamm-data-clean.csv")

# First and last record of each species
species_record_df <- data %>%
  group_by(species) %>%
  summarise(first_record = min(as.numeric(year), na.rm = TRUE),
            last_record = max(as.numeric(year), na.rm = TRUE))

# Species taxonomy
species_df <- data.frame(species = sort(unique(data$species)))
species_backbone_df <- bind_rows(apply(X = species_df, MARGIN = 1, FUN = name_backbone))

# Bind data.frames
sp_record_backbone_df <- bind_cols(species_record_df, species_backbone_df[10:11])

# Reorder columns
sp_record_backbone_df_ordered <- sp_record_backbone_df %>% select(species, family, order, first_record, last_record)

# ggplot data.frame ----
first_record_df <- sp_record_backbone_df %>% select(first_record)
last_record_df <- sp_record_backbone_df %>% select(last_record)

colnames(first_record_df) <- 'Year'
colnames(last_record_df) <- 'Year'

first_record_df$id <- 'First record'
last_record_df$id <- 'Last record'

records_df <- bind_rows(first_record_df, last_record_df)

# Plot -----
plot_through_years <- 
  ggplot(records_df, aes(x = Year, fill = id)) + 
  geom_histogram() + 
  theme_light() + 
  ylab("Number of species") + 
  xlab("Years") +  
  theme(legend.title = element_blank())

# Outputs ----
# Plot
plot_through_years
ggsave('../results/first-last-record-plot.pdf',
       width = 5,
       height = 4)
# Table
OUT <- createWorkbook()
addWorksheet(OUT, "Sheet1")
writeData(OUT, sheet = "Sheet1", x = sp_record_backbone_df_ordered)
saveWorkbook(OUT, "../results/first-last-record-table.xlsx", overwrite = TRUE)


