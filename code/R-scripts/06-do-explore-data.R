# File purpose: Explore mammal records data to generate first and last record
# year plot and table, and the species references table
# Data: 17/11/2020

# Load in libraries
library(tidyverse)
library(openxlsx)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "arrange", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-explore-data.R")

# Load data --------------------------------------------------

data_read <- read.csv("../data/processed-data/clean-mammal-data.csv")
cons_status <- read.csv("../data/raw-data/conservation-status.csv")

# Pre-process data -------------------------------------------

# Add species conservation status
data <- merge(data_read, cons_status, by = "species", all = TRUE)

to_remove <- anti_join(select(data, colnames(data_read)), data_read)
data <- anti_join(data, to_remove)

# Tables -----------------------------------------------------

# List of collection and institutions
collection_institution_df <- do.collection.institution.table(data)

# Organize and clean table
collection_institution_df <-
  collection_institution_df %>%
  mutate(institutionCode = ifelse(str_detect(collectionCode, "UFES") |
    str_detect(collectionCode, "LABEQ"),
  "UFES",
  ifelse(str_detect(collectionCode, "UESC"),
    "UESC",
    ifelse(str_detect(collectionCode, "USP"),
      "USP",
      ifelse(str_detect(collectionCode, "UFRRJ"),
        "UFRRJ",
        ifelse(collectionCode == "MVZ",
          "BNHM",
          institutionCode
        )
      )
    )
  )
  )) %>%
  filter(collectionCode != "Observations") %>%
  unique() %>%
  arrange(institutionCode)

# References of each species
data_reference_table <- do.reference.table(data)

# References of each species distinguishing institutions and collections
species_references_df <-
  data %>%
  group_by(scientificName) %>%
  summarise(
    collection = paste(collectionCode, catalogNumber),
    institution = institutionCode,
    references = citation
  )

# First and last record of each species
species_record_df <-
  data %>%
  mutate(
    International = ifelse(
      International == "",
      "NE",
      International),
    Nacional = ifelse(
      Nacional == "",
      "NE",
      Nacional),
    Regional.BA = ifelse(
      Regional.BA == "",
      "NE",
      Regional.BA),
    Regional.ES = ifelse(
      Regional.ES == "",
      "NE",
      Regional.ES)
  ) %>%
  filter(!str_detect(scientificName, "Felis"), !is.na(scientificName)) %>%
  group_by(species) %>%
  summarise(
    scientificName = unique(scientificName),
    first_record = min(as.numeric(year), na.rm = TRUE),
    last_record = max(as.numeric(year), na.rm = TRUE),
    International = unique(International),
    Nacional = unique(Nacional),
    Regional.BA = unique(Regional.BA),
    Regional.ES = unique(Regional.ES)
  ) %>%
  arrange(by = last_record)

# Plot -------------------------------------------------------

first_record_df <-
  select(species_record_df, first_record)
last_record_df <-
  select(species_record_df, last_record)

colnames(first_record_df) <- "Year"
colnames(last_record_df) <- "Year"

first_record_df$id <- "First record"
last_record_df$id <- "Last record"

records_df <- bind_rows(first_record_df, last_record_df)

plot_through_years <-
  ggplot(records_df, aes(x = Year, fill = id)) +
  geom_histogram() +
  theme_light() +
  ylab("Number of species") +
  xlab("Years") +
  theme(legend.title = element_blank())

# Save -------------------------------------------------------
# Plot
plot_through_years
ggsave("../data/results/first-last-record-plot.pdf",
  width = 5,
  height = 4
)

# Tables
OUT <- createWorkbook()

addWorksheet(OUT, "first-last-record")
addWorksheet(OUT, "species-reference-table")
addWorksheet(OUT, "species-refs-sep-cols")
addWorksheet(OUT, "collections-institutions")
addWorksheet(OUT, "mammal-database")

writeData(OUT, sheet = "first-last-record", x = species_record_df)
writeData(OUT, sheet = "species-reference-table", x = data_reference_table)
writeData(OUT, sheet = "species-refs-sep-cols", x = species_references_df)
writeData(OUT, sheet = "collections-institutions", x = collection_institution_df)
writeData(OUT, sheet = "mammal-database", x = data)

saveWorkbook(OUT, "../data/results/species-table.xlsx", overwrite = TRUE)
