# File purpose: Explore mammal records data to generate first and last record
# year plot and table, and the species references table
# Data: 17/11/2020

# Load in libraries
x <-
  c(
    "tidyverse",
    "rgbif",
    "openxlsx"
  )
lapply(x, library, character.only = TRUE)

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")

# Source functions
source("./R-scripts/functions/04-funs-analyze-mammal-records.R")

# Load in clean data
data <- read.csv("../data/processed-data/clean-mammal-data.csv")

# Tables ----------------------------------------------------------------------

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
  group_by(scientificName) %>%
  summarise(
    first_record = min(as.numeric(year), na.rm = TRUE),
    last_record = max(as.numeric(year), na.rm = TRUE)
  )

# Plot -----------------------------------------------------------------------
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

# Save -----------------------------------------------------------------------
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
