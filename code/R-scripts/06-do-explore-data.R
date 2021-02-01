# File purpose: Explore mammal records data to generate first and last record
# year plot and table, and the species references table
# Data: 17/11/2020

# Load in libraries
xfun::pkg_attach2(c("httr", "rvest", "openxlsx", "tidyverse", "stringi", "openxlsx"))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "arrange", winner = "dplyr")

# Source functions
source("./R-scripts/functions/06-funs-explore-data.R")

# Load data -------------------------------------------------------------------

data_read <-
  read.csv("../data/processed-data/clean-mammal-data.csv") %>%
  select(-X)

# Get species conservation status and endemism --------------------------------
# From Quintela et al. 2020

# Get the source code of the page
scielo_page <- GET("https://www.scielo.br/scielo.php?pid=S0001-37652020000400706&script=sci_arttext")

# Translate the website code
parsed_scielo_page <- content(scielo_page)

# Get tables from page
scielo_tables <- html_table(parsed_scielo_page, fill = TRUE)

# Appendix
mammal_table <- scielo_tables[[3]]

colnames(mammal_table) <- c("species", "IUCN", "ICMBio")

mammal_table_clean <-
  mammal_table[-1, ] %>% filter(IUCN != "" | ICMBio != "")

mammal_information <-
  mammal_table_clean %>%
  mutate(
    endemic = ifelse(
      test = endsWith(species, " *"),
      yes = "Yes",
      no = "No"
    ),
    species = gsub("\\s*\\([^\\)]+\\)", "", as.character(species)),
    species = gsub("[*]", "", as.character(species)),
    species = gsub("  ", " ", as.character(species)),
    species = stri_trim_both(species),
    species = word(species, 1, 2),
    species = ifelse(
      str_detect(species, "hypoxanthus"),
      "Brachyteles hypoxanthus",
      ifelse(str_detect(species, "us geoffroyi"), "Leopardus geoffroyi",
        ifelse(
          str_detect(species, "\teucippe"),
          "Mico leucippe",
          ifelse(
            str_detect(species, "ri sciureus"),
            "Saimiri sciureus",
            ifelse(
              str_detect(species, "Echimys chr\n"),
              "Echimys chrysurus",
              ifelse(
                str_detect(species, "\taurispinosus"),
                "Nyctinomops aurispinosus",
                ifelse(
                  str_detect(species, "rrhinus angustirostris"),
                  "Platyrrhinus angustirostris",
                  species
                )
              )
            )
          )
        )
      )
    )
  ) %>%
  # Following Reis et al., 2017
  mutate(species = ifelse(
    str_detect(species, " cinereus"),
    "Dermanura cinerea",
    species
  )) %>%
  # Following Reis et al., 2017
  mutate(species = ifelse(
    str_detect(species, " gnomus"),
    "Dermanura gnoma",
    species
  ))

# Pre-process data ------------------------------------------------------------

# Add species conservation status
data <- merge(data_read, mammal_information, by = "species", all = TRUE)

# Remove species not detected
to_remove <- anti_join(select(data, colnames(data_read)), data_read)
data <- anti_join(data, to_remove)

# Tables ----------------------------------------------------------------------

# List of species
species_list <-
  data %>%
  select(order, family, species) %>%
  arrange(order, family, species) %>%
  unique()

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
  filter(!is.na(year), year != "", year != "NA") %>%
  group_by(species) %>%
  summarise(
    first_record = min(year),
    last_record = max(year),
    IUCN = unique(IUCN),
    ICMBio = unique(ICMBio)
  ) %>%
  arrange(by = last_record)

# Plot ------------------------------------------------------------------------

frst_lst_rcrd_graph <-
  data %>%
  filter(!is.na(year), year != "", year != "NA") %>%
  mutate(year = as.Date(as.character(year), "%Y")) %>%
  group_by(species) %>%
  summarise(
    first_record = min(year),
    last_record = max(year)
  ) %>%
  pivot_longer(
    cols = -c(species),
    names_to = "record",
    values_to = "year"
  ) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  scale_fill_discrete(labels = c("First record", "Last record")) +
  geom_histogram(aes(x = year, fill = record)) +
  theme_light() +
  ylab("Number of species") +
  xlab("Years") +
  theme(legend.title = element_blank())

# Save ------------------------------------------------------------------------
# Plot
frst_lst_rcrd_graph
ggsave("../data/results/first-last-record-plot.pdf",
  width = 8,
  height = 6
)

# Tables
OUT <- createWorkbook()

addWorksheet(OUT, "species-list")
addWorksheet(OUT, "first-last-record")
addWorksheet(OUT, "species-reference-table")
addWorksheet(OUT, "species-refs-sep-cols")
addWorksheet(OUT, "collections-institutions")
addWorksheet(OUT, "mammal-database")

writeData(OUT, sheet = "species-list", x = species_list)
writeData(OUT, sheet = "first-last-record", x = species_record_df)
writeData(OUT, sheet = "species-reference-table", x = data_reference_table)
writeData(OUT, sheet = "species-refs-sep-cols", x = species_references_df)
writeData(OUT, sheet = "collections-institutions", x = collection_institution_df)
writeData(OUT, sheet = "mammal-database", x = data)

saveWorkbook(OUT, "../data/results/species-table.xlsx", overwrite = TRUE)
