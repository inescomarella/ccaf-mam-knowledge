# File purpose: Explore mammal records data to generate first and last record
# year plot and table, and the species references table
# Data: 17/11/2020

# Load in libraries
xfun::pkg_attach2(c("httr", "rvest", "openxlsx", "tidyverse", "stringi", "openxlsx"))

conflicted::conflict_prefer(name = "filter", winner = "dplyr")
conflicted::conflict_prefer(name = "select", winner = "dplyr")
conflicted::conflict_prefer(name = "arrange", winner = "dplyr")

# Source functions
source("./R-scripts/functions/funs-explore-data.R")

# Load data --------------------------------------------------------

data_read <-
  read.csv("../data/processed-data/clean-mammal-data.csv") %>%
  select(-X)

# Get species conservation status and endemism -------------------
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

# Pre-process data ------------------------------------------------

# Add species conservation status
data <- merge(data_read, mammal_information, by = "species", all = TRUE)

# Remove species not detected
to_remove <- anti_join(select(data, colnames(data_read)), data_read)
data <- anti_join(data, to_remove)

# Tables ----------------------------------------------------------

# List of species
species_list <-
  data %>%
  select(species, family, order, endemic, IUCN, ICMBio) %>%
  arrange(order, family, species) %>%
  unique()

# List of collection and institutions
collection_institution_df <- do.collection.institution.table(data)

# Organize and clean table
collection_institution_df <-
  collection_institution_df %>%
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
    ICMBio = unique(ICMBio),
    Endemic = unique(endemic)
  ) %>%
  arrange(by = last_record)

# Plot ------------------------------------------------------------

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

# Check groups recorded in early 2000
data %>%
  filter(!is.na(year), year != "", year != "NA") %>%
  mutate(year = as.Date(as.character(year), "%Y")) %>%
  group_by(order, species) %>%
  summarise(
    first_record = min(year),
    last_record = max(year)
  ) %>%
  pivot_longer(
    cols = -c(order, species),
    names_to = "record",
    values_to = "year"
  ) %>%
  filter(year > as.Date("2004", "%Y")) %>%
  filter(record == "first_record") %>%
  group_by(order) %>%
  summarise(n = n())

records_df <- data %>%
  mutate(Collection = ifelse(institutionCode == "UFES" | str_detect(institutionCode, "CEPLAC") | institutionCode == "MEL" | institutionCode == "UESC" | institutionCode == "MBML",
    "Local collections",
    ifelse(institutionCode == "",
      "Scientific literature",
      ifelse(institutionCode == "KU" | institutionCode == "LACM" | institutionCode == "USNM" | institutionCode == "BNHM" | institutionCode == "MCZ" | institutionCode == "ROM" | institutionCode == "FMNH" | institutionCode == "CLO" | institutionCode == "UMMZ",
        "International collections",
        ifelse(str_detect(institutionCode, "iNat"),
          "iNaturalist",
          "National collections"
        )
      )
    )
  ))

level_order <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  arrange(desc(n)) %>%
  select(Collection)

colection_contri <- records_df %>%
  group_by(Collection) %>%
  summarise(n = n()) %>%
  mutate(per = n / sum(n))

colection_contri$label <- scales::percent(colection_contri$per)
colection_contri %>%
  filter(per < 0.2) %>%
  select(per) %>%
  sum()

bar_graph <- colection_contri %>%
  ggplot() +
  geom_bar(aes(
    x = factor(Collection, levels = level_order$Collection),
    y = n
  ),
  stat = "identity",
  width = 0.8
  ) +
  xlab(element_blank()) +
  ylab(element_blank()) +
  theme_light()

line_graph_collections <- records_df %>%
  filter(year != "NA") %>%
  mutate(fk_group = "a") %>%
  mutate(year = as.Date(as.character(year), "%Y")) %>%
  group_by(fk_group, Collection, year) %>%
  summarise(n = n()) %>%
  mutate(ncum = cumsum(n)) %>%
  ggplot() +
  scale_x_date(date_labels = "%Y") +
  geom_line(aes(x = year, y = ncum, color = factor(Collection, levels = level_order$Collection))) +
  theme_light() +
  labs(
    color = element_blank(),
    x = element_blank(),
    y = "Cumulative number of records"
  )


# Save ------------------------------------------------------------
# Plot
frst_lst_rcrd_graph
ggsave("../data/results/04-bar-graph-first-last-record.pdf",
  width = 8,
  height = 6
)

line_graph_collections +
  geom_segment(aes(
    x = as.Date("1950", "%Y"),
    y = 2500,
    xend = as.Date("1950", "%Y"),
    yend = 1500
  ),
  arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text(
    x = as.Date("1950", "%Y"),
    y = 2650,
    label = "A"
  ) +
  geom_segment(aes(
    x = as.Date("1988", "%Y"),
    y = 3500,
    xend = as.Date("1988", "%Y"),
    yend = 2500
  ),
  arrow = arrow(length = unit(0.25, "cm"))
  ) +
  geom_text(
    x = as.Date("1988", "%Y"),
    y = 3650,
    label = "B"
  )

ggsave("../data/results/04-line-graph-collections.pdf",
  width = 8,
  height = 6
)

bar_graph
ggsave("../data/results/04-bar-grah-collections.pdf",
  width = 8,
  height = 6
)

# Tables
OUT <- createWorkbook()
Supp <- createWorkbook()

addWorksheet(Supp, "first-last-record")
addWorksheet(OUT, "species-list")
addWorksheet(OUT, "species-reference-table")
addWorksheet(OUT, "species-refs-sep-cols")
addWorksheet(OUT, "collections-institutions")
addWorksheet(OUT, "mammal-database")

writeData(Supp, sheet = "first-last-record", x = species_list)
writeData(OUT, sheet = "species-list", x = species_record_df)
writeData(OUT, sheet = "species-reference-table", x = data_reference_table)
writeData(OUT, sheet = "species-refs-sep-cols", x = species_references_df)
writeData(OUT, sheet = "collections-institutions", x = collection_institution_df)
writeData(OUT, sheet = "mammal-database", x = data)

saveWorkbook(OUT, "../data/results/04-species-table.xlsx", overwrite = TRUE)
saveWorkbook(Supp, "../data/results/04-Supp-table.xlsx", overwrite = TRUE)

# Save workspace ----
save.image("~/tcc-ccma/code/06-do-explore-data.RData")
