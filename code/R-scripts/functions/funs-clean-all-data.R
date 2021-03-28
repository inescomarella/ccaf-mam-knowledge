# File purpose: functions useful to clean mammal data from papers, GBIF and
# speciesLink
# Using minimal number of objects to save memory and (try to) make it run faster
#
# Date: 16/11/2020

xfun::pkg_attach(c(
  "dplyr",
  "sf",
  "sp",
  "brazilmaps",
  "stringr"
))

conflicted::conflict_prefer("summarise", "dplyr")
conflicted::conflict_prefer("filter", "dplyr")

remove.fossil.iNaturalist <- function(dataset) {
  to_remove <-
    dataset %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             str_detect(institutionCode, "iNaturalist"))
  
  anti_join(dataset, to_remove)
}

rl.synonyms <- function(x) {
  # Manipulate rl_synonyms() S4 object to get "results" and "name" as a
  # dataframe
  # Make the rl_synonyms() applicable to a list of species
  # Args:
  #   x: species name as character

  synym <- rl_synonyms(x, key = rlkey)
  
  result <- synym$result
  
  scientificName <- x
  
  suppressMessages({
    results <- bind_cols(scientificName, result)
  })
  colnames(results)[1] <- "scientificName"

  results
}

name.backbone <- function(x) {
  # Add a scientificName column containing the species name as written in the
  # input
  # Arg:
  #   x: species names as character

  bckbn <- name_backbone(x)
  bckbn$scientificName <- x

  bckbn
}

clip.ccaf <- function(pts) {
  # Use st_intersection() to remove points outside ccaf layer (sf obj)
  #
  # Args:
  #   pts: dataframe with decimalLongitude and decimalLatitude columns
  #   specifying coordinates

  br_longlat <-
    get_brmap(geo = "Brazil") %>%
    st_as_sf() %>%
    st_transform(CRS("+proj=longlat +datum=WGS84"))
  
  ccaf <-
    st_read(
      dsn = "../data/raw-data/maps/MMA/corredores_ppg7",
      layer = "corredores_ppg7",
      check_ring_dir = TRUE
    ) %>%
    st_set_crs(CRS("+proj=longlat +datum=WGS84")) %>%
    filter(str_detect(NOME1, "Mata")) %>%
    st_intersection(br_longlat) %>%
    mutate(NOME1 = "Corredor Ecologico Central da Mata Atlantica")
  
  to_remove <-
    pts %>%
    filter(
      is.na(decimalLongitude) |
        decimalLongitude == "" |
        is.na(decimalLatitude) |
        decimalLatitude == ""
    )
  
  pts <- 
    anti_join(pts, to_remove) %>%
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) %>%
    st_set_crs(CRS("+proj=longlat +datum=WGS84")) %>%
    st_intersection(ccaf)
  
  coords <- as.data.frame(st_coordinates(pts))

  pts <- st_drop_geometry(pts)
  
  pts <- pts[1:(ncol(pts) - 2)]

  pts$decimalLongitude <- coords$X
  pts$decimalLatitude <- coords$Y

  pts
}

only.indentified.species <- function(df) {
  # Remove hybrids, not identified species, correct some writing, get species
  # in acceptedNameUsage
  #
  # Args:
  #   df: dataframe containing the columns "scientificName" and
  #   "acceptedNameUsage"

  to_remove_scientificName <-
    df %>%
    mutate(scientificName = as.character(scientificName)) %>%
    filter(
      scientificName == "" |
      scientificName == " " |
      is.na(scientificName) |
        str_detect(scientificName, " ") == FALSE |
        (str_detect(scientificName, " sp") &
      !str_detect(scientificName, "spinosus"))
    )

  to_remove_acceptedNameUsage <-
    df %>%
    mutate(acceptedNameUsage = as.character(acceptedNameUsage)) %>%
    filter(
      acceptedNameUsage == "" |
        acceptedNameUsage == " " |
        is.na(acceptedNameUsage) |
        str_detect(acceptedNameUsage, " ") == FALSE |
        (str_detect(acceptedNameUsage, " sp") &
           !str_detect(acceptedNameUsage, "spinosus"))
    )
  to_remove <-
    intersect(to_remove_acceptedNameUsage, to_remove_scientificName)

  df <- anti_join(df, to_remove)
  
  # Correct species name in acceptedNameUsage instead of scientificName
  sp_in_acceptedNameUsage <-
    anti_join(to_remove_scientificName, to_remove_acceptedNameUsage)
  df <- anti_join(df, sp_in_acceptedNameUsage)

  sp_in_acceptedNameUsage$scientificName <- sp_in_acceptedNameUsage$acceptedNameUsage

  df <- bind_rows(df, sp_in_acceptedNameUsage) 
  
  df <-
    df %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = "[.]",
      replacement = " "
    )) %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = " gr ",
      replacement = " "
    )) %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = "cf ",
      replacement = " "
    )) %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = "Cf ",
      replacement = ""
    )) %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = " j ",
      replacement = " "
    )) %>%
    mutate(scientificName = str_replace(scientificName,
      pattern = "   ",
      replacement = " "
    ))

  to_remove <-
    df %>%
    filter(str_detect(scientificName, "[?]") |
      str_detect(scientificName, "[/]") |
      str_detect(scientificName, "brido"))

  anti_join(df, to_remove)
  
}
