# File purpose: functions to let the cleaning script more clean and easy to
# read
# Date: 21/11/2020

xfun::pkg_attach(c(
  "dplyr",
  "CoordinateCleaner",
  "stringr",
  "biogeo",
  "lubridate",
  "reshape2"
))

conflicted::conflict_prefer("filter", "dplyr")

add.PublicationYear <- function(df) {
  # Add publication year to data with a reference but lacking that information
  #
  # Arg:
  #   df: dataframe containing "PublicationYear" and "reference"
  #   columns

  # Extract rows with reference and without PublicationYear
  to_correct <-
    df %>%
    filter(
      is.na(PublicationYear) &
        is.na(reference) == FALSE & reference != "" |
        PublicationYear == "" &
          is.na(reference) == FALSE & reference != ""
    ) %>%
    mutate(PublicationYear = as.character(PublicationYear))

  corrected <-
    to_correct %>%
    mutate(PublicationYear = substr(gsub("[^0-9]", "", reference), 1, 4))

  df <-
    df %>% mutate(PublicationYear = as.character(PublicationYear))

  df <- anti_join(df, to_correct)

  bind_rows(df, corrected)
}

add.eventYear <- function(df) {
  # In the absence of eventYear assume eventDate or PublicationYear
  #
  # Args:
  #   df: dataframe containing "eventYear", "eventDate" and "PublicationYear"
  #   columns

  df %>%
    mutate(
      eventYear = if_else(
        condition = eventYear == "" & eventDate == "",
        true = PublicationYear,
        false = if_else(
          condition = eventYear == "" & eventDate != "",
          true = eventDate,
          false = eventYear
        )
      )
    )
}

correct.eventYear <- function(df) {
  # Correct event year in diff formats, and assume the last year
  #
  # Args:
  #   df: dataframe containing "eventYear" column

  # Recognize four consecutive numbers as the year (and assume the last year in
  # case there are two years)
  df <-
    df %>%
    mutate(eventYear = sub(".*(\\d{4}).*", "\\1", eventYear))

  # Handling full dates

  df %>%
    mutate(eventYear = if_else(
      condition = !is.na(format(
        as.Date(eventYear, format = "%m/%d/%y"),
        "%Y"
      )),
      true = format(
        as.Date(eventYear, format = "%m/%d/%y"),
        "%Y"
      ),
      false = if_else(
        condition = !is.na(format(
          as.Date(eventYear, format = "%d/%m/%y"),
          "%Y"
        )),
        true = format(
          as.Date(eventYear, format = "%d/%m/%y"),
          "%Y"
        ),
        false = ifelse(
          test = eventYear > PublicationYear,
          yes = PublicationYear,
          no = eventYear
        )
      )
    ))
}

correct.eventDate <- function(df) {
  # Correct event year in diff formats, and assume the last year
  #
  # Args:
  #   df: dataframe containing "eventYear" and "eventDate" columns

  # Handling full dates
  df <-
    df %>%
    mutate(eventDate = if_else(
      condition = !is.na(format(
        as.Date(eventDate, format = "%m/%d/%y"),
        "%Y-%m-%d"
      )),
      true = format(
        as.Date(eventDate, format = "%m/%d/%y"),
        "%Y-%m-%d"
      ),
      false = if_else(
        condition = !is.na(format(
          as.Date(eventDate, format = "%d/%m/%y"),
          "%Y-%m-%d"
        )),
        true = format(
          as.Date(eventDate, format = "%d/%m/%y"),
          "%Y-%m-%d"
        ),
        false = if_else(
          condition = !is.na(format(
            as.Date(eventYear, format = "%m/%d/%y"),
            "%Y-%m-%d"
          )),
          true = format(
            as.Date(eventYear, format = "%m/%d/%y"),
            "%Y-%m-%d"
          ),
          false = if_else(
            condition = !is.na(format(
              as.Date(eventYear, format = "%d/%m/%y"),
              "%Y-%m-%d"
            )),
            true = format(
              as.Date(eventYear, format = "%d/%m/%y"),
              "%Y-%m-%d"
            ),
            false = eventDate
          )
        )
      )
    ))

  df$eventDate <-
    colsplit(df$eventDate, "/", c("eventDate", "later")) %>%
    mutate(eventDate = ifelse(later != "",
      later,
      eventDate
    )) %>%
    select(-later)

  a <-
    df %>%
    filter(nchar(eventDate[, ]) == 4) %>%
    mutate(eventDate = ymd(eventDate[, ], truncated = 2))

  b <-
    df %>%
    filter(nchar(eventDate[, ]) == 7 &
      nchar(word(eventDate[, ], sep = "[-]", 2)) == 2) %>%
    mutate(eventDate = ymd(eventDate[, ], truncated = 2))

  c <-
    df %>%
    filter(nchar(eventDate[, ]) == 7 &
      nchar(word(eventDate[, ], sep = "[-]", 2)) == 4) %>%
    mutate(eventDate = ymd(paste0(
      word(eventDate[, ], sep = "[-]", 2),
      "-",
      word(eventDate[, ], sep = "[-]", 1)
    ), truncated = 2))

  d <-
    df %>%
    filter(eventDate[, ] == "" |
      is.na(eventDate) | nchar(eventDate[, ]) == 1) %>%
    mutate(eventDate = ymd(eventYear, truncated = 2))

  e <-
    df %>%
    filter(nchar(eventDate[, ]) == 10) %>%
    filter(nchar(word(eventDate[, ], sep = "[-]", 1)) == 4) %>%
    mutate(eventDate = ymd(eventDate[, ], truncated = 3))

  f <-
    df %>%
    filter(nchar(eventDate[, ]) == 10) %>%
    filter(nchar(word(eventDate[, ], sep = "[-]", 3)) == 4) %>%
    mutate(eventDate = paste0(
      word(eventDate[, ], sep = "[-]", 3),
      "-",
      word(eventDate[, ], sep = "[-]", 2),
      "-",
      word(eventDate[, ], sep = "[-]", 1)
    ))

  df_p1 <-
    rbind(a, b, c, d, e, f)

  df_p2 <-
    anti_join(
      df,
      df_p1,
      by = c(
        "basisOfRecord",
        "datasetName",
        "language",
        "institutionCode",
        "collectionCode",
        "reference",
        "PublicationYear",
        "typeOfPublication",
        "occurrenceID",
        "catalogNumber",
        "recordedBy",
        "fieldNumber",
        "preparations",
        "associatedReferences",
        "eventYear",
        "country",
        "stateProvince",
        "county",
        "locality",
        "locationRemarks",
        "UC",
        "verbatimLatitude",
        "verbatimLongitude",
        "decimalLatitude",
        "decimalLongitude",
        "geodeticDatum",
        "georeferencedBy",
        "georeferencePrecision",
        "order",
        "family",
        "genus",
        "acceptedNameUsage",
        "scientificName"
      )
    ) %>%
    mutate(eventDate = ymd(paste0(
      word(eventDate[, ], sep = "[/]", 2),
      "-0",
      word(eventDate[, ], sep = "[/]", 1)
    )))

  df <- bind_rows(df_p1, df_p2)
  
  
  corrected <- 
    df %>%
    filter(format(as.Date(eventDate, format = "%Y-%m-%d"),
                  "%Y") > eventYear) %>%
    mutate(eventDate = ymd(eventYear, truncated = 2))
  
  to_bind <-
    anti_join(df, corrected, by = c(
      "basisOfRecord",
      "datasetName",
      "language",
      "institutionCode",
      "collectionCode",
      "reference",
      "PublicationYear",
      "typeOfPublication",
      "occurrenceID",
      "catalogNumber",
      "recordedBy",
      "fieldNumber",
      "preparations",
      "associatedReferences",
      "eventYear",
      "country",
      "stateProvince",
      "county",
      "locality",
      "locationRemarks",
      "UC",
      "verbatimLatitude",
      "verbatimLongitude",
      "decimalLatitude",
      "decimalLongitude",
      "geodeticDatum",
      "georeferencedBy",
      "georeferencePrecision",
      "order",
      "family",
      "genus",
      "acceptedNameUsage",
      "scientificName"
    ))
  
  bind_rows(corrected, to_bind)
}

correct.coordinates.in.geodeticDatum <- function(df) {
  # Identify coordinates in the geodeticDatum column and place it in the
  # correct column, also remove it from the wrong column
  #
  # Arg:
  #   df: dataframe containing "decimalLatitude" and "geodeticDatum" columns

  # Geographical coordinates in geodeticDatum column
  for (i in 1:nrow(df)) {
    if (is.na(df$decimalLatitude[i])) {
      df$decimalLatitude[i] <-
        as.character(df$geodeticDatum[i])
    }
  }

  # Remove from geodeticDatum
  to_remove <-
    df %>%
    select(geodeticDatum) %>%
    filter(!str_detect(geodeticDatum, "[[:alpha:] ]+")) # filtrar dados sem letras

  for (i in 1:nrow(df)) {
    if (df$geodeticDatum[i] %in% to_remove$geodeticDatum) {
      df$geodeticDatum[i] <- NA
    }
  }

  df
}

correct.mixed.latlong <- function(df) {
  # Place latitude an longitude in their respective columns
  #
  # Arg:
  #   df: dataframe containing "decimalLatitude" and "decimalLongitude" columns

  to_correct_latlong <-
    df %>%
    filter(!is.na(decimalLatitude), !is.na(decimalLongitude)) %>%
    mutate(decimalLatitude = as.numeric(decimalLatitude)) %>%
    filter(decimalLatitude < -21) %>%
    mutate(decimalLatitude = as.character(decimalLatitude))

  lon <- to_correct_latlong$decimalLatitude
  lat <- to_correct_latlong$decimalLongitude

  correct_latlong <- to_correct_latlong
  correct_latlong$decimalLongitude <- lon
  correct_latlong$decimalLatitude <- lat

  # Removing wrong rows
  df <- anti_join(df, to_correct_latlong)

  rbind(df, correct_latlong)
}

remove.row.without.coordinates <- function(df) {
  # Remove rows without adequade coordinates, or no coordinates at all
  #
  # Arg:
  #   df: dataframe containing "decimalLatitude" and "geodeticDatum" columns

  # Removing rows without adequate coordinates
  to_correct <-
    df %>%
    filter(
      str_detect(decimalLatitude, "[[:alpha:] ]+"),
      !str_detect(geodeticDatum, "UTM")
    )

  correct <-
    to_correct %>%
    mutate(decimalLatitude = "") %>%
    mutate(decimalLatitude = as.double(decimalLatitude))

  to_remove <-
    df %>%
    filter(
      is.na(decimalLatitude) | decimalLatitude == "",
      is.na(verbatimLatitude) | verbatimLatitude == ""
    )

  df <- anti_join(df, to_correct)
  df <- bind_rows(df, correct)

  anti_join(df, to_remove)
}

convert.coordinate.degree.to.latlong <- function(df) {
  # Calculate coordinate in lat/long to data only in degree
  #
  # Arg:
  #   df: dataframe containing "verbatimLatitude", "verbatimLongitude",
  #   "decimalLatitude" and "decimalLongitude" columns

  # Extract coordinates in UTM and in degrees
  degree_utm_df <-
    df %>%
    filter(
      str_detect(verbatimLatitude, "[[:alpha:] ]+"),
      decimalLongitude == "" | is.na(decimalLongitude)
    ) %>%
    select(verbatimLatitude, verbatimLongitude)

  # UTM coordinates in degree_utm_df
  utm_df <-
    degree_utm_df %>%
    filter(verbatimLatitude == "240487949 N")

  # Degree coordinates
  degree_df <-
    degree_utm_df %>%
    filter(!verbatimLatitude %in% utm_df$verbatimLatitude)

  # Preparing columns to convert to decimal degrees
  # Keep a single type of degree (º) to be easily identified
  degree_df$verbatimLatitude <-
    degree_df$verbatimLatitude %>%
    str_replace(pattern = "[°]", replacement = "º")

  degree_df$verbatimLongitude <-
    degree_df$verbatimLongitude %>%
    str_replace(pattern = "[°]", replacement = "º")

  degree_df <-
    separate(
      as.data.frame(degree_df),
      col = verbatimLatitude,
      into = c("degree_lat", "verbatimLatitude"),
      sep = "[º]"
    )

  degree_df <-
    separate(
      as.data.frame(degree_df),
      col = verbatimLongitude,
      into = c("degree_long", "verbatimLongitude"),
      sep = "[º]"
    )

  # Remove special characters
  degree_df$verbatimLongitude <-
    str_replace_all(degree_df$verbatimLongitude, "[[:alpha:] ]+", "")
  degree_df$verbatimLatitude <-
    str_replace_all(degree_df$verbatimLatitude, "[[:alpha:] ]+", "")

  # Separate minute and second
  degree_df <-
    separate(
      as.data.frame(degree_df),
      col = verbatimLatitude,
      into = c("min_lat", "seg_lat"),
      sep = "[^[:alnum:]]"
    )
  degree_df <-
    separate(
      as.data.frame(degree_df),
      col = verbatimLongitude,
      into = c("min_long", "seg_long"),
      sep = "[^[:alnum:]]"
    )

  # Remove special characters
  degree_df$degree_lat <-
    str_replace_all(degree_df$degree_lat, "[[:alpha:] ]+", "")

  degree_df$degree_long <-
    str_replace_all(degree_df$degree_long, "[[:alpha:] ]+", "")

  degree_df$seg_lat[degree_df$seg_lat == ""] <- "0"
  degree_df$seg_long[degree_df$seg_long == ""] <- "0"

  # Convert coordinate in degrees to decimal
  lat_degree_df <-
    dms2dd(
      as.numeric(degree_df$degree_lat),
      as.numeric(degree_df$min_lat),
      as.numeric(degree_df$seg_lat),
      "S"
    )
  long_degree_df <-
    dms2dd(
      as.numeric(degree_df$degree_long),
      as.numeric(degree_df$min_long),
      as.numeric(degree_df$seg_long),
      "W"
    )

  coord <- data.frame(
    lat = as.data.frame(lat_degree_df),
    long = as.data.frame(long_degree_df)
  )

  # Extracting degree and UTM coordinates to remove and later return correct
  degree_utm_df <-
    df %>%
    filter(
      str_detect(verbatimLatitude, "[[:alpha:] ]+"),
      decimalLongitude == "" | is.na(decimalLongitude)
    )

  utm_df <-
    degree_utm_df %>%
    filter(verbatimLatitude == "240487949 N")

  degree_df <-
    anti_join(degree_utm_df, utm_df)

  df <- anti_join(df, degree_df)

  degree_df$decimalLatitude <-
    as.character(coord$lat_degree_df)

  degree_df$decimalLongitude <-
    as.character(coord$long_degree_df)

  df$decimalLatitude <-
    as.character(df$decimalLatitude)

  df$decimalLongitude <-
    as.character(df$decimalLongitude)

  degree_df <-
    degree_df %>%
    select(colnames(df))

  bind_rows(df, degree_df)
}
