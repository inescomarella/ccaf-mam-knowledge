# File purpose: function to remove fossil and iNaturalist records from GBIF data
# Data: 16/11/2020
# 
#' Convert a character number with commas to a numeric
#'
#' @param x GBIF data.frame containing basisOfRecord and institutionCode columns
#'
#' @return GBIF data.frame
#' @export
#'
#' 
library(dplyr)
library(stringr)


remove_fossil_iNaturalist <- function(dataframe) {
  to_remove <-
    dataframe %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             str_detect(institutionCode, "iNaturalist"))
  df_clean <- anti_join(dataframe, to_remove)
}
