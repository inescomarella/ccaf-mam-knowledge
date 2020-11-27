# File purpose: function to remove fossil and iNaturalist records from GBIF data
# Data: 16/11/2020
# 
#' Remove fossil and iNaturalist records
#'
#' @param dataset GBIF data containing basisOfRecord and institutionCode columns
#'
#' @return GBIF data
#' @export
#'
#' 
library(dplyr)
library(stringr)
library(conflicted)

conflict_prefer("filter", "dplyr")

remove.fossil.iNaturalist <- function(dataset) {
  to_remove <-
    dataset %>%
    filter(basisOfRecord == "FOSSIL_SPECIMEN" |
             str_detect(institutionCode, "iNaturalist"))
  dataset <- anti_join(dataset, to_remove)
  return(dataset)
}
