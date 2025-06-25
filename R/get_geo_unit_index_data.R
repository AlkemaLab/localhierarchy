

#' Get geo_unit_index_data
#'
#' Get unique hierarchical levels from survey data and assign index "c" based on area.
#'
#' @param data survey data of interest that contains the hierarchical_column_names
#' @param hierarchical_levels vector that contains one of more specs of hierarchical_level (incl intercept)
#' @param area unit of analysis (eg country or subnational region)
#'
#' @returns tibble with unique hierarchical levels (one column for each level) and index "c" assigned based on area
#' @export
#'
#' @examples
#' survey_dat <- tibble(subcluster = c("A", "A", "B", "B"),
#'     iso = c("iso1", "iso2", "iso3", "iso4"))
#' get_geo_unit_index_data(survey_dat,
#'         hierarchical_levels = c("intercept", "subcluster", "iso"),
#'         area = "iso")
get_geo_unit_index_data <- function(data,
                                    hierarchical_levels,
                                    area){
  hierarchical_column_names <- unique(hierarchical_levels) %>%
    setdiff("intercept")
  if (!all(hierarchical_column_names %in% names(data))) {
    stop("data does not contain all hierarchical levels")
  }
  data[!is.na(data[[area]]), ] %>%
    dplyr::distinct(!!! syms(hierarchical_column_names)) %>%
    dplyr::mutate(c = 1:n())
}

