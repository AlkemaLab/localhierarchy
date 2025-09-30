
#' Check for missing values
#'
#' This function checks if there are any NAs in a specified column of a data frame.
#'
#' @param data tibble or data frame
#' @param column column to check for NAs
#'
#' @returns stops if there are NAs in the column or if the column does not exist
#'
check_nas <- function(data, column) {
  if(!(column %in% names(data))) {
    stop(glue::glue("Could not find column {column}."))
  }
  if(sum(is.na(data[[column]]))) {
    stop(glue::glue("There are NAs in column {column}."))
  }
}

