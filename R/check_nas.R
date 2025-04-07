check_nas <- function(data, column) {
  if(!(column %in% names(data))) {
    stop(glue::glue("Could not find column {column}."))
  }
  if(sum(is.na(data[[column]]))) {
    stop(glue::glue("There are NAs in column {column}."))
  }
}

