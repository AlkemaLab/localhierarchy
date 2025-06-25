

#' hierarchical_model_matrix
#' Create list with information including a model matrix, associated with the geo_unit_data and hierarchical_levels
#'
#' @param geo_unit_index_data tibble with unique hierarchical levels (one column for each level, one row per combi)
#' @param hierarchical_level vector with hierarchical spec from highest to lowest levels
#'
#' @returns list (modelmatrix, assign, index) associated with the geo_unit_data and hierarchical_levels
#' in modelmatrix each column refers to one eta (and nrows is the number of lowest level units).
#' assign is length of etas, with 0 for intercept, then an hierarchical level index (starting at 2) for each eta level
#' index is a tibble with n_eta rows, with info for each eta in and columns i [index of eta], column [hierarchical level], and level [name of hierarchical level]
#' @export
#'
hierarchical_model_matrix <- function(geo_unit_index_data, hierarchical_level) {
  mat <- matrix(NA, nrow = nrow(geo_unit_index_data), ncol = 0)
  assign <- c()

  index <- tibble::tibble(i = numeric(0), column = character(0), level = character(0))

  i <- 1
  if("intercept" %in% hierarchical_level) {
    mat <- cbind(mat, rep(1, nrow(mat)))
    assign <- c(assign, 0)

    index[i, ] <- tibble(i = i, column = "intercept", level = "intercept")

    i <- i + 1
  }
  # columns in model matrix are ordered by hierarchical level
  for(column in hierarchical_level) {
    for(l in levels(factor(geo_unit_index_data[[column]]))) {
      mat <- cbind(mat, as.numeric(geo_unit_index_data[[column]] == l))
      assign <- c(assign, which(column == hierarchical_level))
      index[i, ] <- tibble::tibble(i = i, column = column, level = l)

      i <- i + 1
    }
  }

  list(
    assign = assign,
    matrix = mat,
    index = index
  )
}

