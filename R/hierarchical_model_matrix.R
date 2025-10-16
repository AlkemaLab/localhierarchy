

#' Hierarchical model matrix
#'
#' Create list with information including a model matrix, associated with the geo_unit_data and hierarchical_levels
#'
#' @param geo_unit_index_data tibble with unique hierarchical levels (one column for each level, one row per combi)
#' @param hierarchical_level vector with hierarchical spec from highest to lowest levels
#'
#' @return A list with components `modelmatrix`, `assign`, and `index`:
#'   - `modelmatrix`: A matrix where each column refers to one eta (number of rows equals number of lowest level units).
#'   - `assign`: Integer vector of length equal to the number of etas; `0` for the intercept, then a hierarchical level index (starting at 2) for each eta level.
#'   - `index`: A tibble with `n_eta` rows, and columns:
#'       - `i`: Index of eta
#'       - `column`: Hierarchical level
#'       - `level`: Name of hierarchical level
#'
#' All components are associated with the provided `geo_unit_data` and `hierarchical_levels`.
#'
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

