
#' Hierarchical_data
#'
#' @param geo_unit_index_data tibble with unique hierarchical levels (one column for each level)
#' @param hierarchical_level vector with hierarchical spec
#'
#' @returns list with model matrix and info on start and end indices associated with different levels
#' @export
#'
#' @examples
hierarchical_data <- function(geo_unit_index_data, hierarchical_level) {
  model_matrix <- hierarchical_model_matrix(geo_unit_index_data = geo_unit_index_data,
                                            hierarchical_level = hierarchical_level)
  n_terms <- ncol(model_matrix$mat)
  re <- unique(model_matrix$assign)
  n_re <- length(re)
  re_start <- purrr::map_int(re, function(x) min(which(model_matrix$assign == x)))
  re_end   <- purrr::map_int(re, function(x) max(which(model_matrix$assign == x)))

  list(
    model_matrix = model_matrix,
    n_terms = n_terms,
    n_re = n_re,
    re_start = re_start,
    re_end = re_end
  )
}
