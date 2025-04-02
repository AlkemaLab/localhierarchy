#' @import purrr
hierarchical_data <- function(data, hierarchy) {
  model_matrix <- hierarchical_model_matrix(hierarchy, data)
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
