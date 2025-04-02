
extract_parameter_subhierarchical_simplified <- function(
    hierarchical_data, # hierarchical_data, obtained from hierarchical_data(fit$geo_unit, fit$hierarchical_level)
    subhierarchy, # selected hierarchical_level (example: "intercept" or "region")
    parname, # selected parameter name (example: "Omega")
    fit_samples #fit$samples
) {


  start <- hierarchical_data$model_matrix$index %>%
    dplyr::filter(column == subhierarchy) %>%
    dplyr::pull(i) %>%
    min()

  end <- hierarchical_data$model_matrix$index %>%
    dplyr::filter(column == subhierarchy) %>%
    dplyr::pull(i) %>%
    max()

  pars <- c(glue::glue("{parname}_star"))

  star <- fit_samples$draws(pars) %>%
    tidybayes::spread_draws((!!sym(pars[1]))[i]) %>%
    dplyr::group_by(.chain, .iteration, .draw) %>%
    tidyr::nest() %>%
    dplyr::mutate(star = map(data, `[[`, glue::glue("{parname}_star"))) %>%
    dplyr::select(-data)

  uniq <- unique(hierarchical_data$model_matrix$mat[, 1:end, drop = FALSE])

  titles <- c()
  for(i in 1:nrow(uniq)) {
    index <- rep(0, hierarchical_data$n_terms)
    index[1:end] <- uniq[i, 1:end]
    title <- hierarchical_data$model_matrix$index %>%
      dplyr::filter(i == last(which(index == 1))) %>%
      dplyr::pull(level)
    titles <- c(titles, title)

    star[[title]] = map_dbl(star[["star"]], function(star) {
      index %*% star
    })
  }

  star <- star %>%
    tidyr::pivot_longer(cols = all_of(titles)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-star)

  return(star)
}
