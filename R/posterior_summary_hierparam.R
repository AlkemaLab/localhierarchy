

#' posterior_summary_hierparam
#'
#' @param fit needs to include parname_star
#' @param parname selected parameter name (example: "mu")
#' @param morethan1param does paramname refer to more than 1 parameter (a vector)
#'
#' @returns list with summaries of mu for each hierchical level (units with each level)
#'  these mus are obtained by summing up all relevant etas
#' for morethan1param, k refers to the index
#' @export
#'
#' @examples
posterior_summary_hierparam <- function(fit, parname, morethan1param = FALSE){
  mu <- list()
  for(subhierarchy in fit$hierarchical_level) {
    mu[[subhierarchy]] <-
      extract_parameter_subhierarchical(
        hierarchical_data = hierarchical_data(fit$geo_unit, fit$hierarchical_level),
        subhierarchy = subhierarchy,
        parname = parname,
        fit_samples = fit$samples,
        morethan1param = morethan1param
        )
  }
  if (!morethan1param){
    res <- map(mu, function(tibble_samples)
      tibble_samples %>% select(name, value)  %>% reframe(quantile_df(value), .by = name))
  } else {
    res <- map(mu, function(tibble_samples)
      tibble_samples %>% select(name, value, k)  %>% reframe(quantile_df(value), .by = c(name, k)))
  }
  return(res)
}


#' extract_parameter_subhierarchical
#'
#' @param hierarchical_data hierarchical_data, obtained from hierarchical_data(fit$geo_unit, fit$hierarchical_level)
#' @param subhierarchy selected hierarchical_level (example: "intercept" or "region")
#' @param parname selected parameter name (example: "mu")
#' @param fit_samples fit$samples including parname_star
#' @param morethan1param does parname refer to more than 1 parameter (a vector)
#'
#' @returns a tibble with posterior samples of the selected parameter mu at the subhierarchy level.
#' these mus are obtained by summing up all relevant etas
#' variables are `name`, `value`, `.draw/iteration/chain'
#' and `k` referring to parameter index if morethan1param = TRUE
#' @export
#'
#' @examples
extract_parameter_subhierarchical <- function(
    hierarchical_data,
    subhierarchy,
    parname,
    fit_samples,
    morethan1param = FALSE

) {

  # not currently used
  # start <- hierarchical_data$model_matrix$index %>%
  #   dplyr::filter(column == subhierarchy) %>%
  #   dplyr::pull(i) %>%
  #   min()

  # end is used to determine the number of columns in model matrix to be used
  end <- hierarchical_data$model_matrix$index %>%
    dplyr::filter(column == subhierarchy) %>%
    dplyr::pull(i) %>%
    max()

  # get the param_stars
  pars <- c(glue::glue("{parname}_star"))
  if (!morethan1param){
    star <- fit_samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i]) %>%
      dplyr::group_by(.chain, .iteration, .draw)
  } else {
    star <- fit_samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i,k]) %>%
      dplyr::group_by(.data$k, .data$.chain, .data$.iteration, .data$.draw)
  }
  star <-
    star %>%
    tidyr::nest() %>%
    dplyr::mutate(star = map(data, `[[`, glue::glue("{parname}_star"))) %>%
    dplyr::select(-data)
  # star has posterior samples of vectors of param_stars (etas)

  # to get the summed up mu_stars, use unique model matrix rows
  uniq <- unique(hierarchical_data$model_matrix$mat[, 1:end, drop = FALSE])
  titles <- c()
  for(i in 1:nrow(uniq)) {
    # here mult_vector refers to a vector with 0s after end
    # and the row of the model matrix before end
    mult_vector <- rep(0, hierarchical_data$n_terms)
    mult_vector[1:end] <- uniq[i, 1:end]

    # title refers to name of unit in the hierarchical level
    title <- hierarchical_data$model_matrix$index %>%
      dplyr::filter(i == last(which(mult_vector == 1))) %>%
      dplyr::pull(level)
    titles <- c(titles, title)

    star[[title]] = map_dbl(star[["star"]], function(star) {
      mult_vector %*% star
    })
  }

  star <- star %>%
    tidyr::pivot_longer(cols = all_of(titles)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-star)

  return(star)
}


quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}
