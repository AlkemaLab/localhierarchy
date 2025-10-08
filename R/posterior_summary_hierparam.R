

#' posterior_summary_hierparam_localhierarchy
#'
#' Calculate posterior summaries for hierarchical parameters
#'
#' @param fit needs to include parname_star
#' @param parname selected parameter name (example: "mu")
#' @param morethan1param does paramname refer to more than 1 parameter (a vector)
#' @param hierarchical_levels specifies the names of the hierarchical levels (defaults to fit$hierarchical_level)
#'
#' @returns list with summaries of mu for each hierarchical level (units with each level)
#'  these mus are obtained by summing up all relevant etas
#' for morethan1param, each level has a list where k refers to the index
#' @export
#'
posterior_summary_hierparam_localhierarchy <- function(fit, parname, morethan1param = FALSE,
                                        hierarchical_levels = fit$hierarchical_level){
  print("Calculating posterior summary for hierarchical parameters")
  print("This can take a little while")

  # initially we had a loop over levels here
  # but for each level, we were getting all the stars out
  # so here updated to first get once:
  # posterior samples of vectors of param_stars (etas)
  pars <- c(glue::glue("{parname}_star"))
  if (!morethan1param){
    star <- fit$samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i]) %>%
      dplyr::group_by(.chain, .iteration, .draw)
  } else {
    star <- fit$samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i,k]) %>%
      dplyr::group_by(.data$k, .data$.chain, .data$.iteration, .data$.draw)
  }
  star <-
    star %>%
    tidyr::nest() %>%
    dplyr::mutate(star = map(data, `[[`, glue::glue("{parname}_star"))) %>%
    dplyr::select(-data)
  star
  # star has posterior samples of vectors of param_stars (etas)

  # get total mus, obtained by summing up relevant mu_stars
  mu <- list()
  for(subhierarchy in hierarchical_levels) {
    mu[[subhierarchy]] <-
      extract_parameter_subhierarchical(
        hierarchical_data = hierarchical_data(fit$geo_unit, hierarchical_levels),
        subhierarchy = subhierarchy,
        parname = parname,
        star_samples = star,
        morethan1param = morethan1param
        )
  }
  if (!morethan1param){
    res <- map(mu, function(tibble_samples)
      tibble_samples %>% select(name, value)  %>% reframe(summary_df(value), .by = name))
  } else {
    res <- map(mu, function(tibble_samples)
      tibble_samples %>% select(name, value, k)  %>% reframe(summary_df(value), .by = c(name, k)))
  }
  return(res)
}


#' extract_parameter_subhierarchical
#'
#' @param hierarchical_data hierarchical_data, obtained from hierarchical_data(fit$geo_unit, fit$hierarchical_level)
#' @param subhierarchy selected hierarchical_level (example: "intercept" or "region")
#' @param parname selected parameter name (example: "mu")
#' @param star_samples samples from fit$samples for parname_star
#' @param morethan1param does parname refer to more than 1 parameter (a vector)
#'
#' @returns a tibble with posterior samples of the selected parameter mu at the subhierarchy level.
#' these mus are obtained by summing up all relevant etas
#' variables are `name`, `value`, `.draw/iteration/chain'
#' and `k` referring to parameter index if morethan1param = TRUE
#'
extract_parameter_subhierarchical <- function(
    hierarchical_data,
    subhierarchy,
    parname,
    star_samples,
    morethan1param = FALSE,
    add_standardizedmu = FALSE
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

  # to get the summed up mu_stars, use unique model matrix rows
  mu_total <- star_samples
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

    mu_total[[title]] = map_dbl(mu_total[["star"]], function(star) {
      mult_vector %*% star
    })
  }

  mu_total <- mu_total %>%
    tidyr::pivot_longer(cols = all_of(titles)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-star)

  return(mu_total)
}


#' Compute quantiles
#'
#' This function computes quantiles for a given vector and returns them in a tibble format.
#'
#' @param x vector with values
#' @param probs vector of quantiles
#'
#' @returns tibble with the requested quantiles
#'
quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}

#' Compute summaries
#'
#' This function computes posterior summaries for a given vector and returns them in a tibble format.
#'
#' @param x vector with values
#'
#' @returns tibble with 2.5th quantile, posterior mean, and 97.5th quantile, and quant = c(0.025, 0.5, 0.975)
#'
#'
summary_df <- function(x) {
  tibble(
    val = c(quantile(x, 0.025, na.rm = TRUE),
            mean(x, na.rm = TRUE),
            quantile(x, 0.975, na.rm = TRUE)),
    quant = c(0.025, 0.5, 0.975)
  )
}
