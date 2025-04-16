
#' Title
#'
#' @param fit needs to include parname_raw_estimate
#' @param parname selected parameter name (example: "mu")
#' @param morethan1param does paramname refer to more than 1 parameter (a vector)
#'
#' @returns tibble with samples of parname_raw_estimate, including info on hierarchical level
#' @export
#'
#' @examples
get_mu_raw_labeled <- function(fit, parname, morethan1param = FALSE){
  # mu_raw_estimate is saved directly so getting those is not difficult
  # the only challenge is to figure out what hier level they refer to
  # here an easy approach,
  # get number of mu-raws-estimates
  # those refer to last columns of model matrix
  # so use those
  pars <- c(glue::glue("{parname}_raw_estimate"))
  if (!morethan1param){
    raw <- fit$samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i]) %>%
      dplyr::group_by(.chain, .iteration, .draw)
  } else {
    raw <- fit$samples$draws(pars) %>%
      tidybayes::spread_draws((!!sym(pars[1]))[i,k]) %>%
      dplyr::group_by(.data$k, .data$.chain, .data$.iteration, .data$.draw)
  }
  #raw
  nparams_estimated <- max(raw$i)
  # get info on all parameters, incl fixed ones
  index_all <- fit[[paste0(parname, "_data")]]$model_matrix$index
  nparam_total <- max(index_all$i)
  raw_index <-
    index_all %>%
    filter(i > (nparam_total - nparams_estimated)) %>%
    mutate(i = i - (nparam_total - nparams_estimated))
  raw_index
  raw <-
    raw %>%
    left_join(raw_index) %>%
    rename(hierarchical_level = column, hierarchical_unit = level)
  # %>%
  #   tidyr::nest() %>%
  #   dplyr::mutate(raw = map(data, `[[`, glue::glue("{parname}_raw_estimate"))) %>%
  #   dplyr::select(-data)
  raw
  return(raw)
}
