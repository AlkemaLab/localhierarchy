
#' Set up data for Stan related to hierarchical parameters
#'
#' @param global_fit an optional "global" fit that will be used to extract
#'   parameter estimates for any specified hierarchical units to fix
#' @param param_name The name of the parameter we are working with:
#'   "P_tilde", "Omega", or "a"
#' @param param_data Data structures as constructed by hierarchical_data
#' @param hierarchical_terms_fixed character vector specifying hierarchical
#'   levels for which the terms should be fixed
#' @param hierarchical_terms_fixed character vector specifying hierarchical
#'   levels for which the terms should be fixed
#'
#' @return named list with Stan data relevant to the hierarchical set up for
#'   this parameter, e.g. if the `param_name` is `"Omega"`, these will be
#'   `Omega_n_terms, Omega_n_sigma, Omega_re_start, Omega_re_end,
#'   Omega_model_matrix, Omega_n_terms_fixed, Omega_n_terms_estimate,
#'   Omega_raw_fixed, Omega_n_sigma_fixed, Omega_n_sigma_estimate,
#'   Omega_sigma_fixed`
hierarchical_param_stan_data <- function(param_name, param_data,
                                         hierarchical_terms_fixed,
                                         hierarchical_sigmas_fixed,
                                         global_fit) {
  result <- list()
  result[[paste0(param_name, "_n_terms")]] <- param_data$n_terms
  result[[paste0(param_name, "_n_sigma")]] <- param_data$n_re
  result[[paste0(param_name, "_re_start")]] <- array(param_data$re_start)
  result[[paste0(param_name, "_re_end")]] <- array(param_data$re_end)
  result[[paste0(param_name, "_model_matrix")]] <- param_data$model_matrix$mat


  # get nfixed etc, no estimates yet
  if (is.null(global_fit)) {
    result[[paste0(param_name, "_n_terms_fixed")]] <- 0
    result[[paste0(param_name, "_n_terms_estimate")]] <- param_data$n_terms
    result[[paste0(param_name, "_raw_fixed")]] <- numeric(0)

    result[[paste0(param_name, "_n_sigma_fixed")]] <- 0
    result[[paste0(param_name, "_n_sigma_estimate")]] <- param_data$n_re - 1
    result[[paste0(param_name, "_sigma_fixed")]] <- numeric(0)
    return(result)
  } else {

    # extract relevant data
    globalfit_param_data = global_fit[[paste0(param_name, "_data")]]
    globalfit_param_post_summ_raw = global_fit$post_summ %>%
      filter(variable_no_index == paste0(param_name, "_raw"))
    globalfit_param_post_summ_sigma = global_fit$post_summ %>%
      filter(variable_no_index == paste0(param_name, "_sigma"))

    # number of fixed terms: number of terms for spatial units in
    # fixed hierarchical levels where that spatial unit was in the global fit
    n_terms_fixed <- purrr::map2_int(
      param_data$model_matrix$index$column,
      param_data$model_matrix$index$level,
      function(column, level) {
        if (!column %in% hierarchical_terms_fixed) return(0L)

        # in the global fit's model matrix index, keep rows matching the
        # specified column and level
        global_index_subset <- globalfit_param_data$model_matrix$index |>
          dplyr::filter(column == !!column, level == !!level)
        if (nrow(global_index_subset) > 1) {
          # this will never happen
          stop("unexpected match to multiple global geographic units")
        } else {
          # 0 or 1 matches
          return(nrow(global_index_subset))
        }
      }) |>
      sum()
    result[[paste0(param_name, "_n_terms_fixed")]] <- n_terms_fixed
    # number of terms to estimate: total number of terms minus number fixed
    result[[paste0(param_name, "_n_terms_estimate")]] <-
      param_data$n_terms - n_terms_fixed
    # number of sigmas to fix.
    # Note that the intercept is always fixed, and is handled separately
    n_sigma_fixed <- length(
      hierarchical_sigmas_fixed[hierarchical_sigmas_fixed != "intercept"])
    result[[paste0(param_name, "_n_sigma_fixed")]] <- n_sigma_fixed
    # Note that the intercept is always fixed,
    # and is handled separately, hence subtract 1
    result[[paste0(param_name, "_n_sigma_estimate")]] <- param_data$n_re - n_sigma_fixed - 1
    # get indices from the vector of fixed parameter values (Ptilde, Omega, or one spline coefficient)
    # simple coding to avoid errors....
    # print("uses new set up")
    index_global <- globalfit_param_data$model_matrix$index
    index_local <- param_data$model_matrix$index[1:n_terms_fixed,]
    indices_for_local <- rep(NA, length(index_local$column))
    for (k in 1:length(index_local$column)){
      indices_for_local[k]  <-
        index_global %>%
        filter(column == index_local$column[k] & level == index_local$level[k]) %>%
        pull(i)
    }

    ### add estimates of the fixed terms, based on global fit
    is_matrix <- is_there_a_comma(globalfit_param_post_summ_raw$variable[1])
    if (!is_matrix){
      result[[paste0(param_name, "_raw_fixed")]] <- globalfit_param_post_summ_raw$median[indices_for_local]
      result[[paste0(param_name, "_sigma_fixed")]] <- globalfit_param_post_summ_sigma$median[seq_len(n_sigma_fixed)]
    } else {
      result[[paste0(param_name, "_raw_fixed")]] <- create_a_matrix(globalfit_param_post_summ_raw[, c("variable", "median")])[indices_for_local, , drop = FALSE]
      result[[paste0(param_name, "_sigma_fixed")]] <- create_a_matrix(globalfit_param_post_summ_sigma[, c("variable", "median")])[seq_len(n_sigma_fixed), , drop=FALSE]
    }
    return(result)
  }
}


