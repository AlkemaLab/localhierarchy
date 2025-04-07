
#' Set up data for Stan related to hierarchical parameters
#'
#' @param param_name The name of the parameter we are working with, e.g. "mu"
#' @param param_data Data structures as constructed by hierarchical_data for that parameter
#' @param global_fit an optional "global" fit that will be used to extract
#'   parameter estimates for any specified hierarchical units to fix
#' @param hierarchical_terms_fixed character vector specifying hierarchical
#'   levels for which the terms should be fixed (subset of `hierarchical_levels`)
#' @param hierarchical_terms_fixed character vector specifying hierarchical
#'   levels for which the terms should be fixed (subset of `hierarchical_levels`)
#'
#' @return named list with Stan data relevant to the hierarchical set up for
#'   this parameter, e.g. if the `param_name` is `"mu"`, these will be
#'   `mu_n_terms, mu_n_sigma, mu_re_start, mu_re_end,
#'   mu_model_matrix, mu_n_terms_fixed, mu_n_terms_estimate,
#'   mu_raw_fixed, mu_n_sigma_fixed, mu_n_sigma_estimate,
#'   mu_sigma_fixed`
hierarchical_param_stan_data <- function(param_name, param_data,
                                         hierarchical_terms_fixed,
                                         hierarchical_sigmas_fixed,
                                         global_fit) {
  result <- list()
  result[[paste0(param_name, "_raw_n_terms")]] <- param_data$n_terms
  result[[paste0(param_name, "_re_start")]] <- array(param_data$re_start)
  result[[paste0(param_name, "_re_end")]] <- array(param_data$re_end)
  result[[paste0(param_name, "_model_matrix")]] <- param_data$model_matrix$mat

  # update 4/2/2025: nsigma now refers to the number of sigmas, prior sd is NOT added
  n_sigma <- param_data$n_re - 1
  result[[paste0(param_name, "_n_sigma")]] <- n_sigma

  # get nfixed etc, no estimates yet
  if (is.null(global_fit)) {
    result[[paste0(param_name, "_raw_n_terms_fixed")]] <- 0
    result[[paste0(param_name, "_raw_n_terms_estimate")]] <- param_data$n_terms
    result[[paste0(param_name, "_raw_fixed")]] <- numeric(0)

    result[[paste0(param_name, "_n_sigma_fixed")]] <- 0
    result[[paste0(param_name, "_n_sigma_estimate")]] <- n_sigma
    result[[paste0(param_name, "_sigma_fixed")]] <- numeric(0)
    return(result)
  } else {


    # extract relevant data
    globalfit_param_data = global_fit[[paste0(param_name, "_data")]]
    # later on, these are used in order in current version
    # WIP: use the indices as stored in the summary tibble
    globalfit_param_post_summ_raw = global_fit$post_summ %>%
      filter(variable_no_index == paste0(param_name, "_raw"))
    #globalfit_param_post_summ_sigma = global_fit$post_summ %>%
    #  filter(variable_no_index == paste0(param_name, "_sigma"))

    # get number of fixed terms (etas): number of etas in
    # fixed hierarchical levels where that eta was in the global fit
    # this code checks whether there are matches for all requested etas
    # this code is the same for 1par or parvector
    n_terms_fixed <- purrr::map2_int(
      param_data$model_matrix$index$column,
      param_data$model_matrix$index$level,
      function(column, level) {
        if (!column %in% hierarchical_terms_fixed)
          return(0L)
        # in the global fit's model matrix index, keep rows matching the
        # specified column and level
        global_index_subset <- globalfit_param_data$model_matrix$index |>
          dplyr::filter(column == !!column, level == !!level)
        if (nrow(global_index_subset) == 1) {
          return(nrow(global_index_subset))
        } else {
          # 0 or multiple matches
          stop("We can't fix an eta, 0 (or more than 1) match")
        }
      }) |>
      sum()
    result[[paste0(param_name, "_raw_n_terms_fixed")]] <- n_terms_fixed
    # number of terms to estimate: total number of terms minus number fixed
    result[[paste0(param_name, "_raw_n_terms_estimate")]] <-
      param_data$n_terms - n_terms_fixed



    # get indices in global for local vector of fixed parameter values
    # we assume that in the local model, the fixed etas are before the to-be-estimated ones
    # when sticking to an ordering of levels from higher order to lower order
    # and defining fixed hierarchical levels only for higher order levels, this is correct
    index_local <- param_data$model_matrix$index[1:n_terms_fixed,]
    index_global <- globalfit_param_data$model_matrix$index
    indices_for_local <- rep(NA, length(index_local$column))
    for (k in 1:length(index_local$column)){
      indices_for_local[k]  <-
        index_global %>%
        filter(column == index_local$column[k] & level == index_local$level[k]) %>%
        pull(i)
     }

    # number of sigmas to fix.
    # intercept is not included
    n_sigma_fixed <- length(
      hierarchical_sigmas_fixed[hierarchical_sigmas_fixed != "intercept"])
    result[[paste0(param_name, "_n_sigma_fixed")]] <- n_sigma_fixed
    result[[paste0(param_name, "_n_sigma_estimate")]] <- n_sigma - n_sigma_fixed

    # now plug in the values
    is_matrix <- is_there_a_comma(globalfit_param_post_summ_raw$variable[1])

    if (!is_matrix){
      results_sigma <- rep(NA, n_sigma_fixed)
      # here index k used for etas [not dimension of parvector]
      for (k in 1:n_sigma_fixed){
        results_sigma[k] <- global_fit$post_summ %>%
          filter(variable == paste0(param_name, "_sigma[", k, "]")) %>%
          pull(postmean)
      }
      results_eta <- rep(NA, length(index_local$column))
      for (k in 1:length(index_local$column)){
        results_eta[k] <- global_fit$post_summ %>%
          filter(variable == paste0(param_name, "_raw[", indices_for_local[k], "]")) %>%
          pull(postmean)
      }
      result[[paste0(param_name, "_raw_fixed")]] <- results_eta # globalfit_param_post_summ_raw$postmean[indices_for_local]
      result[[paste0(param_name, "_sigma_fixed")]] <- results_sigma # globalfit_param_post_summ_sigma$postmean[seq_len(n_sigma_fixed)]
    } else {
      # using old set up here
      print("In hierarchical_param_stan_data, consider further testing/rewriting of set up for multiple parameters")
      # alternatively: just loop over the 2nd index of the parameter!
      # extract relevant data
      globalfit_param_data = global_fit[[paste0(param_name, "_data")]]
      globalfit_param_post_summ_raw = global_fit$post_summ %>%
        filter(variable_no_index == paste0(param_name, "_raw"))
      globalfit_param_post_summ_sigma = global_fit$post_summ %>%
        filter(variable_no_index == paste0(param_name, "_sigma"))
      result[[paste0(param_name, "_raw_fixed")]] <- create_a_matrix(globalfit_param_post_summ_raw[, c("variable", "postmean")])[indices_for_local, , drop = FALSE]
      result[[paste0(param_name, "_sigma_fixed")]] <- create_a_matrix(globalfit_param_post_summ_sigma[, c("variable", "postmean")])[seq_len(n_sigma_fixed), , drop=FALSE]
    }
    print("We are fixing the following parameters:")
    print(result[[paste0(param_name, "_raw_fixed")]])
    print(result[[paste0(param_name, "_sigma_fixed")]])
    return(result)
  }
}


