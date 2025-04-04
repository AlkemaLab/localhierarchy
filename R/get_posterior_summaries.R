



#' get_posterior_summaries
#'
#' @param fit fit object to summarize
#' @param params vector of parnames to summarize (w/o index)
#'
#' @returns tibble with variable (parname with index used in model) and posterior mean
#' @export
#'
#' @examples
get_posterior_summaries <- function(
    fit,
    params = c("mu_raw", "mu_sigma", "nonse")
) {

  # get parameter summaries for each group
  # new use of map with arguments to function
  # seq(1,2) %>% map(\(x) sum(x, 2))
  fixable_combined_summary <-
    # purrr::map(
    # fixable_params_to_collect,
    # get_posterior_summary_one_param,
    # fit = fit
    params %>%
    purrr::map(\(x) get_posterior_summary_one_param(x, fit = fit)
  ) |>
    purrr::list_rbind()%>%
    # add column wtith parname w/o []
    dplyr::mutate(
      variable_no_index = stringr::str_split_i(string = variable, pattern = fixed("["), i = 1)
    )

  return(fixable_combined_summary)
}





#' get_posterior_summary_one_param
#'
#' @param fit fit object
#' @param param_name parameter name w/o index
#'
#' need to get the estimate and/or fixed value for each indexed parameter
#' difficulty = stacking estimate on top of fixed values (for vector or array)
#' note that this does NOT get into hierarchical info (and is same for hier and nonhier param)
#' it is just the stacking

#' @returns tibble with variable (name with index) and posterior mean
#' @export
#'
#' @examples
get_posterior_summary_one_param <- function(
    fit,
    param_name) {

  # extract parameter summaries for estimated parameter
  estimate_param_name <- paste0(param_name, "_estimate")
  #print(estimate_param_name)
  if (variable_exists_in_draws(fit, estimate_param_name)) {
    param_estimate_summary <- fit$samples$summary(estimate_param_name, "mean")
  } else {
    # empty result
    param_estimate_summary <- tibble(variable = character(0), mean = numeric(0))
  }

  #extract fixed values used for estimation
  param_fixed <- fit$stan_data[[paste0(param_name, "_fixed")]]

  # if there are no fixed param values, nothing to do other than update the
  # variable name, dropping "_estimate"
  if (length(param_fixed) == 0) {
    return(
      param_estimate_summary |>
        dplyr::mutate(variable = gsub(pattern = "_estimate", replacement = "",
                                      x = variable))
    )
  }

  # what is the dimension of the array we're expecting?
  param_inf <- fit$stan_model$variables()[["parameters"]][[estimate_param_name]]
  num_inds <- param_inf[["dimensions"]]
  # check later: does `fit$stan_model$variables()` sometimes not exist?
  # if so, it's used here just to get the dimension, 1 or 2
  # so could hardcode as well
  # print("Warning: in obtaining posterior summary in get_posterior_summary_one_param, we have hardcoded num_inds based on names of parameters.")
  # num_inds = ifelse(param_name %in% c("a_raw", "a_sigma", "d_a_raw", "d_a_sigma",
  #                                              "z_a_raw", "z_a_sigma"), 2, 1)

  # process variable names in param_estimate_summary, splitting
  # into columns var_name, i, and possibly j, where i and j
  # are indices
  param_estimate_summary <- split_var_name_and_index(param_estimate_summary,
                                                     num_inds)

  # shape of param_fixed
  # if one-dimensional, length (no columns)
  # if two-dimensional, number of rows and columns
  if (num_inds == 1) {
    n_fixed <- length(param_fixed)
    m_fixed <- NA_integer_
  } else {
    n_fixed <- dim(param_fixed)[1]
    m_fixed <- dim(param_fixed)[2]
  }

  # create a matching data structure from param_fixed
  # result has columns variable (including indices), median or mean
  param_fixed_summary <- param_array_to_indexed_df(
    param_values = param_fixed,
    num_inds = num_inds,
    n = n_fixed,
    m = m_fixed,
    param_name = param_name)

  # update row index for param_estimate_summary and recombine with variable name
  param_estimate_summary <- param_estimate_summary |>
    dplyr::mutate(
      i = i + n_fixed
    )
  if (num_inds == 1) {
    param_estimate_summary <- param_estimate_summary |>
      dplyr::mutate(
        variable = paste0(param_name, "[", i, "]")
      )
  } else {
    param_estimate_summary <- param_estimate_summary |>
      dplyr::mutate(
        variable = paste0(param_name, "[", i, ",", j, "]")
      )
  }

  # combine the two summaries
  result <- dplyr::bind_rows(
    param_fixed_summary |>
      dplyr::select(variable, mean),
    param_estimate_summary |>
      dplyr::select(variable, mean)
  )

  return(result)
}
