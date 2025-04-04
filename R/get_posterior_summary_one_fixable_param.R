
# what happens:
# get estimate and/or fixed value
# if there is a fixed value, need to use that
# difficulty = figuring out what indices to use in summary
# some messiness re mean or median



#' Title
#'
#' @param fit fit object
#' @param prefixed_param_name parameter name, can include an index
#'
#' @returns
#' @export
#'
#' @examples
get_posterior_summary_one_fixable_param <- function(
    fit,
    prefixed_param_name) {


  # extract parameter summaries for estimated parameter
  estimate_param_name <- paste0(prefixed_param_name, "_estimate")
  #print(estimate_param_name)
  if (variable_exists_in_draws(fit, estimate_param_name)) {
    param_estimate_summary <- fit$samples$summary(
      estimate_param_name,
      "mean"
      #,
      #"median"
    )
  } else {
    # empty result
    param_estimate_summary <- tibble(variable = character(0),
                                     #                                   median = numeric(0))
                                     mean = numeric(0))
  }

  #extract fixed values used for estimation
  param_fixed <- fit$stan_data[[paste0(prefixed_param_name, "_fixed")]]

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
  # update la: when reading in a run, this `fit$stan_model$variables()` does not exist
  # it's used here just to get the dimension, 1 or 2
  # easiest to check in main function, so passing here as an argument
  # # hardcoding here
  # #warning
  # print("Warning: in obtaining posterior summary in get_posterior_summary_one_fixable_param, we have hardcoded num_inds based on names of parameters.")
  # num_inds = ifelse(prefixed_param_name %in% c("a_raw", "a_sigma", "d_a_raw", "d_a_sigma",
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
    param_name = prefixed_param_name)

  # update row index for param_estimate_summary and recombine with variable name
  param_estimate_summary <- param_estimate_summary |>
    dplyr::mutate(
      i = i + n_fixed
    )
  if (num_inds == 1) {
    param_estimate_summary <- param_estimate_summary |>
      dplyr::mutate(
        variable = paste0(prefixed_param_name, "[", i, "]")
      )
  } else {
    param_estimate_summary <- param_estimate_summary |>
      dplyr::mutate(
        variable = paste0(prefixed_param_name, "[", i, ",", j, "]")
      )
  }

  # combine the two summaries
  result <- dplyr::bind_rows(
    param_fixed_summary |>
      dplyr::select(variable,
                    mean#,
                    #median
      ),
    param_estimate_summary |>
      dplyr::select(variable,
                    mean#,
                    #median
      )
  )

  return(result)
}
