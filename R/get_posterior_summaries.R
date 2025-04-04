
# to do
# consider udpate to prefixes, process vs dm set up etc...
# different way to collect parameter names to get fixable_params_to_collect

# gets vector of parnames to fix
# calls get_posterior_summary_one_fixable_param for each of them
# note that parameter has NO index with it...


#' get_posterior_summaries_simplified
#'
#' @param fit # fit object to summarize
#' @param process_indicator_prefixes # prefixes for parameters from process model
#' @param dm_indicator_prefixes
#' @param process_params
#' @param dm_params
#'
#' @returns
#' @export
#'
#' @examples
get_posterior_summaries_simplified <- function(
    fit,
    process_indicator_prefixes = c(""),
    dm_indicator_prefixes = c(""),
    process_params = c("mu_raw", "mu_sigma"),
    dm_params = c("nonse")
) {

  ## part to get the names of parameters to fix
  params_to_collect <- dplyr::bind_rows(
    expand.grid(
      prefix = process_indicator_prefixes,
      param = process_params),
    expand.grid(
      prefix = dm_indicator_prefixes,
      param = dm_params)
  ) |>
    dplyr::mutate(prefixed_param_name = paste0(prefix, param))

  # split parameters into those that are "fixable" in local fits and those that
  # are "unfixable"
  #  unfixable_param_names <- c("local_shrinkage_dm")
  # unfixable_param_names <- c("BLA")
  fixable_params_to_collect <- params_to_collect |>
    #   dplyr::filter(!param %in% unfixable_param_names) |>
    dplyr::pull(prefixed_param_name)

  #####

  # unfixable_params_to_collect <- params_to_collect |>
  #   dplyr::filter(param %in% unfixable_param_names) |>
  #   dplyr::pull(prefixed_param_name)

  # get parameter summaries for each group
  # get parameter summaries for each group
  print(
    "Warning: in obtaining posterior summary in get_posterior_summary_one_fixable_param, we have hardcoded num_inds based on names of parameters."
  )
  # new use of map with arguments to function
  # seq(1,2) %>% map(\(x) sum(x, 2))
  fixable_combined_summary <-
    # purrr::map(
    # fixable_params_to_collect,
    # get_posterior_summary_one_fixable_param,
    # fit = fit
    fixable_params_to_collect %>%
    purrr::map(\(x) get_posterior_summary_one_fixable_param(x, fit = fit)
  ) |>
    purrr::list_rbind()%>%
    # add column wtith parname w/o []
    dplyr::mutate(
      variable_no_index = stringr::str_split_i(string = variable, pattern = fixed("["), i = 1)
    )

  return(fixable_combined_summary)
}
