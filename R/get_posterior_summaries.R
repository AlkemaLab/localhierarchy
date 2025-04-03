get_posterior_summaries_simplified <- function(
    fit,
    process_indicator_prefixes = c(""),
    dm_indicator_prefixes = c(""),
    process_params = c("mu_raw", "mu_sigma"),
    dm_params = c("nonse")
) {
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

  # unfixable_params_to_collect <- params_to_collect |>
  #   dplyr::filter(param %in% unfixable_param_names) |>
  #   dplyr::pull(prefixed_param_name)

  # get parameter summaries for each group
  # get parameter summaries for each group
  print(
    "Warning: in obtaining posterior summary in get_posterior_summary_one_fixable_param, we have hardcoded num_inds based on names of parameters."
  )
  fixable_combined_summary <- purrr::map(
    fixable_params_to_collect,
    get_posterior_summary_one_fixable_param,
    fit = fit
  ) |>
    purrr::list_rbind()%>%
    # LA added column wtith parname w/o []
    dplyr::mutate(
      variable_no_index = stringr::str_split_i(string = variable, pattern = fixed("["), i = 1)
    )

  # unfixable_combined_summary <- fit$samples$summary(unfixable_params_to_collect,
  #                                                   "mean"#,
  #                                                   #"median"
  #                                                   )

  return(fixable_combined_summary)
  # return(dplyr::bind_rows(
  #   fixable_combined_summary,
  #   unfixable_combined_summary
  #     ) %>%
  #     # LA added column wtith parname w/o []
  #   dplyr::mutate(
  #     variable_no_index = stringr::str_split_i(string = variable, pattern = fixed("["), i = 1)
  #   )
  # )
}
