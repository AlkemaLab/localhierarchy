
# functions for simplified model fitting


#' Simplified fitting function
#'
#' @param survey_df tibble with survey data
#' @param y column name of outcome.
#' @param area column name of the area of each observation
#'
#' @param runstep type of run, currently one of "step1a", "step1b", "local_national" (see Details).
#' @param global_fit optional object of class `"fpemplus"`, used to obtain fixed
#'   values to use for some parameters in the current fit (see Details).
#' @param iso_select ISO code to use for local national run
#'
#' @param hierarchical_level vector specifying hierarchical structure used for mu
#'
#' Settings for sampling
#' @param chains number of chains to run
#' @param iter_sampling number of posterior samples to draw
#' @param iter_warmup number of warmup iterations
#' @param compile_model boolean indicator of whether to compile the Stan model
#' @param force_recompile boolean indicator of whether to force recompilation of the Stan model
#' @param seed random seed
#' @param refresh number of iterations between progress updates
#' @param adapt_delta target acceptance rate for the No-U-Turn Sampler
#' @param max_treedepth maximum tree depth for the No-U-Turn Sampler
#'
#' @return
#'
#' @details
#' The `fit_model_simplified` function fits the toy example for hierarchical models/seq fitting.
#' The argument \code{runstep} determines the type of run to perform. The
#' following run steps are supported:
#' - "step1a": Fit the global model
#' - "local_national": Fit the model to data from a single country, using a 1a fit.
#' This is also explained in the documentation folder.
#'
#' Details on hierarchical set ups used
#' Several area-specific parameters of the fpemplus model have hierarchical priors
#' assigned to them so that information can be shared between areas.
#' The package allows the structure of the hierarchical prior to be configured by the user
#' through the  \code{hierarchical_level} argument.
#' These arguments expect a character vector that specifies a nesting hierarchical structure.
#' Each element of the vector must be either "intercept" or a column name in the dataset, where
#' "intercept" will add a global intercept for the parameter.
#' The vector must be in descending order in terms of the hierarchy: that is, it starts with
#' "intercept" and proceeds down the hierarchy.
#'
#' For example, suppose we are fitting country-level data, where the dataset has columns
#' "name_country", "name_sub_region", and "name_region" containing the name of the country,
#' sub-region, and region that each observation belongs to. To specify that the spline coefficients
#' should be fitted with a hierarchical model in which countries are nested within sub-regions within regions within world,
#' we would use the argument
#' \code{hierarchical_splines = c("intercept", "name_region", "name_sub_region", "name_country")}.
#'
#' Optionally, model parameters can be fixed to values from a previous model fit
#' provided via the \code{global_fit} argument. In a typical use case, the
#' \code{global_fit} will have been fit to data from many geographic units
#' (e.g., all countries), while the current fit uses data from a smaller number
#' of locations.
#' Note: remainder of details is currently determined by the runstep
#' To use a global fit to fix parameter values, a number of settings must be the
#' same in the global fit and the current call to `fpemplus`:
#' - For any of the hierarchical settings, e.g.
#'   \code{hierarchical_asymptote}, all of the hierarchical levels
#'   used for that parameter in the global fit must also be used in the current
#'   fit. For instance, if the `global_fit` used
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")},
#'   then in the current fit it is valid to use
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")} again or
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country", "name_subnational")},
#'   but it is not valid to use
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_subnational")}.
#' - All hierarchical levels for terms and sigmas to fix in the current fit are
#'   contained in the levels in the hierarchy used for the corresponding
#'   parameters in the global fit. For example, if
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region", "name_country")}
#'   then the global fit must have included at least "intercept", "name_region",
#'   and "name_country" for \code{hierarchical_asymptote}.
#' - Any hierarchical levels to fix in the current fit are at the highest levels
#'   of the hierarchical structure. For example, if
#'   \code{hierarchical_asymptote = c("intercept", "name_region", "name_country")},
#'   then it is valid to use
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_region")},
#'   but it is not valid to use
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept", "name_country")}.
#' - It is not valid to fix terms at a given hierarchy level without also fixing
#'   the sigma estimate at that hierarchy level. For example, we cannot specify
#'   \code{hierarchical_asymptote_sigmas_fixed = c("intercept")} and
#'   \code{hierarchical_asymptote_terms_fixed = c("intercept", "name_region")}
#' - It is only valid to set `fix_smoothing = TRUE` if also `smoothing = TRUE`.
#' - All settings for the arguments `model`, `t_star`, `smoothing`, `tau_prior`,
#'   and `rho_prior` must be the same in the `global_fit` and the current fit.
#' - If `hierarchical_splines_sigmas_fixed` or
#'   `hierarchical_splines_terms_fixed` include any hierarchical levels (i.e.,
#'   if either is different from the empty vector `c()`), all settings for
#'   `num_knots` and `spline_degree` must be the same in the `global_fit` and
#'   the current fit.
#' - All geographic units that appear in the `data` for the current fit at
#'   hierarchical levels for which any parameter is fixed must have also been
#'   included in the `data` used for the `global_fit`.
#' - If `fix_nonse = TRUE`, all data `source`s that appear in the `data` for the
#'   current fit must also have been included in the `data` used for the
#'   `global_fit`.
#'
#' @importFrom cmdstanr cmdstan_model write_stan_file
#' @importFrom tibble tibble
#' @importFrom splines bs
#' @import dplyr
#' @importFrom readr read_file
#' @importFrom stringr str_replace_all
#'
#' @export
#'
#'
#'
fit_model_simplified <- function(
  survey_df,
  y = "logit_indicator",
  area = "iso",
  iso_select  = NULL, # used for local national run

  # type of run is defined by runstep:
  runstep, # type of run, step or localnat or localsubnat
  # step1a =  global fit
  # local_national =  get local results only
  global_fit = NULL, # eventually, read in from data_raw if needed but NULL

  hierarchical_level     = c("intercept", "subcluster", "iso"),

  # settings for sampling
  chains = 4, # probably need more for final model
  iter_sampling = 200,
  iter_warmup = 150,
  compile_model  = TRUE, force_recompile = FALSE,
  seed = 1234,
  refresh = 200,
  adapt_delta = 0.9,
  max_treedepth = 14
) {

  data <- survey_df # for now, just to keep the same name as in fpem
  if (!runstep %in% c("step1a", "local_national")){
    stop("runstep not yet implemented!")
  }
  if (runstep %in% c("step1a")){
    # global fit
    print("We do a global fit.")
    print("We don't fix anything")
    hierarchical_level_sigmas_fixed = c()
    hierarchical_level_terms_fixed = c()
    fix_nonse = FALSE
  } else {
    if (is.null(global_fit)){
      # to do: finish this once we decide what to save where (and whether to use indicator names too)
      stop("Need a global fit for this run.")
      # globalstepname <- dplyr::case_when(
      #   runstep == "step1b" ~ "1a",
      #   runstep == "local_national" ~ "1b",
      #   runstep == "step3" ~ "1b",
      #   TRUE ~ "3"
      # )
      # global_fit <- readRDS(file = paste0(
      #   here::here("data-raw/internal/"), "fit",
      #   globalstepname,
      #   ".rds"))
    }

    print("We use a global fit, and take selected or all settings from there.")
    # minor to do: print these settings?
    print("Settings for hierarchical settings")
    hierarchical_level <- global_fit$hierarchical_level
    # for hier stuff, we fix things according to the run
    if (runstep %in% c("local_national")) {
      print("For hierarchical terms, we fix things up to the 2nd-lowest level.")
      hierarchical_level_terms_fixed = hierarchical_level[1:(length(hierarchical_level)-1)]
      print("For sigma terms, we fix up to lowest level.")
      hierarchical_level_sigmas_fixed = hierarchical_level[1:(length(hierarchical_level))]
      print("We fix data model parameters.")
      fix_nonse <- TRUE
    }
  }

  ##### Data processing  #####
  if(length(hierarchical_level) == 0) {
    stop("No hierarchical structure supplied for the level in reference year. See the hierarchical_level argument.")
  }
  # if a global fit was provided, check for consistency between
  # settings for the global fit and settings for the current fit
  # note: less relevant now that we take settings from global fit
  if (!is.null(global_fit)) {
    # check that levels in the hierarchies used in the global fit are all
    # contained in the levels in the hierarchies used in this fit, and the
    # first entries of the hierarchy to use for this fit match the hierarchical
    # levels used for the global fit.
    if (!all(global_fit$hierarchical_level %in% hierarchical_level)) {
      stop("hierarchical_level must contain all levels that were used for the global fit.")
    }
    if (!all(global_fit$hierarchical_level == hierarchical_level[seq_along(global_fit$hierarchical_level)])) {
      stop("The top levels of hierarchical_level must match the levels that were used for the global fit.")
    }
    # check that hierarchical terms and sigmas to fix are all contained at the
    # top of the levels in the hierarchy used in the global fit
    # to do: need to update checks...
    #
    # if (#!all(hierarchical_level_sigmas_fixed %in% global_fit$hierarchical_level) ||
    #     !all(hierarchical_level_terms_fixed %in% global_fit$hierarchical_level)) {
    #   stop("Hierarchical estimates to fix for hierarchical_level were not estimated in the global fit.")
    # }
    # if (#!all(hierarchical_level_sigmas_fixed == global_fit$hierarchical_level[seq_along(hierarchical_level_sigmas_fixed)]) ||
    #     !all(hierarchical_level_terms_fixed == global_fit$hierarchical_level[seq_along(hierarchical_level_terms_fixed)])) {
    #   stop("Hierarchical estimates to fix for hierarchical_level do not match the highest hierarchical_level levels estimated in the global fit.")
    # }
    #
    # It is not valid to fix terms at a given hierarchy level without also fixing
    # the sigma estimate at that hierarchy level.
    # For example, if x = mu + z * sigma,
    # it does not make sense to fix z without also fixing sigma.
    # on the other hand, we might fix sigma but not z if we want to borrow information
    # about variability from global fit, but the global fit didn't produce an estimate
    # of z for the geo unit we're interested in, or we are ok with re-estimating it?
    if (!all(hierarchical_level_terms_fixed %in% hierarchical_level_sigmas_fixed)) {
      stop("All values of hierarchical_level_terms_fixed must also be contained in hierarchical_level_sigmas_fixed")
    }
  }



  # Create district index for matching district and district index
  hierarchical_column_names <- unique(c(
    hierarchical_level
  )) %>%
    setdiff("intercept")

  # Make sure there are no NAs in any of the columns
  for(column in hierarchical_column_names) {
    if(column == "intercept") next
    if (column == area) {
      check_nas_or_pops(data, column, year, population_data)
    } else {
      check_nas(data, column)
    }
  }


  ##### Load model #####
  if (compile_model){
    stan_file_path <- file.path(here::here("documentation/code_for_hierarchicalmodels_seqrun", "hierfunctions_seq.stan"))
    stan_model <- cmdstanr::cmdstan_model(
      stan_file = stan_file_path,
      force_recompile = force_recompile
    )
  }

  ##### Setup data for Stan #####

  geo_unit_index <- data[!is.na(data[[area]]), ] %>%
    dplyr::distinct(!!! syms(hierarchical_column_names)) %>%
    dplyr::mutate(c = 1:n())

  data <- data %>%
    dplyr::left_join(geo_unit_index, by = hierarchical_column_names)

  # In "local" fits, ensure that for all hierarchy levels where any quantity is
  # fixed, all geographic units in data used for the local fit also were present
  #  in the global fit
  # LA doesn't think this check is needed? also, gives error when just running with global fit but without fixing any hierarchies
  # if (!is.null(global_fit)) {
  #   fixed_hierarchy_levels <- unique(c(hierarchical_asymptote_sigmas_fixed,
  #                                      hierarchical_asymptote_terms_fixed,
  #                                      hierarchical_splines_sigmas_fixed,
  #                                      hierarchical_splines_terms_fixed,
  #                                      hierarchical_level_sigmas_fixed,
  #                                      hierarchical_level_terms_fixed))
  #   fixed_hierarchy_levels <- fixed_hierarchy_levels[fixed_hierarchy_levels != "intercept"]
  #
  #   fixed_geo_unit_index_local <- geo_unit_index[fixed_hierarchy_levels] |>
  #     dplyr::distinct()
  #   fixed_geo_unit_index_global <- global_fit$geo_unit_index[fixed_hierarchy_levels] |>
  #     dplyr::distinct()
  #   missing_geos <- fixed_geo_unit_index_local |>
  #     dplyr::anti_join(fixed_geo_unit_index_global,
  #                      by = fixed_hierarchy_levels)
  #   if (nrow(missing_geos) > 0) {
  #     stop("All geographic units that appear in the data for the current fit at hierarchical levels for which any parameter is fixed must have also been included in the data used for the `global_fit`.")
  #   }
  # }

  # put together stan data
  stan_data <- list(
    C = nrow(geo_unit_index),
    N = nrow(data),
    # for geo_unit:
    # in case of NA values in data$c, replace with the dummy value 0
    # this is only used in multiscale fitting with mixed national and subnational data
    geo_unit = array(ifelse(is.na(data$c), 0L, data$c)),
    y = array(data[[y]])
  )

  ##### Set up hierarchical structures ######
  hier_data <- hier_stan_data  <- list()
  hier_data[["Omega_data"]] <- hierarchical_data(geo_unit_index, hierarchical_level)
  #hier_data[["a_data"]] <- hierarchical_data(geo_unit_index, hierarchical_splines)
  # hier_stan_data[["a"]] <- hierarchical_param_stan_data(
  #   global_fit = global_fit,
  #   param_name = "a",
  #   param_data = hier_data[["a_data"]],
  #   hierarchical_terms_fixed = hierarchical_splines_terms_fixed,
  #   hierarchical_sigmas_fixed = hierarchical_splines_sigmas_fixed)
  hier_stan_data[["Omega"]] <- hierarchical_param_stan_data(
    global_fit = global_fit,
    param_name ="Omega",
    param_data = hier_data[["Omega_data"]],
    hierarchical_terms_fixed = hierarchical_level_terms_fixed,
    hierarchical_sigmas_fixed = hierarchical_level_sigmas_fixed)
  # to check if this is needed for simplified stuff
  hier_stan_data <- purrr::list_flatten(hier_stan_data, name_spec = "{inner}")



  ##### Set up handing of data model hyperparameters ######
  nonse_data <- list(
    "fix_nonse" = as.integer(fix_nonse),
    "nonse_fixed" = numeric(0))
  if (fix_nonse) {
    if (is.null(global_fit)) {
      stop("fix_nonse was set to TRUE, but a global_fit was not provided.")
    }
    for (parname in "nonse"){
      nonse_data[[paste0(parname, "_fixed")]] <- global_fit$post_summ %>% filter(variable == paste0(parname, "[1]")) %>% pull(median)
    }
  }# end fixing dm pars



  ##### Create list with combined inputs/outputs ####


  # to pass to stan
  stan_data <- c(
                 stan_data,
                 hier_stan_data,
                 nonse_data)
  # pass back to user
  result <- list(
    stan_data = stan_data,
    geo_unit_index = geo_unit_index,
    y = y,
    area = area,
    # for re-use in (more) local fits
    hierarchical_level = hierarchical_level
  )
   result <- c(result,
             # hier_data needs to be unlisted in result
             hier_data)


  if (compile_model){
    result$stan_model <- stan_model
  }




    init_ll <- NULL
    fit <- stan_model$sample(
      stan_data,
      # save_latent_dynamics = TRUE,
      init = init_ll,
      chains = chains,
      parallel_chains  = chains,
      iter_sampling = iter_sampling,
      iter_warmup = iter_warmup,
      seed = seed,
      refresh = refresh,
      adapt_delta = adapt_delta,
      max_treedepth = max_treedepth
    )

    result <- c(result,
                samples = fit)
  result
}


get_posterior_summaries_simplified <- function(
    fit,
    process_indicator_prefixes = c(""),
    dm_indicator_prefixes = c(""),
    process_params = c("Omega_raw", "Omega_sigma"),
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



