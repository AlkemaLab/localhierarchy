



#' Fit model for localhierarchy model
#'
#' This function fits the localhierarchy model
#'
#' @param survey_df tibble with survey data
#' @param y column name of outcome, defaults to `y`.
#' @param area column name of the area of each observation (used as geounit; iso or subnational region)
#' @param area_select area name to use for local run (eg iso code or subnat region name)
#'
#' @param runstep Type of run, defines which model fitting step to perform (see Details for options).
#' @param global_fit optional global fit object, used to obtain fixed
#'   values to use for some parameters in the current fit (see Details).
#' @param hierarchical_level vector specifying hierarchical structure used for mu
#' @param add_subnational_hierarchy level that's added to the hierarchy for subnational, defaults to `subnat`
#' @param use_globalsubnat_fromnat Logical, whether in a local subnational run,
#' to use the global fit derived from national data
#' if TRUE and local subnat run, global_fit needs to contain object `fit_globalsubnat_fromnat`
#' @param mu_isvector Logical, TRUE if mu is a vector, defaults to FALSE
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
#' @returns List that contains samples, stan_data, other information relevant to model fit (arguments),
#' and for global fits, point estimates of relevant parameters (post_summ).
#' For subnational global fits, the list includes fit_globalsubnat_fromnat, which is the global fit with
#' additional subnational sigmas added to the postsum object.
#'
#' @export
#'
#' @details
#' The `fit_model_localhierarchy` function fits the toy example for hierarchical models/seq fitting.
#' The argument \code{runstep} determines the type of run to perform. The
#' following run steps are supported:
#' * "global_national": Fit the global model.
#' * "local_national": Fit the model to data from a single country, using a global_national fit.
#' * "global_subnational": Fit the model to global database with subnational data, using a global_national fit.
#' * "local_subnational": Fit the model to subnational data from a single country or region, using a global_subnational fit.
#' This is also explained in the article with the package.
#'
#' Details on hierarchical set ups used
#' The package allows the structure of the hierarchical prior to be configured by the user
#' through the  \code{hierarchical_level} argument.
#' These arguments expect a character vector that specifies a nesting hierarchical structure.
#' Each element of the vector must be either "intercept" or a column name in the survey dataset, where
#' "intercept" will add a global intercept for the parameter.
#' The vector must be in descending order in terms of the hierarchy: that is, it starts with
#' "intercept" and proceeds down the hierarchy.
#'
#' For example, suppose we are fitting country-level data, where the dataset has columns
#' "name_country", "name_sub_region", and "name_region" containing the name of the country,
#' sub-region, and region that each observation belongs to. To specify that parameter mu
#' should be fitted with a hierarchical model in which countries are nested within sub-regions within regions within world,
#' we would use the argument
#' \code{hierarchical_level = c("intercept", "name_region", "name_sub_region", "name_country")}.
#'
#' Optionally, model parameters can be fixed to values from a previous model fit
#' provided via the \code{global_fit} argument. In a typical use case, the
#' \code{global_fit} will have been fit to data from many geographic units
#' (e.g., all countries), while the current fit uses data from a smaller number
#' of locations.
#' When using a global_fit to fix parameter values, what exactly is fixed is determined by the runstep and global_fit combi.
#'
#' @importFrom cmdstanr cmdstan_model
#' @importFrom tibble tibble
#' @import dplyr
#'
#'
#'
#'
#'
#'
fit_model_localhierarchy <- function(
  survey_df,
  y = "y", # observation
  area = "iso", # add default or set in function?
  area_select  = NULL, # used for local run
  runstep, # type of run, see details
  global_fit = NULL,
  hierarchical_level     = c("intercept", "subcluster", "iso"),
  add_subnational_hierarchy = "subnat", # this is what's added to the hierarchy for subnational
  use_globalsubnat_fromnat = TRUE,
  mu_isvector = FALSE, # TRUE if mu is a vector
  # settings for sampling
  chains = 4,
  iter_sampling = 300,
  iter_warmup = 150,
  compile_model  = TRUE, force_recompile = FALSE,
  seed = 1234,
  refresh = 200,
  adapt_delta = 0.9,
  max_treedepth = 14

) {

  if(length(hierarchical_level) == 0) {
    stop("No hierarchical structure supplied. See the hierarchical_level argument.")
  }

  if (!runstep %in% c("global_national", "local_national", "global_subnational", "local_subnational")){
    stop("runstep not yet implemented!")
  }
  if (runstep %in% c("global_national")){
    print("We do a global fit.")
    print("We don't fix anything")
    hierarchical_level_sigmas_fixed = c()
    hierarchical_level_terms_fixed = c()
    fix_nonse = FALSE
  } else {
    if (is.null(global_fit)){
      stop("Need a global fit for this run.")
    }

    print("We use a global fit, and take selected or all settings from there.")
    if (use_globalsubnat_fromnat & runstep %in% c("local_subnational")){
      add_subnational_hierarchy <- global_fit$area
      global_fit <- global_fit$fit_globalsubnat_fromnat
    }
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
    if (runstep %in% c("global_subnational", "local_subnational")){
      print("We fix data model parameters.")
      fix_nonse <- TRUE
      if (runstep %in% c("global_subnational") | use_globalsubnat_fromnat) {
        print("We add a level for subnational hierarchical settings ")
        hierarchical_level <- c(hierarchical_level, add_subnational_hierarchy)
      }
      if (runstep %in% c("global_subnational")){
        print("For hierarchical terms, we fix things up to the 2nd-lowest level (here 3rd is used).")
        hierarchical_level_terms_fixed = hierarchical_level[1:(length(hierarchical_level)-2)]
        print("For sigma terms, we fix up to 2nd-lowest level.")
        hierarchical_level_sigmas_fixed = hierarchical_level[1:(length(hierarchical_level)-1)]
      } else {
        print("For hierarchical terms, we fix things up to the 2nd-lowest level (here 3rd is used).")
        hierarchical_level_terms_fixed = hierarchical_level[1:(length(hierarchical_level)-2)]
        print("For sigma terms, we fix up to lowest level.")
        hierarchical_level_sigmas_fixed = hierarchical_level[1:(length(hierarchical_level))]
      }
    }
  }
  print("Hierarchical levels used for this run:")
  print(hierarchical_level)
  print("Hierarchical terms fixed:")
  print(hierarchical_level_terms_fixed)
  print("Hierarchical sigmas fixed:")
  print(hierarchical_level_sigmas_fixed)


  ##### Data processing  and Setup data for Stan #####
  # select data based on area_select
  if (!is.null(area_select)){
    survey_df <- survey_df %>%
      dplyr::filter(!!rlang::sym(area) %in% area_select)
  }
  geo_unit_index <- get_geo_unit_index_data(survey_df,
                                            hierarchical_levels = c(hierarchical_level),
                                            area = area)
  hierarchical_column_names <- unique(hierarchical_level) %>%
    setdiff("intercept")
  survey_df <- survey_df %>%
    dplyr::left_join(geo_unit_index, by = hierarchical_column_names)
  # Make sure there are no NAs in any of the columns
  for(column in hierarchical_column_names) {
    if(column == "intercept") next
    check_nas(survey_df, column)
  }

  #In "local" fits, ensure that for all hierarchy levels where any quantity is
  #fixed, all geographic units in data used for the local fit also were present
  # in the global fit
  if (!is.null(global_fit)) {
    fixed_hierarchy_levels <- unique(c(
      # commenting out sigmas for use_globalsubnat_fromnat = TRUE,
      # could make this depend on that argument too
      # note: now we do NOT check whether sigma was estimated
      # confirmed that this results in an error when no sigmas at lower level included
      # hierarchical_level_sigmas_fixed,
                                       hierarchical_level_terms_fixed))
    fixed_hierarchy_levels <- fixed_hierarchy_levels[fixed_hierarchy_levels != "intercept"]
    fixed_geo_unit_index_local <- geo_unit_index[fixed_hierarchy_levels] |>
      dplyr::distinct()
    fixed_geo_unit_index_global <- global_fit$geo_unit_index[fixed_hierarchy_levels] |>
      dplyr::distinct()
    missing_geos <- fixed_geo_unit_index_local |>
      dplyr::anti_join(fixed_geo_unit_index_global,
                       by = fixed_hierarchy_levels)
    if (nrow(missing_geos) > 0) {
      stop("All geographic units that appear in the data for the current fit at hierarchical levels for which any parameter is fixed must have also been included in the data used for the `global_fit`.")
    }
  }

  # put together stan data
  stan_data <- list(
    n_geounit = nrow(geo_unit_index),
    N = nrow(survey_df),
    geo_unit = survey_df$c,
    y = array(survey_df[[y]]),
    verysmallnumber = 0.00001 # lower bound for sds
  )
  # area = survey_df[[area]]


  ##### Set up hierarchical structures ######
  stan_data[["mu_isvector"]] <- mu_isvector
  parname <- "mu"

  ### get the model file


  if (!mu_isvector){
    #stan_file_path = file.path(here::here("inst/stan/", "hierfunctions_seq_muscalar.stan"))
    ### for pkgdown to be able to find the file, need to use the install
    stan_file_path <- system.file("stan/hierfunctions_seq_muscalar.stan", package = "localhierarchy")
  } else {
    stan_data[["mu_k_terms"]] <- 3 # 2 or higher
    #stan_file_path = file.path(here::here("inst/stan/", "hierfunctions_seq_muvector.stan"))
    ### for pkgdown to be able to find the file, need to use the install
    stan_file_path <- system.file("stan/hierfunctions_seq_muvector.stan", package = "localhierarchy")
  }
  stan_data[[paste0(parname, "_scalarprior_sd")]] <- 1
  stan_data[[paste0(parname, "_scalarprior_mean")]] <- 0
  stan_data[[paste0(parname, "_prior_sd_sigma_estimate")]] <- 1
  hier_data <- hier_stan_data  <- list()
  hier_data[[paste0(parname, "_data")]] <- hierarchical_data(geo_unit_index_data = geo_unit_index, hierarchical_level = hierarchical_level)
  hier_stan_data[[parname]] <- hierarchical_param_stan_data(
    global_fit = global_fit,
    param_name = parname,
    param_data = hier_data[[paste0(parname, "_data")]],
    hierarchical_terms_fixed = hierarchical_level_terms_fixed,
    hierarchical_sigmas_fixed = hierarchical_level_sigmas_fixed)
  # this is needed when working with >1 parameter
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
      nonse_data[[paste0(parname, "_fixed")]] <- global_fit$post_summ %>% filter(variable == paste0(parname, "[1]")) %>% pull(postmean)
    }
  }# end fixing dm pars

  ##### Load model #####
  if (compile_model){
    stan_model <- cmdstanr::cmdstan_model(
      stan_file = stan_file_path,
      force_recompile = force_recompile
    )
  }

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
  #return(result)



   add_inits <- FALSE #TRUE
   # if TRUE, can consider avoiding warnings
   options(cmdstanr_warn_inits = FALSE)
    if (add_inits){
      #print("We set initial values using init_fun_localhierarchy")
      init_ll <- lapply(1:chains, function(id) init_fun_localhierarchy(chain_id = id, stan_data))
    } else {
      init_ll <- NULL
    }


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
      max_treedepth = max_treedepth,
      # if we don't want to get progress reports
      show_messages = FALSE
    )


    result <- c(result,
                samples = fit)
    if (runstep %in% c("global_national", "global_subnational")){
      post_summ <- get_posterior_summaries_localhierarchy(result)
      result <- c(result,
          list(post_summ = post_summ))
      if (runstep %in% c("global_subnational")){
        # we also want to produce the object that has the national global fit with
        # summaries that are extended to include additional subnational sigmas
        # get this object (w/o samples)
        fit_globalsubnat_fromnat <- global_fit
        fit_globalsubnat_fromnat$samples <- NULL
        param_add <- anti_join(result$post_summ %>%
                                 # select rows where variable_no_index has sigma in it
                                 filter(grepl("sigma", variable_no_index)),
                               fit_globalsubnat_fromnat$post_summ %>%
                                 filter(grepl("sigma", variable_no_index)))
        # param_add
        fit_globalsubnat_fromnat$post_summ <- bind_rows(fit_globalsubnat_fromnat$post_summ,
                                                        param_add)


        result <- c(result,
                    list(fit_globalsubnat_fromnat = fit_globalsubnat_fromnat))
      }
    }
  result
}






