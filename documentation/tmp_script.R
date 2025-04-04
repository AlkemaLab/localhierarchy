devtools::load_all(here::here())
fit1a <- fit_model_simplified(runstep = "step1a",
                              hierarchical_level     = c("intercept",  "subcluster", "iso"),

                              survey_df = dat,
                              chains = 4)

# functions to clean out
## check_nas
## hier data helpers



# overview of function calls
# get the stan_datat
data <- tibble(subcluster = c("A", "A", "B", "B"), iso = c("iso1", "iso2", "iso3", "iso3"))
hierarchical_level <- c("intercept",  "subcluster", "iso")
area <- "iso"
geo_unit_index <- get_geo_unit_index_data(data,
                                          hierarchical_levels = c(hierarchical_level),
                                          area = area)
hier_data <- hierarchical_data(geo_unit_index, hierarchical_level)
# here the global fit can be used
hier_stan_data <- hierarchical_param_stan_data(
  global_fit = NULL,
  param_name ="mu",
  param_data = hier_data #[["mu_data"]] #,
  # used only when fixing things
  #  hierarchical_terms_fixed = hierarchical_level_terms_fixed,
  #  hierarchical_sigmas_fixed = hierarchical_level_sigmas_fixed
)

# when working with a vector of parameters,
# eg instead of mu[i] we have mu[i, k] for k>1
# with same hierarchical model
# then hier_stan_data changes only if we fix things (as we need to input mu[i,k]s as opposed to mu[i]s)
hier_stan_data <- hierarchical_param_stan_data(
  global_fit = NULL,
  param_name ="mu",
  param_num_indices = 2,
  param_data = hier_data #[["mu_data"]] #,
  # used only when fixing things
  #  hierarchical_terms_fixed = hierarchical_level_terms_fixed,
  #  hierarchical_sigmas_fixed = hierarchical_level_sigmas_fixed
)




