# Fit localhierarchy example model

This function fits the localhierarchy example model

## Usage

``` r
fit_model_localhierarchy(
  survey_df,
  y = "y",
  area = "iso",
  area_select = NULL,
  runstep,
  global_fit = NULL,
  hierarchical_level = c("intercept", "subcluster", "iso"),
  add_subnational_hierarchy = "subnat",
  use_globalsubnat_fromnat = TRUE,
  mu_isvector = FALSE,
  chains = 4,
  iter_sampling = 300,
  iter_warmup = 150,
  compile_model = TRUE,
  force_recompile = FALSE,
  seed = 1234,
  refresh = 200,
  adapt_delta = 0.9,
  max_treedepth = 14
)
```

## Arguments

- survey_df:

  tibble with survey data

- y:

  column name of outcome, defaults to `y`.

- area:

  column name of the area of each observation (used as geounit; iso or
  subnational region)

- area_select:

  area name to use for local run (eg iso code or subnat region name)

- runstep:

  Type of run, defines which model fitting step to perform (see Details
  for options).

- global_fit:

  optional global fit object, used to obtain fixed values to use for
  some parameters in the current fit (see Details).

- hierarchical_level:

  vector specifying hierarchical structure used for mu

- add_subnational_hierarchy:

  level that's added to the hierarchy for subnational, defaults to
  `subnat`

- use_globalsubnat_fromnat:

  Logical, whether in a local subnational run, to use the global fit
  derived from national data if TRUE and local subnat run, global_fit
  needs to contain object `fit_globalsubnat_fromnat`

- mu_isvector:

  Logical, TRUE if mu is a vector, defaults to FALSE

  Settings for sampling

- chains:

  number of chains to run

- iter_sampling:

  number of posterior samples to draw

- iter_warmup:

  number of warmup iterations

- compile_model:

  boolean indicator of whether to compile the Stan model

- force_recompile:

  boolean indicator of whether to force recompilation of the Stan model

- seed:

  random seed

- refresh:

  number of iterations between progress updates

- adapt_delta:

  target acceptance rate for the No-U-Turn Sampler

- max_treedepth:

  maximum tree depth for the No-U-Turn Sampler

## Value

List that contains samples, stan_data, other information relevant to
model fit (arguments), and for global fits, point estimates of relevant
parameters (post_summ). For subnational global fits, the list includes
fit_globalsubnat_fromnat, which is the global fit with additional
subnational sigmas added to the postsum object.

## Details

The `fit_model_localhierarchy` function fits the toy example for
hierarchical models/seq fitting. The argument `runstep` determines the
type of run to perform. The following run steps are supported:

- "global_national": Fit the global model.

- "local_national": Fit the model to data from a single country, using a
  global_national fit.

- "global_subnational": Fit the model to global database with
  subnational data, using a global_national fit (NISE modeling).

- "local_subnational": Fit the model to subnational data from a single
  country or region, using a global_subnational fit.

The options are also explained in the article with the package.

Details on hierarchical set ups used The package allows the structure of
the hierarchical prior to be configured by the user through the
`hierarchical_level` argument. These arguments expect a character vector
that specifies a nesting hierarchical structure. Each element of the
vector must be either "intercept" or a column name in the survey
dataset, where "intercept" will add a global intercept for the
parameter. The vector must be in descending order in terms of the
hierarchy: that is, it starts with "intercept" and proceeds down the
hierarchy.

For example, suppose we are fitting country-level data, where the
dataset has columns "name_country", "name_sub_region", and "name_region"
containing the name of the country, sub-region, and region that each
observation belongs to. To specify that parameter mu should be fitted
with a hierarchical model in which countries are nested within
sub-regions within regions within world, we would use the argument
`hierarchical_level = c("intercept", "name_region", "name_sub_region", "name_country")`.

Optionally, model parameters can be fixed to values from a previous
model fit provided via the `global_fit` argument. In a typical use case,
the `global_fit` will have been fit to data from many geographic units
(e.g., all countries), while the current fit uses data from a smaller
number of locations. When using a global_fit to fix parameter values,
what exactly is fixed is determined by the runstep and global_fit combi.
