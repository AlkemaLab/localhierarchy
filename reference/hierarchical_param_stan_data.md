# Stan data for hierarchical parameters

Set up data for Stan related to hierarchical parameters

## Usage

``` r
hierarchical_param_stan_data(
  param_name,
  param_data,
  global_fit = NULL,
  hierarchical_terms_fixed,
  hierarchical_sigmas_fixed
)
```

## Arguments

- param_name:

  The name of the parameter we are working with, e.g. "mu"

- param_data:

  Data structures as constructed by hierarchical_data for that parameter

- global_fit:

  an optional "global" fit that will be used to extract parameter
  estimates for any specified hierarchical units to fix. Defaults to
  NULL, no fixing.

- hierarchical_terms_fixed:

  character vector specifying hierarchical levels for which the terms
  should be fixed (subset of `hierarchical_levels`). Used only if global
  fit is provided.

- hierarchical_sigmas_fixed:

  character vector specifying hierarchical levels for which the terms
  should be fixed (subset of `hierarchical_levels`, intercept is always
  included) Used only if global fit is provided.

## Value

named list with Stan data relevant to the hierarchical set up for this
parameter, e.g. if the `param_name` is `"mu"`, these will be
`mu_n_terms, mu_n_sigma, mu_re_start, mu_re_end, mu_model_matrix, mu_n_terms_fixed, mu_n_terms_estimate, mu_raw_fixed, mu_n_sigma_fixed, mu_n_sigma_estimate, mu_sigma_fixed`
