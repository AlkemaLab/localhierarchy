# get_posterior_summary_one_param

Get posterior summary for one parameter

## Usage

``` r
get_posterior_summary_one_param(fit, param_name)
```

## Arguments

- fit:

  fit object

- param_name:

  parameter name w/o index

  need to get the estimate and/or fixed value for each indexed parameter
  difficulty = stacking estimate on top of fixed values (for vector or
  array) note that this does NOT get into hierarchical info (and is same
  for hier and nonhier param) it is just the stacking

## Value

tibble with variable (parname with index used in model) and postmean
(posterior mean)
