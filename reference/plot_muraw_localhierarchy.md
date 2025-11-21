# Plot prior and posterior densities of location parameters

This function plots the posterior densities of location (`mu_raw`)
parameters, with priors added.

## Usage

``` r
plot_muraw_localhierarchy(
  fit,
  parname,
  morethan1param = FALSE,
  nresultsperpage = 30
)
```

## Arguments

- fit:

  List that includes "parname"\_raw_estimate and stan_data

- parname:

  Selected parameter name (example: mu)

- morethan1param:

  Logical, does parname refer to more than 1 parameter (a vector)

- nresultsperpage:

  Number of results per page in summary plots

## Value

lists with list 'summary_plots' and list 'plots_allmuraw'. summary plots
gives summary CIs, nresultsperpage at a time. plots_allmuraw gives all
the individual plots of each mu_raw, with density per chain and prior
added If morethan1param = TRUE, then each list contains a list per
parameter k
