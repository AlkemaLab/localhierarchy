# Plot prior and posteriors of sigmas from hierarchical models

This function plots the prior and posterior densities of sigma_estimate
parameters.

## Usage

``` r
plot_prior_post_sigmas_localhierarchy(fit, parname)
```

## Arguments

- fit:

  List, needs to include parname_sigma_estimate and stan_data

- parname:

  Selected parameter name (example: "mu")

## Value

Plot with density of sigma_estimate and prior added
