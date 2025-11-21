# Get posterior summaries for use in local models

Get posterior summaries for a set of parameters for use in local model

## Usage

``` r
get_posterior_summaries_localhierarchy(
  fit,
  params = c("mu_raw", "mu_sigma", "nonse")
)
```

## Arguments

- fit:

  fit object to summarize

- params:

  vector of parnames to summarize (w/o index)

## Value

tibble with variable (parname with index used in model) and postmean
(posterior mean)
