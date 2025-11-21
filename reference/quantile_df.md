# Compute quantiles

This function computes quantiles for a given vector and returns them in
a tibble format.

## Usage

``` r
quantile_df(x, probs = c(0.025, 0.5, 0.975))
```

## Arguments

- x:

  vector with values

- probs:

  vector of quantiles

## Value

tibble with the requested quantiles
