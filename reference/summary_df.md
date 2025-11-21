# Compute summaries

This function computes posterior summaries for a given vector and
returns them in a tibble format.

## Usage

``` r
summary_df(x)
```

## Arguments

- x:

  vector with values

## Value

tibble with 2.5th quantile, posterior mean, and 97.5th quantile, and
quant = c(0.025, 0.5, 0.975)
