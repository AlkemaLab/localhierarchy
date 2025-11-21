# Get mu_raw_labeled

This function extracts the mu_raw_estimate samples from a fitted model
and labels them with their hierarchical level.

## Usage

``` r
get_mu_raw_labeled(fit, parname, morethan1param = FALSE)
```

## Arguments

- fit:

  needs to include parname_raw_estimate (example: mu_raw)

- parname:

  selected parameter name (example: mu)

- morethan1param:

  does parname refer to more than 1 parameter (a vector)

## Value

tibble with samples of parname_raw_estimate, including info on
hierarchical level
