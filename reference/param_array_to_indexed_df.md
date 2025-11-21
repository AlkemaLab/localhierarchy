# Starting from an array of parameter values (of dimension 1 or 2), create a data frame with columns `variable` with the combined parameter name and indices, and `mean` with the parameter values

Starting from an array of parameter values (of dimension 1 or 2), create
a data frame with columns `variable` with the combined parameter name
and indices, and `mean` with the parameter values

## Usage

``` r
param_array_to_indexed_df(param_values, num_inds, n, m, param_name)
```

## Arguments

- param_values:

  Parameter values

- num_inds:

  Dimension (1 or 2)

- n:

  Number of indices

- m:

  For dimension 2, the number of indices in the 2nd dimension

- param_name:

  Parameter name

## Value

Data frame with columns `variable` with the combined parameter name and
indices, and `mean` with the parameter values
