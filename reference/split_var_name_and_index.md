# Split variable name and index

For a data frame of estimates with a column that contains combined info
about a variable name and row and/or column indices, add columns
containing just the variable name and the indices.

## Usage

``` r
split_var_name_and_index(estimate_df, num_inds)
```

## Arguments

- estimate_df:

  data frame with a column called `variable` that contains `parname[i]`
  or `parname[i,j]` entries

- num_inds:

  number of indices in the variable name (used for 1 or 2)

## Value

data frame with the same columns as `estimate_df` plus columns
containing just the variable name and the indices.
