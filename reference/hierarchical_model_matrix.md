# Hierarchical model matrix

Create list with information including a model matrix, associated with
the geo_unit_data and hierarchical_levels

## Usage

``` r
hierarchical_model_matrix(geo_unit_index_data, hierarchical_level)
```

## Arguments

- geo_unit_index_data:

  tibble with unique hierarchical levels (one column for each level, one
  row per combi)

- hierarchical_level:

  vector with hierarchical spec from highest to lowest levels

## Value

A list with components `modelmatrix`, `assign`, and `index`:

- `modelmatrix`: A matrix where each column refers to one eta (number of
  rows equals number of lowest level units).

- `assign`: Integer vector of length equal to the number of etas; `0`
  for the intercept, then a hierarchical level index (starting at 2) for
  each eta level.

- `index`: A tibble with `n_eta` rows, and columns:

  - `i`: Index of eta

  - `column`: Hierarchical level

  - `level`: Name of hierarchical level

All components are associated with the provided `geo_unit_data` and
`hierarchical_levels`.
