# Create hierarchical data structure for a hierarchical model.

Create hierarchical data structure for a hierarchical model.

## Usage

``` r
hierarchical_data(geo_unit_index_data, hierarchical_level)
```

## Arguments

- geo_unit_index_data:

  tibble with unique hierarchical levels (one column for each level)

- hierarchical_level:

  vector with hierarchical spec

## Value

list with model matrix and info on start and end indices associated with
different levels
