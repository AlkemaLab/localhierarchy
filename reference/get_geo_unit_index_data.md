# Get unique hierarchical levels from survey data

Get unique hierarchical levels from survey data and assign index "c"
based on area.

## Usage

``` r
get_geo_unit_index_data(data, hierarchical_levels, area)
```

## Arguments

- data:

  survey data of interest that contains the hierarchical_column_names

- hierarchical_levels:

  vector that contains one of more specs of hierarchical_level (incl
  intercept)

- area:

  unit of analysis (eg country or subnational region)

## Value

tibble with unique hierarchical levels (one column for each level) and
index "c" assigned based on area

## Examples

``` r
survey_dat <- tibble::tibble(subcluster = c("A", "A", "B", "B"),
    iso = c("iso1", "iso2", "iso3", "iso4"))
get_geo_unit_index_data(survey_dat,
        hierarchical_levels = c("intercept", "subcluster", "iso"),
        area = "iso")
#> # A tibble: 4 × 3
#>   subcluster iso       c
#>   <chr>      <chr> <int>
#> 1 A          iso1      1
#> 2 A          iso2      2
#> 3 B          iso3      3
#> 4 B          iso4      4
```
