# extract_parameter_subhierarchical

extract_parameter_subhierarchical

## Usage

``` r
extract_parameter_subhierarchical(
  hierarchical_data,
  subhierarchy,
  parname,
  star_samples,
  morethan1param = FALSE,
  add_standardizedmu = FALSE
)
```

## Arguments

- hierarchical_data:

  hierarchical_data, obtained from hierarchical_data(fit\$geo_unit,
  fit\$hierarchical_level)

- subhierarchy:

  selected hierarchical_level (example: "intercept" or "region")

- parname:

  selected parameter name (example: "mu")

- star_samples:

  samples from fit\$samples for parname_star

- morethan1param:

  does parname refer to more than 1 parameter (a vector)

## Value

a tibble with posterior samples of the selected parameter mu at the
subhierarchy level. these mus are obtained by summing up all relevant
etas variables are `name`, `value`, `.draw/iteration/chain' and `k\`
referring to parameter index if morethan1param = TRUE
