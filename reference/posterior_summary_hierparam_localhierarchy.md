# Calculate posterior summaries for hierarchical parameters

Calculate posterior summaries for hierarchical parameters

## Usage

``` r
posterior_summary_hierparam_localhierarchy(
  fit,
  parname,
  morethan1param = FALSE,
  hierarchical_levels = fit$hierarchical_level
)
```

## Arguments

- fit:

  needs to include parname_star

- parname:

  selected parameter name (example: "mu")

- morethan1param:

  does paramname refer to more than 1 parameter (a vector)

- hierarchical_levels:

  specifies the names of the hierarchical levels (defaults to
  fit\$hierarchical_level)

## Value

list with summaries of mu for each hierarchical level (units with each
level) these mus are obtained by summing up all relevant etas for
morethan1param, each level has a list where k refers to the index
