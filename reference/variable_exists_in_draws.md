# Determine whether or not a variable exists in draws from a fit This is similar to the function posterior:::check_existing_variables, but note that function is not exported from posterior

Determine whether or not a variable exists in draws from a fit This is
similar to the function posterior:::check_existing_variables, but note
that function is not exported from posterior

## Usage

``` r
variable_exists_in_draws(fit, var_name)
```

## Arguments

- fit:

  fpemplus fit object

- var_name:

  name of variable as character string

## Value

TRUE or FALSE indicating whether or not the variable exists in the fit.
