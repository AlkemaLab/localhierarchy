# Set initial values

This function sets the initial values for the parameters in a simple
hierarchical model.

## Usage

``` r
init_fun_localhierarchy(chain_id, stan_data)
```

## Arguments

- chain_id:

  the chain ID, used for setting the seed

- stan_data:

  a list containing the Stan data, to obtain length of parameter vectors

## Value

a list of initial values for the model parameters
