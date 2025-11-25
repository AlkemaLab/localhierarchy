localhierarchy
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

## Background

Bayesian hierarchical models are widely used in global health
estimation, where data availability may vary across national or
subnational populations. Such models are typically fitted to a global
database, e.g., to produce national-level estimates for all countries in
the world or in some region. To facilitate analysis at a local level,
models are often desirable that are informed by global models but can be
fitted to just a subset of the data, such as data for one country. We
refer to such models as local models: models that are derived from
global hierarchical models but adapted such that they can be fitted to
the data from one population alone.

The `localhierarchy` R package provides functionality for fitting
Bayesian hierarchical models in settings where both global and local
estimation is required. The package provides R functions and Stan model
components to support global modeling, in which all parameters in
hierarchical models are estimated, and local modeling, in which
parameters are estimated for only one or a small number of populations,
using fixed values from a global model fit.

The article on this website presents a practical introduction to the
package for the applied user and illustrates the package’s functionality
through examples for national estimation.

## Installation

- `localhierarchy` depends on `cmdstanr`: Instructions for installing
  `cmdstanr` are available in their [Getting
  started](https://mc-stan.org/cmdstanr/articles/cmdstanr.html) guide.

- Install `localhierarchy` from Github:
  `remotes::install_github("AlkemaLab/localhierarchy")`

## Examples

See
[article](https://alkemalab.github.io/localhierarchy/articles/localhierarchy_article.html)
on package website, at
<a href="https://alkemalab.github.io/localhierarchy/index.html"
class="uri">https://alkemalab.github.io/localhierarchy</a>.

## Citation

Please cite as follows:

``` r
citation("localhierarchy")
#> To cite package 'localhierarchy' in publications use:
#> 
#>   Alkema L, Mooney S, Ray E, Susmann H (2025). _localhierarchy: An R
#>   package to facilitate fitting of global and local Bayesian
#>   hierarchical models_. R package version 0.9,
#>   <https://alkemalab.github.io/localhierarchy/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {localhierarchy: An R package to facilitate fitting of global and local Bayesian hierarchical models},
#>     author = {Leontine Alkema and Shauna Mooney and Evan Ray and Herbert Susmann},
#>     year = {2025},
#>     note = {R package version 0.9},
#>     url = {https://alkemalab.github.io/localhierarchy/},
#>   }
```

## Funding

This work was supported, in whole or in part, by the Bill & Melinda
Gates Foundation (INV-00844).
