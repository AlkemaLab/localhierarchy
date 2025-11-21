# Plot summaries of hierarchical parameters

Display outputs from posterior_summary_hierparam_localhierarchy

## Usage

``` r
plot_posterior_summaries_localhierarchy(
  res,
  hierarchy_select = NULL,
  areas_select = NULL,
  res2 = NULL,
  modelname1 = "model 1",
  modelname2 = "model 2",
  k_select = NULL,
  dodge = position_dodge(width = 0.5)
)
```

## Arguments

- res:

  output from posterior_summary_hierparam_localhierarchy

- hierarchy_select:

  optional, what hierarchical level to show? if NULL, all levels are
  shown

- areas_select:

  optional: specific areas in a level to filter by, allowed only if one
  hierarchical level is selected

- res2:

  optional, output from posterior_summary_hierparam_localhierarchy for
  comparison

- modelname1:

  label for res

- modelname2:

  label for res2

- k_select:

  optional, if res contains k, which k values to show? if NULL, all k
  values are shown

- dodge:

  used for offsetting plots, default is 0.5

## Value

ggplot object
