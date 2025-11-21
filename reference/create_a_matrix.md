# Create a matrix

This function takes a postsumm object and returns a matrix with the
relevant information

## Usage

``` r
create_a_matrix(post_summ)
```

## Arguments

- post_summ:

  tibble with columns `variable` and `postmean`, variable examples are
  `a[1,1]`, `a[2,1]`, etc.

## Value

matrix with rows and columns corresponding to the indices in the
variable
