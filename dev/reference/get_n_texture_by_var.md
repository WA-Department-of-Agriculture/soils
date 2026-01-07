# Calculate n samples and most frequent texture by a grouping variable

This function is used in `summarize_by_var`.

## Usage

``` r
get_n_texture_by_var(results_long, producer_info, var)
```

## Arguments

- results_long:

  Dataframe in tidy, long format with columns: `sample_id`, `texture`.

- producer_info:

  Vector of producer's values for the grouping variable.

- var:

  Variable to group and summarize by.
