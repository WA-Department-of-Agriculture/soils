# Calculate number of samples and most frequent texture (deprecated)

**\[deprecated\]**

## Usage

``` r
get_n_texture_by_var(results_long, producer_info, var)
```

## Arguments

- results_long:

  Data frame in tidy, long format containing `sample_id` and `texture`.

- producer_info:

  Vector of producer values for the grouping variable.

- var:

  Variable to group and summarize by.

## Value

Deprecated. Previously returned a data frame with `n` and `Texture` by
group.

## Details

This function is deprecated. Its functionality has been incorporated
directly into
[`summarize_by_var()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/summarize_by_var.md).
Do not use directly in new code.

## See also

summarize_by_var
