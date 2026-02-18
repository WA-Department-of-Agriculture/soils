# Check if a data frame has at least one complete row for the specified columns

Returns TRUE if there is at least one row in the data frame where all
specified columns are non-missing (no `NA` values).

## Usage

``` r
has_complete_row(df, cols)
```

## Arguments

- df:

  A data frame.

- cols:

  A character vector of column names to check.

## Value

Logical scalar. TRUE if at least one row has all specified columns
complete, FALSE otherwise.

## Examples

``` r
has_complete_row(washi_data, c("crop", "county"))
#> [1] TRUE
```
