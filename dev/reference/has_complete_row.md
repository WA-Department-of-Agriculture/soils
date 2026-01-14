# Check for at least one complete row for required columns

Returns TRUE if the data frame contains at least one row where all
specified columns are non-missing.

## Usage

``` r
has_complete_row(df, cols)
```

## Arguments

- df:

  A data frame.

- cols:

  A character vector of column names that must be complete.

## Value

Logical scalar. TRUE if at least one complete row exists.
