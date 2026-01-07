# Complete missing soil texture fractions

Internal helper that computes a missing soil fraction (sand, silt, or
clay) when exactly one is missing, using `100 - (sum of the other two)`.

## Usage

``` r
complete_texture_fractions(df)
```

## Arguments

- df:

  A data frame containing `sand_percent`, `silt_percent`, and
  `clay_percent`.

## Value

A data frame with completed soil fraction percentages.
