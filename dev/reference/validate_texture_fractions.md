# Validate soil texture fractions

Internal helper that validates the presence of soil particle-size
fractions (sand, silt, and clay). Rounds fractions prior to validation.
Errors if two or more fractions are missing for any sample and warns if
exactly one fraction is missing.

## Usage

``` r
validate_texture_fractions(df)
```

## Arguments

- df:

  A data frame containing `sample_id`, `sand_percent`, `silt_percent`,
  and `clay_percent`.

## Value

A data frame with a helper column `missing_n` indicating the number of
missing fractions per sample.
