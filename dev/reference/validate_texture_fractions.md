# Validate soil texture fractions

Internal helper that validates soil particle-size fractions (sand, silt,
and clay). If exactly two of the three fraction columns are present, the
missing column is created and filled with `NA` to be later computed in
[`complete_texture_fractions()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/complete_texture_fractions.md).
Samples with fewer than two provided fractions are retained but skipped
during texture validation and classification.

## Usage

``` r
validate_texture_fractions(df)
```

## Arguments

- df:

  A data frame containing `sample_id` and at least two of
  `sand_percent`, `silt_percent`, and `clay_percent`.

## Value

A data frame with validated soil texture fraction columns
(`sand_percent`, `silt_percent`, `clay_percent`). If exactly two of the
fractions are present, the third is created and filled with `NA` for
downstream completion.

## Details

Fractions are rounded prior to validation. Warns if fewer than two
fractions are provided or if exactly one fraction is missing. Errors if
fraction values are outside the allowable range (0-100) or if complete
fractions do not sum to 100 (+/- 1).
