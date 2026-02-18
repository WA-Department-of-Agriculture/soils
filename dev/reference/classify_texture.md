# Classify USDA soil texture from particle-size fractions

Validates soil particle-size fractions (sand, silt, and clay), completes
missing values when possible, and assigns a USDA soil texture class.

## Usage

``` r
classify_texture(df)
```

## Source

Thresholds for texture classification are based on the [USDA NRCS Soil
Texture
Calculator](https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator).

## Arguments

- df:

  A data frame containing the columns `sample_id`, `sand_percent`,
  `silt_percent`, and `clay_percent`. An optional `texture` column can
  also be provided.

## Value

A data frame with a `texture` column containing USDA soil texture
classes (if enough soil fraction data are provided). Soil fractions are
rounded to whole numbers.

## Details

`classify_texture()` applies the following validation rules and
assumptions:

- **Eligibility**

  - Each sample must contain values for at least two of `sand_percent`,
    `silt_percent`, and `clay_percent` to be eligible for fraction-based
    texture classification.

- **Errors** (cause validation to fail)

  - All fraction values must fall within the range 0–100.

  - Samples with all three fractions must sum to 100 with a ±1 tolerance
    (allowable range 99–101).

- **Warnings** (validation continues)

  - When exactly one fraction is missing, it is calculated as
    `100 - (sum of the other two)`.

  - Samples with fewer than two provided fractions **and no** provided
    texture do not have sufficient data for texture classification.
    These samples are retained and returned with an `NA` texture value.

- **Special cases** (no warning or error)

  - Samples with fewer than two provided fractions **and** a provided
    texture will preserve the texture without modification.

## Examples

``` r
# Three samples classified without error

df <- data.frame(
  sample_id = c(1, 2, 3),
  sand_percent = c(20, 45, 75),
  silt_percent = c(65, 35, 15),
  clay_percent = c(15, 20, 10)
)

classify_texture(df)
#>   sample_id    texture sand_percent silt_percent clay_percent
#> 1         1  Silt Loam           20           65           15
#> 2         2       Loam           45           35           20
#> 3         3 Sandy Loam           75           15           10

# Error when any fraction is outside the allowable range (0-100)

df <- data.frame(
  sample_id = c(1, 2, 3),
  sand_percent = c(40, 0, 65),
  silt_percent = c(40, 55, 5),
  clay_percent = c(20, 110, 30)
)

try(classify_texture(df))
#> Error : ✖ Soil texture validation failed.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Samples with errors to correct:
#> • Sample 2 must have all fraction values between 0 and 100.
#> • Sample 2 must have fractions that sum to 100 (+/- 1).

# Error when fractions do not sum to 100 +/- 1 (allowable range: 99-101)

df <- data.frame(
  sample_id = c(1, 2, 3),
  sand_percent = c(40, 0, 90),
  silt_percent = c(40, 55, 5),
  clay_percent = c(20, 45, 30)
)

try(classify_texture(df))
#> Error : ✖ Soil texture validation failed.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Samples with errors to correct:
#> • Sample 3 must have fractions that sum to 100 (+/- 1).

# Warn when one fraction is missing
# The missing fraction is calculated as 100 minus the other two

df <- data.frame(
  sample_id = c(1, 2, 3),
  sand_percent = c(NA, 60, 25),
  silt_percent = c(45, 10, 40),
  clay_percent = c(50, 30, 35)
)

classify_texture(df)
#> Warning: ! Soil texture validation completed with assumptions.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Samples with assumptions:
#> • Sample 1 is missing one fraction and will be calculated as 100 minus the
#>   other two.
#>   sample_id         texture sand_percent silt_percent clay_percent
#> 1         1      Silty Clay            5           45           50
#> 2         2 Sandy Clay Loam           60           10           30
#> 3         3       Clay Loam           25           40           35

# Warn when a sample has insufficient data and classification is skipped
# One sample is missing two soil fractions

df <- data.frame(
  sample_id = c(1, 2, 3),
  sand_percent = c(40, NA, 65),
  silt_percent = c(40, 55, 5),
  clay_percent = c(20, NA, 30)
)

classify_texture(df)
#> Warning: ! Soil texture validation completed with assumptions.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Samples with assumptions:
#> • Sample 2 has fewer than two fractions and will be skipped during texture
#>   classification.
#>   sample_id         texture sand_percent silt_percent clay_percent
#> 1         1            Loam           40           40           20
#> 2         2            <NA>           NA           55           NA
#> 3         3 Sandy Clay Loam           65            5           30

# No fractions provided, but texture is supplied
# No warning; existing texture is preserved

df <- data.frame(
  sample_id = c(1, 2),
  sand_percent = c(NA, NA),
  silt_percent = c(NA, NA),
  clay_percent = c(NA, NA),
  texture = c("Loam", "Sandy loam")
)

classify_texture(df)
#>   sample_id sand_percent silt_percent clay_percent    texture
#> 1         1           NA           NA           NA       Loam
#> 2         2           NA           NA           NA Sandy loam
```
