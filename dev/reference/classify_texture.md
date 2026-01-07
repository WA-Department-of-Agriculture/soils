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

  A data frame containing the columns `sand_percent`, `silt_percent`,
  and `clay_percent`. An optional `texture` column can also be provided:
  if the sand/silt/clay fractions are present, `texture` will be
  overwritten by the classified value; if the fractions are missing,
  existing `texture` values are preserved.

## Value

A data frame with a `texture` column containing USDA soil texture
classes. Soil fractions are rounded to whole numbers.

## Details

`classify_texture()` applies the following validation rules and
assumptions:

- Each row must contain values for at least two of `sand_percent`,
  `silt_percent`, and `clay_percent`. Rows with fewer than two provided
  fractions must be corrected before texture classification can proceed.

- When exactly one fraction is missing, it is calculated as
  `100 - (sum of the other two)`.

- All fraction values must fall within the range 0–100. Rows with all
  three fractions must sum to 100 with a ±1 tolerance (allowable range
  99-101).

- Rows with all three fractions missing are assumed to represent samples
  where soil texture was not measured. These rows are retained and
  returned without fraction-based classification.

- If a `texture` column is provided, non-missing texture values are
  preserved and are not overwritten by fraction-based classification.
  This allows users to supply texture classes from external sources
  (e.g., NRCS gSSURGO) when particle-size fractions are unavailable.

## Examples

``` r
# Three samples classified without error

df <- data.frame(
  sand_percent = c(20, 45, 75),
  silt_percent = c(65, 35, 15),
  clay_percent = c(15, 20, 10)
)

classify_texture(df)
#> ✔ Soil texture successfully validated.
#>      texture sand_percent silt_percent clay_percent
#> 1  Silt Loam           20           65           15
#> 2       Loam           45           35           20
#> 3 Sandy Loam           75           15           10

# Error when a sample has insufficient data
# One row is missing two soil fractions

df <- data.frame(
  sand_percent = c(40, NA, 65),
  silt_percent = c(40, 55, 5),
  clay_percent = c(20, NA, 30)
)

try(classify_texture(df))
#> Error : ✖ Soil texture validation failed.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Rows with errors to correct:
#> •  Row 2 must have at least two fractions.

# Error when any fraction is outside the allowable range (0-100)

df <- data.frame(
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
#> ℹ Rows with errors to correct:
#> • Row 2 must have all fraction values between 0 and 100.
#> • Row 2 must have fractions that sum to 100 (+/- 1).

# Error when fractions do not sum to 100 +/- 1 (allowable range: 99-101)

df <- data.frame(
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
#> ℹ Rows with errors to correct:
#> • Row 3 must have fractions that sum to 100 (+/- 1).

# Warn when one fraction is missing
# The missing fraction is calculated as 100 minus the other two

df <- data.frame(
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
#> ℹ Rows with assumptions:
#> • Row 1 is missing one fraction and will be calculated as 100 minus the other
#>   two.
#>           texture sand_percent silt_percent clay_percent
#> 1      Silty Clay            5           45           50
#> 2 Sandy Clay Loam           60           10           30
#> 3       Clay Loam           25           40           35

# Warn when soil texture was not measured
# Rows with all fractions missing return a blank texture class

  df <- data.frame(
   sand_percent = c(NA, NA, 30),
   silt_percent = c(NA, NA, 30),
   clay_percent = c(NA, NA, 40)
 )

 classify_texture(df)
#> Warning: ! Soil texture validation completed with assumptions.
#> 
#> Soil fractions are provided in the columns sand_percent, clay_percent, and
#> silt_percent.
#> 
#> ℹ Rows with assumptions:
#> • Rows 1 and 2 are missing all fractions and no texture was provided, so
#>   texture will remain blank.
#>   texture sand_percent silt_percent clay_percent
#> 1    <NA>           NA           NA           NA
#> 2    <NA>           NA           NA           NA
#> 3    Clay           30           30           40
```
