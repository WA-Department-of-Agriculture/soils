# Assign USDA soil texture class

Internal helper that assigns a USDA soil texture class based on
completed sand, silt, and clay percentages.

## Usage

``` r
assign_texture_class(df)
```

## Source

Thresholds for texture classification are from the USDA NRCS Soil
Texture Calculator found at
<https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator>.

## Arguments

- df:

  A data frame containing completed `sand_percent`, `silt_percent`, and
  `clay_percent`.

## Value

A data frame with an added `texture` column.
