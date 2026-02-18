# Extract unique values from a single column of a data frame

Extract unique values from a single column of a data frame

## Usage

``` r
pull_unique(df, target)
```

## Arguments

- df:

  A data frame containing the column of interest.

- target:

  Column to extract unique values from (string or unquoted symbol).

## Value

A vector of unique values from the specified column.

## Examples

``` r
pull_unique(washi_data, "crop")
#>  [1] "Hay/Silage"       "Green Manure"     "Vegetable"        "Herb"            
#>  [5] "Pasture, Seeded"  "Cereal Grain"     "Native Land"      "Forest"          
#>  [9] "Commercial Tree"  "Berry"            "Orchard"          "Fallow"          
#> [13] "CRP/Conservation" "Seed"             "Oilseed"         
```
