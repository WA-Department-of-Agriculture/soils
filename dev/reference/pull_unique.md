# Pull unique values from one column of dataframe

Pull unique values from one column of dataframe

## Usage

``` r
pull_unique(df, target)
```

## Arguments

- df:

  Dataframe with column to extract unique values from.

- target:

  Variable to pull unique vector of (i.e. crop or county).

## Value

Vector of unique values from target column.

## Examples

``` r
washi_data |>
  pull_unique(crop)
#>  [1] "Hay/Silage"       "Green Manure"     "Vegetable"        "Herb"            
#>  [5] "Pasture, Seeded"  "Cereal Grain"     "Native Land"      "Forest"          
#>  [9] "Commercial Tree"  "Berry"            "Orchard"          "Fallow"          
#> [13] "CRP/Conservation" "Seed"             "Oilseed"         
```
