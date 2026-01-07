# Get table headers for flextable

This function uses the data dictionary to create a new dataframe of the
abbreviations and units for each measurement group for flextable

## Usage

``` r
get_table_headers(dictionary, group)
```

## Arguments

- dictionary:

  Dataframe containing columns `measurement_group`, `abbr`, `unit`.

- group:

  Character `measurement_group` value.
