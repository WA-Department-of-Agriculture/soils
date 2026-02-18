# Get table headers for flextable

Internal helper that uses the data dictionary to construct flextable
column headers for a single measurement group.

## Usage

``` r
get_table_headers(dictionary, group)
```

## Arguments

- dictionary:

  Data frame containing columns `measurement_group`, `abbr`, `unit`.

- group:

  Character `measurement_group` value.
