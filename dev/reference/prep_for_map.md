# Prepare data for interactive mapping

Prepare a dataframe for use in interactive maps by creating formatted
label and popup columns from user-specified variables. This function is
intended for use prior to
[`make_interactive_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_interactive_map.md).

## Usage

``` r
prep_for_map(df, label_heading, label_body)
```

## Arguments

- df:

  A data frame containing at minimum `longitude` and `latitude` columns.

- label_heading:

  A column in `df` used as the bold heading for each map feature. This
  value appears as the point label and as the first line of the popup.

- label_body:

  A column in `df` used as body text displayed below the heading in the
  popup.

## Value

A data frame with additional `label` and `popup` columns, suitable for
input into
[`make_interactive_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_interactive_map.md).

## Examples

``` r
washi_data |>
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
  dplyr::select(c(latitude, longitude, farm_name, crop)) |>
  head(3) |>
  prep_for_map(label_heading = farm_name, label_body = crop) |>
  dplyr::glimpse()
#> Rows: 3
#> Columns: 6
#> $ latitude  <dbl> 49, 47, 47
#> $ longitude <dbl> -119, -123, -122
#> $ farm_name <chr> "Farm 150", "Farm 085", "Farm 058"
#> $ crop      <chr> "Hay/Silage", "Green Manure", "Vegetable"
#> $ label     <glue> "<strong>Farm 150</strong>", "<strong>Farm 085</strong>", "<…
#> $ popup     <glue> "<strong>Farm 150</strong><br>Hay/Silage", "<strong>Farm 085…
```
