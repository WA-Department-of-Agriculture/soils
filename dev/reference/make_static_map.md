# Make a static map of soil sample locations with `ggplot2`

Create a static map using basemap tiles and sample point locations. This
function is intended as a non-interactive alternative to
[`make_interactive_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_interactive_map.md).

## Usage

``` r
make_static_map(
  df,
  label = field_id,
  provider = "Esri.WorldImagery",
  body_font = "sans",
  primary_color = "#a60f2d"
)
```

## Arguments

- df:

  Dataframe containing at minimum `longitude` and `latitude` columns.

- label:

  Name of the column in `df` used to label soil sample points.

- provider:

  Character string specifying the basemap tile provider, passed to
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html).
  See details of
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html)
  for available providers. Defaults to "Esri.WorldImagery".

- body_font:

  Character string specifying the font family used for map labels.
  Defaults to "sans".

- primary_color:

  A character string specifying the color used for point features on the
  map (hex code or color name). Defaults to WaSHI red (#a60f2d).

## Value

A `{ggplot2}` object representing the static map.

## Examples

``` r
gis_df <- washi_data |>
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
  dplyr::select(c(latitude, longitude, farm_name)) |>
  head(3)

dplyr::glimpse(gis_df)
#> Rows: 3
#> Columns: 3
#> $ latitude  <int> 49, 47, 47
#> $ longitude <int> -119, -123, -122
#> $ farm_name <chr> "Farm 150", "Farm 085", "Farm 058"

static_map <- make_static_map(gis_df, label = farm_name)
static_map
```
