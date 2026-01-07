# Convert a `ggplot2` plot to an interactive `ggiraph`

Convert a `ggplot2` plot to an interactive `ggiraph`

## Usage

``` r
convert_ggiraph(plot, ..., body_font = "Poppins", width = 6, height = 4)
```

## Arguments

- plot:

  `ggplot2` plot to convert to interactive `ggiraph`. `plot` must
  contain `ggiraph::geom_<plot_type>_interactive()`.

- ...:

  Other arguments passed to
  [`ggiraph::girafe_options()`](https://davidgohel.github.io/ggiraph/reference/girafe_options.html).

- body_font:

  Font family to use throughout plot. Defaults to `"Poppins"`.

- width:

  Width of SVG output in inches. Defaults to 6.

- height:

  Height of SVG output in inches. Defaults to 4.

## Value

Facetted strip plots with classes of `girafe` and `htmlwidget`.

## Examples

``` r
# Read in wrangled example plot data
df_plot_path <- soils_example("df_plot.RDS")
df_plot <- readRDS(df_plot_path)

# Make strip plot with all measurements and set scales based on
# the category column and then apply theme.

# Subset df to just biological measurement group
df_plot_bio <- df_plot |>
  dplyr::filter(measurement_group == "biological")

# NOTE: the plot gets piped into the `set_scales()` function, which gets
# added to `theme_facet_strip()`.
plot <- make_strip_plot(
  df_plot_bio,
  x = dummy,
  y = value,
  id = sample_id,
  group = abbr_unit,
  tooltip = label,
  color = category,
  fill = category,
  size = category,
  alpha = category,
  shape = category
) |>
  set_scales() +
  theme_facet_strip(body_font = "sans")

# Convert static plot to interactive `ggiraph`
convert_ggiraph(plot)
#> Error: 'font_family_exists' is not an exported object from 'namespace:ggiraph'
```
