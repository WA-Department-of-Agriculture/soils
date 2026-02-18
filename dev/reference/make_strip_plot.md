# Make a facetted strip plot

Make a facetted strip plot

## Usage

``` r
make_strip_plot(
  df,
  ...,
  x = dummy,
  y = value,
  id = sample_id,
  group = abbr_unit,
  tooltip = label,
  language = "English"
)
```

## Arguments

- df:

  Data frame to plot.

- ...:

  Other arguments passed to
  [`graphics::points()`](https://rdrr.io/r/graphics/points.html).

- x:

  Column for x-axis. For these strip plots, we recommend using a dummy
  variable to act as a placeholder. Defaults to a column named `dummy`
  with only one value ("dummy") for all rows.

- y:

  Column for y-axis. Defaults to `value`.

- id:

  Column with unique identifiers for each sample to use as `data_id` for
  interactive plots. Defaults to `sample_id`.

- group:

  Column to facet by. Defaults to `abbr_unit`.

- tooltip:

  Column with tooltip labels for interactive plots.

- language:

  Language of the legend. `"English"` (default) or `"Spanish"`.

## Value

Facetted `ggplot2` strip plots.

## Examples

``` r
# Read in wrangled example plot data
df_plot_path <- soils_example("df_plot.RDS")
df_plot <- readRDS(df_plot_path)

# Subset df to just biological measurement group
df_plot_bio <- df_plot |>
  dplyr::filter(measurement_group == "Biological")

# Make strip plot with all measurements and set scales based on
# the category column and then apply theme.

# NOTE: the plot gets piped into the `set_scales()` function, which gets
# added to `theme_facet_strip()`.

make_strip_plot(
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


# Example of strip plot without scales or theme functions
make_strip_plot(df_plot_bio)


# Example of strip plot with `x` set to the facet group instead of a dummy
# variable. The dummy variable is what centers the points within the subplot.
make_strip_plot(
  df_plot_bio,
  x = abbr_unit,
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
```
