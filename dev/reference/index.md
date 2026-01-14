# Package index

## Create a {soils} project

- [`create_soils()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/create_soils.md)
  : Create a project directory for generating soil health reports

## Data wrangling

- [`classify_texture()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/classify_texture.md)
  : Classify USDA soil texture from particle-size fractions
- [`get_n_texture_by_var()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/get_n_texture_by_var.md)
  : Calculate n samples and most frequent texture by a grouping variable
- [`summarize_by_project()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/summarize_by_project.md)
  : Summarize samples across the project
- [`summarize_by_var()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/summarize_by_var.md)
  : Summarize producer's samples with averages by grouping variable
- [`get_table_headers()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/get_table_headers.md)
  : Get table headers for flextable

## Map

- [`prep_for_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/prep_for_map.md)
  : Prepare data for interactive mapping

- [`make_interactive_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_interactive_map.md)
  :

  Make an interactive map of soil sample locations with `leaflet`

- [`make_static_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_static_map.md)
  :

  Make a static map of soil sample locations with `ggplot2`

## Plots

- [`make_texture_triangle()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_texture_triangle.md)
  : Make a textural class triangle

- [`add_texture_points()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/add_texture_points.md)
  : Add points to texture triangle

- [`add_legend()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/add_legend.md)
  : Add a legend to the texture triangle

- [`make_strip_plot()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_strip_plot.md)
  : Make a facetted strip plot

- [`theme_facet_strip()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/theme_facet_strip.md)
  : Theme for facetted strip plots

- [`set_scales()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/set_scales.md)
  : Define styles for producer's samples versus all samples

- [`convert_ggiraph()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/convert_ggiraph.md)
  :

  Convert a `ggplot2` plot to an interactive `ggiraph`

## Tables

- [`make_ft()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_ft.md)
  : Make a flextable with column names from another dataframe
- [`unit_hline()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/unit_hline.md)
  : Add bottom border to specific columns in flextable
- [`format_ft_colors()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/format_ft_colors.md)
  : Conditional formatting of flextable background cell colors
- [`style_ft()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/style_ft.md)
  : Style a flextable

## Helpers

- [`is_column_empty()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/is_column_empty.md)
  : Check if a column is empty (all NA or blank)
- [`has_complete_row()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/has_complete_row.md)
  : Check for at least one complete row for required columns
- [`calculate_mode()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/calculate_mode.md)
  : Calculate the mode of categorical variable
- [`pull_unique()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/pull_unique.md)
  : Pull unique values from one column of dataframe

## Example data

- [`soils_example()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/soils_example.md)
  : Get path to example data
- [`washi_data`](https://wa-department-of-agriculture.github.io/soils/dev/reference/washi_data.md)
  : Example WaSHI data
- [`data_dictionary`](https://wa-department-of-agriculture.github.io/soils/dev/reference/data_dictionary.md)
  : Data dictionary
