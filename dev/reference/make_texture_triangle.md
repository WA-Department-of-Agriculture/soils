# Make a textural class triangle

Make a texture triangle with USDA textural classes.

## Usage

``` r
make_texture_triangle(
  body_font = "Poppins",
  show_names = TRUE,
  show_lines = TRUE,
  show_grid = FALSE
)
```

## Source

Adapted from plotrix:
<https://github.com/plotrix/plotrix/blob/0d4c2b065e2c2d327358ac8cdc0b0d46b89bea7f/R/soil.texture.R>

## Arguments

- body_font:

  Font family to use throughout plot. Defaults to `"Poppins"`.

- show_names:

  Boolean. Defaults to `TRUE` to show USDA textural class names.

- show_lines:

  Boolean. Defaults to `TRUE` to show boundaries of USDA textural
  classes.

- show_grid:

  Boolean. Defaults to `FALSE` to hide grid lines at each 10 level of
  each soil component.

## Value

Opens the graphics device with a triangle plot containing USDA textural
classes.

## Examples

``` r
# Note the text appears squished in this example since the width, height,
# and resolution have been optimized to print the figure 6 in wide in the
# report.

make_texture_triangle(body_font = "sans")
```
