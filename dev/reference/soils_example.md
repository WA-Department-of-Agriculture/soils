# Get path to example data

`soils` comes bundled with some example files in its `inst/extdata`
directory. This function make them easy to access.

## Usage

``` r
soils_example(file = NULL)
```

## Source

Adapted from `readxl::readxl_example()`.

## Arguments

- file:

  Name of file. If `NULL`, the example files will be listed.

## Examples

``` r
soils_example()
#> [1] "df_plot.RDS" "headers.RDS" "tables.RDS" 

soils_example("df_plot.RDS")
#> [1] "/home/runner/work/_temp/Library/soils/extdata/df_plot.RDS"
```
