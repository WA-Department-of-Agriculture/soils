# Calculate the mode of a categorical variable

Returns the most frequently occurring value in a character vector,
ignoring missing values.

## Usage

``` r
calculate_mode(x)
```

## Arguments

- x:

  Character vector to calculate the mode from.

## Value

A single value (character) that occurs most often in `x`.

## Examples

``` r
calculate_mode(washi_data$crop)
#> [1] "Vegetable"
```
