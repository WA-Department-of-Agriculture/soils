# Synchronize dictionary with texture and fractions added by classify_texture()

Adds missing `texture` and soil particle-size fraction columns to the
dictionary in a fixed order for the physical measurement group. Intended
for internal use but exported so it can be called in templates.

## Usage

``` r
sync_dictionary_texture(data, dictionary, language = "English")
```

## Arguments

- data:

  Data frame potentially containing `texture`, `sand_percent`,
  `silt_percent`, and `clay_percent`.

- dictionary:

  Data frame with columns `measurement_group`, `column_name`, `abbr`,
  and `unit`.

- language:

  Either `"English"` or `"Spanish"`. Default `"English"`.

## Value

Updated dictionary with any missing texture/fraction rows inserted in
the order: `texture`, `sand_percent`, `silt_percent`, `clay_percent`.
Returns the original dictionary if no rows were added.
