# Changelog

## soils (development version)

- Remove `field_name` as a required column and adjust templates, example
  data, and vignettes.

- Added `complete_texture()`, which validates and completes sand, silt,
  and clay fractions, computes a missing fraction when two are provided,
  and assigns a USDA soil texture class. A pre-existing `texture` column
  is no longer required.

- `01_producer-report.qmd` now reads in `sample_id` and `field_id` as
  character type
  ([\#10](https://github.com/WA-Department-of-Agriculture/soils/issues/10)).

- [`convert_ggiraph()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/convert_ggiraph.md)
  uses
  [`gdtools::font_family_exists()`](https://davidgohel.github.io/gdtools/reference/font_family_exists.html)
  instead of `ggiraph::font_family_exists()`
  ([\#12](https://github.com/WA-Department-of-Agriculture/soils/issues/12)).

## soils 1.0.1

- Pre-calculate zoom for the static map to fix issue when there are less
  than 4 tiles
  (<https://github.com/WA-Department-of-Agriculture/dirt-data-reports/issues/110>)
  and switch to World Imagery instead of Street Map.
