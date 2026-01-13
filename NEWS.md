# soils (development version)

-   Remove `field_name` as a required column and adjust templates, example data,
    and vignettes.

-   Added `complete_texture()`, which validates and completes sand, silt, and
    clay fractions, computes a missing fraction when two are provided, and
    assigns a USDA soil texture class. A pre-existing `texture` column is no
    longer required.

-   `01_producer-report.qmd` now reads in `sample_id` and `field_id` as
    character type (#10).

-   `convert_ggiraph()` uses `gdtools::font_family_exists()` instead of
    `ggiraph::font_family_exists()` (#12).

-   Added `make_static_map()` for creating non-interactive maps using basemap
    tiles and `ggplot2` (#13). Removed dependency on `tidyterra` for static map
    rendering. Basemap tiles are now plotted using `ggplot2` with terra-backed
    rasters, improving stability and reducing package dependencies.

-   Static maps now request basemap tiles using a buffered bounding box rather
    than point geometries. This improves robustness for very small or
    single-point spatial extents and prevents tile grid errors when samples are
    tightly clustered.

-   Renamed `make_leaflet()` to `make_interactive_map()` to provide a more
    descriptive and consistent naming convention alongside `make_static_map()`.
    `make_leaflet()` will be retained for backwards compatibility.

# soils 1.0.1

-   Pre-calculate zoom for the static map to fix issue when there are less than
    4 tiles
    (<https://github.com/WA-Department-of-Agriculture/dirt-data-reports/issues/110>)
    and switch to World Imagery instead of Street Map.
