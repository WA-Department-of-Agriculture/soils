# soils (development version)

-   Remove `field_name` as a required column and adjust templates, example data,
    and vignettes.

-   `01_producer-report.qmd` now reads in `sample_id` and `field_id` as
    character type, which fixes unwanted commas in numeric IDs (#10).

-   Data upload no longer requires a pre-existing `texture` column.

    -   Added new texture helpers: `validate_texture_fractions()`,
        `complete_texture_fractions()`, and `classify_texture()`. These validate
        sand, silt, and clay fractions, compute a missing fraction when exactly
        two are provided, and assign USDA soil texture classes when possible.

    -   Updated internal helpers and the `01_producer-report.qmd` template to
        conditionally compute texture and to synchronize the data dictionary
        when texture or fractions are added (#21).

    -   Introduced `sync_dictionary_texture()` to automatically add missing
        texture and fraction rows to the dictionary in a fixed order (`texture`,
        `sand_percent`, `silt_percent`, `clay_percent`), with support for
        English and Spanish labels.

    -   Deprecated `get_n_texture_by_var()`; its logic is now handled directly
        in `summarize_by_var()`, which treats texture as optional.

-   Fixed texture triangle rendering to require at least one complete sand,
    silt, and clay sample. Incomplete texture rows are now dropped early,
    preventing errors when producer data include partial texture information
    (#15).

-   `convert_ggiraph()` uses `gdtools::font_family_exists()` instead of
    `ggiraph::font_family_exists()` (#12).

-   Added `make_static_map()` for creating non-interactive maps using basemap
    tiles and `ggplot2` (#13). Removed dependency on `tidyterra` for static map
    rendering. Basemap tiles are now plotted using `ggplot2`, improving
    stability and reducing package dependencies.

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
