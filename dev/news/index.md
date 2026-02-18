# Changelog

## soils (development version)

- Remove `field_name` as a required column and adjust templates, example
  data, and vignettes.

- `01_producer-report.qmd` now reads in `sample_id` and `field_id` as
  character type, which fixes unwanted commas in numeric IDs
  ([\#10](https://github.com/WA-Department-of-Agriculture/soils/issues/10)).

- Replaced internal `testthat` checks for missing required columns with
  a custom cli-based helper, `abort_if_missing_cols()`, to provide
  clearer, user-facing error messages
  ([\#14](https://github.com/WA-Department-of-Agriculture/soils/issues/14)).

- **Texture updates**:

  - Data upload no longer requires a pre-existing `texture` column.

    - Added new texture helpers:
      [`validate_texture_fractions()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/validate_texture_fractions.md),
      [`complete_texture_fractions()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/complete_texture_fractions.md),
      and
      [`classify_texture()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/classify_texture.md).
      These validate sand, silt, and clay fractions, compute a missing
      fraction when exactly two are provided, and assign USDA soil
      texture classes when possible.

    - Updated internal helpers and the `01_producer-report.qmd` template
      to conditionally compute texture and to synchronize the data
      dictionary when texture or fractions are added
      ([\#21](https://github.com/WA-Department-of-Agriculture/soils/issues/21)).

    - Introduced
      [`sync_dictionary_texture()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/sync_dictionary_texture.md)
      to automatically add missing texture and fraction rows to the
      dictionary in a fixed order (`texture`, `sand_percent`,
      `silt_percent`, `clay_percent`), with support for English and
      Spanish labels.

    - Deprecated
      [`get_n_texture_by_var()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/get_n_texture_by_var.md);
      its logic is now handled directly in
      [`summarize_by_var()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/summarize_by_var.md),
      which treats texture as optional.

  - Fixed texture triangle rendering to require at least one complete
    sand, silt, and clay sample. Incomplete texture rows are now dropped
    early, preventing errors when producer data include partial texture
    information
    ([\#15](https://github.com/WA-Department-of-Agriculture/soils/issues/15)).

- **Plot and map updates**:

  - [`convert_ggiraph()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/convert_ggiraph.md)
    uses
    [`gdtools::font_family_exists()`](https://davidgohel.github.io/gdtools/reference/font_family_exists.html)
    instead of `ggiraph::font_family_exists()`, which is no longer
    exported from `ggiraph`
    ([\#12](https://github.com/WA-Department-of-Agriculture/soils/issues/12)).

  - Added
    [`make_static_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_static_map.md)
    for creating non-interactive maps using basemap tiles and `ggplot2`
    ([\#13](https://github.com/WA-Department-of-Agriculture/soils/issues/13)).
    Removed dependency on `tidyterra` for static map rendering. Basemap
    tiles are now plotted using `ggplot2`, improving stability and
    reducing package dependencies.

  - Static maps now request basemap tiles using a buffered bounding box
    rather than point geometries. This improves robustness for very
    small or single-point spatial extents and prevents tile grid errors
    when samples are tightly clustered.

  - Renamed
    [`make_leaflet()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_leaflet.md)
    to
    [`make_interactive_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_interactive_map.md)
    to provide a more descriptive and consistent naming convention
    alongside
    [`make_static_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_static_map.md).
    [`make_leaflet()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_leaflet.md)
    will be retained for backwards compatibility.

- **Measurement handling updates**:

  - Measurements must be quantitative. Modified template to dynamically
    select measurement columns (based on provided vector of metadata
    columns)
    ([\#24](https://github.com/WA-Department-of-Agriculture/soils/issues/24))
    and coerce measurement columns to numeric with new function
    `coerce_to_numeric()`
    ([\#11](https://github.com/WA-Department-of-Agriculture/soils/issues/11)
    and
    [\#26](https://github.com/WA-Department-of-Agriculture/soils/issues/26)).

  - Added a footnote to all tables with new function
    [`add_field_count_note()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/add_field_count_note.md)
    to clarify that field counts reflect the total samples contributing
    to each average, which may be fewer than the total samples for some
    measurements now that qualitative measurements are coerced to
    numeric.

## soils 1.0.1

- Pre-calculate zoom for the static map to fix issue when there are less
  than 4 tiles
  (<https://github.com/WA-Department-of-Agriculture/dirt-data-reports/issues/110>)
  and switch to World Imagery instead of Street Map.
