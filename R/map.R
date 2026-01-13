#' Prepare data for interactive mapping
#'
#' Prepare a dataframe for use in interactive maps by creating formatted
#' label and popup columns from user-specified variables. This function is
#' intended for use prior to `make_interactive_map()`.
#'
#' @param df A data frame containing at minimum `longitude` and
#'   `latitude` columns.
#' @param label_heading A column in `df` used as the bold heading for each
#'   map feature. This value appears as the point label and as the first line of
#'   the popup.
#' @param label_body A column in `df` used as body text displayed below the
#'   heading in the popup.
#'
#' @returns A data frame with additional `label` and `popup` columns,
#'   suitable for input into `make_interactive_map()`.
#' @export
#'
#' @examples
#' washi_data |>
#'   dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
#'   dplyr::select(c(latitude, longitude, farm_name, crop)) |>
#'   head(3) |>
#'   prep_for_map(label_heading = farm_name, label_body = crop) |>
#'   dplyr::glimpse()
prep_for_map <- function(df, label_heading, label_body) {
  testthat::expect_contains(names(df), c("longitude", "latitude"))

  df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), \(x) round(x, 4)),
      label = glue::glue(
        "<strong>{eval(rlang::expr({{ label_heading }}))}</strong>"
      ),
      popup = glue::glue(
        "{label}<br>{eval(rlang::expr({{ label_body }}))}"
      )
    )
}

#' Renamed to `make_interactive_map()`
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' `make_leaflet()` was renamed to `make_interactive_map()` for a more
#' descriptive and consistent naming convention.
#'
#' @param df Dataframe containing `longitude`, `latitude`, `label`, and `popup`
#'   columns. See `prep_for_map()` for details.
#' @param primary_color A character string specifying the color used for point
#'   features on the map (hex code or color name). Defaults to WaSHI red
#'   (#a60f2d).
#'
#' @source JavaScript code adapted from
#'   [leaflet.extras](https://github.com/bhaskarvk/leaflet.extras/tree/master).
#'
#' @returns A \pkg{leaflet} map widget.
#' @keywords internal
#' @export
#'
#' @examples
#' gis_df <- washi_data |>
#'   dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
#'   dplyr::select(c(latitude, longitude, farm_name, crop)) |>
#'   head(3) |>
#'   prep_for_map(label_heading = farm_name, label_body = crop)
#'
#' dplyr::glimpse(gis_df)
#'
#' make_leaflet(gis_df)
make_leaflet <- function(
  df,
  primary_color = "#a60f2d"
) {
  lifecycle::deprecate_warn("1.0.2", "make_leaflet()", "make_interactive_map()")
  make_interactive_map(df, primary_color)
}

#' Make an interactive map of soil sample locations with `leaflet`
#'
#' Create an interactive web map with point locations and popups using
#' \pkg{leaflet}. This function is designed to work with data prepared using
#' `prep_for_map()`.
#'
#' @param df Dataframe containing `longitude`, `latitude`, `label`, and `popup`
#'   columns. See `prep_for_map()` for details.
#' @param primary_color A character string specifying the color used for point
#'   features on the map (hex code or color name). Defaults to WaSHI red
#'   (#a60f2d).
#'
#' @source JavaScript code adapted from
#'   [leaflet.extras](https://github.com/bhaskarvk/leaflet.extras/tree/master).
#'
#' @returns A \pkg{leaflet} map widget.
#'
#' @export
#'
#' @seealso `make_static_map()`
#'
#' @examples
#' gis_df <- washi_data |>
#'   dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
#'   dplyr::select(c(latitude, longitude, farm_name, crop)) |>
#'   head(3) |>
#'   prep_for_map(label_heading = farm_name, label_body = crop)
#'
#' dplyr::glimpse(gis_df)
#'
#' make_interactive_map(gis_df)
make_interactive_map <- function(df, primary_color = "#a60f2d") {
  agol <- "https://server.arcgisonline.com/ArcGIS/rest/services/"
  testthat::expect_contains(
    names(df),
    c("longitude", "latitude", "label", "popup")
  )

  leaflet::leaflet(df) |>
    leaflet::addTiles(
      urlTemplate = glue::glue(
        "{{agol}World_Imagery/MapServer/tile/{z}/{y}/{x}",
        .open = "{{"
      ),
      group = "Satellite"
    ) |>
    leaflet::addTiles(
      urlTemplate = glue::glue(
        "{{agol}/World_Topo_Map/MapServer/tile/{z}/{y}/{x}",
        .open = "{{"
      ),
      group = "Topographic"
    ) |>
    leaflet::addCircleMarkers(
      ~longitude,
      ~latitude,
      label = ~ purrr::map(label, \(x) htmltools::HTML(x)),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        style = list("font-size" = "15px"),
        direction = "auto"
      ),
      popup = ~ purrr::map(popup, \(x) htmltools::HTML(x)),
      popupOptions = leaflet::popupOptions(closeOnClick = TRUE),
      options = leaflet::markerOptions(riseOnHover = TRUE),
      radius = 10,
      color = primary_color,
      stroke = FALSE,
      fillOpacity = 0.7
    ) |>
    leaflet::addLayersControl(
      baseGroups = c("Satellite", "Topographic"),
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addEasyButton(
      leaflet::easyButton(
        icon = "ion-arrow-shrink",
        title = "Reset view",
        onClick = leaflet::JS(
          "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
        )
      )
    ) |>
    htmlwidgets::onRender(leaflet::JS(
      "
function(el, x){
  var map = this;
  map.whenReady(function(){
    map._initialCenter = map.getCenter();
    map._initialZoom = map.getZoom();
  });
}"
    ))
}


#' Make a static map of soil sample locations with `ggplot2`
#'
#' Create a static map using basemap tiles and sample point locations. This
#' function is intended as a non-interactive alternative to
#' `make_interactive_map()`.
#'
#' @param df Dataframe containing at minimum `longitude` and `latitude` columns.
#' @param label Name of the column in `df` used to label soil sample points.
#' @param provider Character string specifying the basemap tile provider, passed
#'   to `maptiles::get_tiles()`. See details of `maptiles::get_tiles()` for
#'   available providers. Defaults to "Esri.WorldImagery".
#' @param body_font Character string specifying the font family used for map
#'   labels. Defaults to "sans".
#' @inheritParams make_leaflet
#' @returns A `{ggplot2}` object representing the static map.
#' @export
#'
#' @examples
#' gis_df <- washi_data |>
#'   dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
#'   dplyr::select(c(latitude, longitude, farm_name)) |>
#'   head(3)
#'
#' dplyr::glimpse(gis_df)
#'
#' static_map <- make_static_map(gis_df, label = farm_name)
#' static_map
make_static_map <- function(
  df,
  label = field_id,
  provider = "Esri.WorldImagery",
  body_font = "sans",
  primary_color = "#a60f2d"
) {
  df_sf <- sf::st_as_sf(
    x = df,
    coords = c("longitude", "latitude"),
    crs = "+proj=lonlat"
  ) |>
    sf::st_transform(3857)

  # Use a buffered bounding box (not buffered points) when requesting tiles
  # to avoid erroneous spatial extents for tightly clustered or single-point
  # datasets, which can cause tile grid computation failures in maptiles.
  #
  # https://github.com/WA-Department-of-Agriculture/dirt-data-reports/issues/110

  make_tile_bbox <- function(sf_obj, min_buffer_m = 50, buffer_frac = 0.05) {
    bbox <- sf::st_bbox(sf_obj)

    width <- bbox["xmax"] - bbox["xmin"]
    height <- bbox["ymax"] - bbox["ymin"]

    max_dim <- max(width, height)

    buffer_m <- if (max_dim == 0) {
      min_buffer_m
    } else {
      max(min_buffer_m, max_dim * buffer_frac)
    }

    # Convert bbox to geometry and buffer it
    sf::st_as_sfc(bbox) |>
      sf::st_buffer(dist = buffer_m)
  }

  tile_geom <- make_tile_bbox(df_sf)

  # Get basetiles
  basetiles <- maptiles::get_tiles(
    tile_geom,
    provider = provider,
    cachedir = tempdir(),
    retina = TRUE
  )

  # Convert basetiles to a dataframe for ggplot2
  tiles_df <- as.data.frame(
    basetiles,
    xy = TRUE,
    na.rm = TRUE
  )

  colnames(tiles_df) <- c("x", "y", "r", "g", "b")

  map <- ggplot2::ggplot(df_sf) +
    ggplot2::geom_raster(
      data = tiles_df,
      ggplot2::aes(
        x = x,
        y = y,
        fill = grDevices::rgb(r, g, b, maxColorValue = 255)
      )
    ) +
    ggplot2::scale_fill_identity() +
    ggplot2::geom_sf(color = primary_color, size = 3) +
    ggrepel::geom_label_repel(
      ggplot2::aes(label = {{ label }}, geometry = geometry),
      stat = "sf_coordinates",
      family = body_font,
      size = 3,
      point.padding = 2,
      min.segment.length = 0
    ) +
    ggplot2::coord_sf(crs = 3857) +
    ggplot2::theme_void()

  return(map)
}
