#' Make leaflet map
#'
#' ```{r child = "man/rmd/wrangle.Rmd"}
#' ````
#'
#' @param df Dataframe containing columns: `Field ID`, `Field Name`, `Crop`,
#'   `Latitude`, `Longitdude`.
#' @param primary_color Color of points. Defaults to WaSHI red.
#'
#' @source JavaScript code adapted from
#'   [`leaflet.extras`](https://github.com/bhaskarvk/leaflet.extras/tree/master).
#'
#' @returns Leaflet map.
#'
#' @export
#'
#' @examples
#' # Just for this example: remove duplicate coordinates
#' df <- exampleData |>
#'   dplyr::distinct(latitude, longitude, .keep_all = TRUE)
#'
#' # Set up df for leaflet
#' df <- df |>
#'   subset(!duplicated(sampleId)) |>
#'   dplyr::arrange(fieldId) |>
#'   dplyr::select(c(
#'     "Sample ID" = sampleId,
#'     "Field ID" = fieldId,
#'     "Field Name" = fieldName,
#'     "Crop" = crop,
#'     "Longitude" = longitude,
#'     "Latitude" = latitude
#'   )) |>
#'   dplyr::mutate(`Field ID` = as.character(`Field ID`))
#'
#' # Glimpse data structure
#' dplyr::glimpse(df)
#'
#' # Make leaflet
#'
#' # Remember this is a dummy dataset with truncated coordinates, so many
#' # points overlap and some may be displayed in water bodies
#' make_leaflet(df)
make_leaflet <- function(
  df,
  primary_color = washi::washi_pal[["standard"]][["red"]]
    ) {
  agol <- "https://server.arcgisonline.com/ArcGIS/rest/services/"

  leaflet::leaflet(df) |>
    leaflet::addTiles(
      urlTemplate = paste0(agol, "World_Imagery/MapServer/tile/{z}/{y}/{x}"),
      group = "Satellite"
    ) |>
    leaflet::addTiles(
      urlTemplate = paste0(agol, "/World_Topo_Map/MapServer/tile/{z}/{y}/{x}"),
      group = "Topographic"
    ) |>
    leaflet::addCircleMarkers(
      ~Longitude,
      ~Latitude,
      label = ~ lapply(paste(`Field ID`), htmltools::HTML),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        style = list("font-size" = "15px"),
        direction = "auto"
      ),
      popup = ~ lapply(
        paste(
          `Field ID`,
          `Field Name`,
          Crop,
          sep = "<br>"
        ),
        htmltools::HTML
      ),
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
