#' Make leaflet map
#'
#' Reset view code from https://github.com/bhaskarvk/leaflet.extras/blob/master/R/mapUtils.R
#'
#' @param df Dataframe that contains sample info (sampleid, fieldid, crop, coordinates).
#' @param primary_color Color of points. Defaults to WaSHI red.
#'
#' @returns Leaflet map.
#'
#' @export
#'

map_samples <- function(
    df,
    primary_color = washi::washi_pal[["standard"]][["red"]]) {
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
    leaflet::addCircleMarkers(~Longitude, ~Latitude,
      label = ~ lapply(paste(`Field ID`), htmltools::HTML),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        style = list("font-size" = "15px"),
        direction = "auto"
      ),
      popup = ~ lapply(
        paste(
          `Field ID`, `Field Name`, Crop,
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
