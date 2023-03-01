#' make leaflet map
#' @description reset view code from https://github.com/bhaskarvk/leaflet.extras/blob/master/R/mapUtils.R
#' @param data dataframe that contains sample info (sampleid, fieldid, crop, coordinates)
#' @param primary_color color of points
#'
#' @export
#'

map_samples <- function(data, primary_color) {
  box::use(
    leaflet[
      leaflet, addProviderTiles, addCircleMarkers,
      markerOptions, labelOptions, popupOptions,
      addLayersControl, layersControlOptions, addEasyButton,
      easyButton, JS
    ],
    htmltools[HTML],
    htmlwidgets[onRender]
  )

  leaflet(data) |>
    addProviderTiles("Esri.WorldTopoMap", group = "Topographic") |>
    addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    addCircleMarkers(~Longitude, ~Latitude,
      label = ~ lapply(paste(`Field ID`), HTML),
      labelOptions = labelOptions(
        noHide = TRUE,
        style = list("font-size" = "15px"),
        direction = "auto"
      ),
      popup = ~ lapply(paste(`Field ID`, `Field Name`, Crop, sep = "<br>"), HTML),
      popupOptions = popupOptions(closeOnClick = TRUE),
      options = markerOptions(riseOnHover = TRUE),
      radius = 10,
      color = primary_color,
      stroke = FALSE,
      fillOpacity = 0.7
    ) |>
    addLayersControl(
      baseGroups = c("Topographic", "Satellite"),
      options = layersControlOptions(collapsed = FALSE)
    ) |>
    addEasyButton(
      easyButton(
        icon = "ion-arrow-shrink",
        title = "Reset view",
        onClick = JS(
          "function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }"
        )
      )
    ) |>
    onRender(JS(
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
