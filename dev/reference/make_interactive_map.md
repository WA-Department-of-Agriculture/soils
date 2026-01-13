# Make an interactive map of soil sample locations with `leaflet`

Create an interactive web map with point locations and popups using
leaflet. This function is designed to work with data prepared using
[`prep_for_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/prep_for_map.md).

## Usage

``` r
make_interactive_map(df, primary_color = "#a60f2d")
```

## Source

JavaScript code adapted from
[leaflet.extras](https://github.com/bhaskarvk/leaflet.extras/tree/master).

## Arguments

- df:

  Dataframe containing `longitude`, `latitude`, `label`, and `popup`
  columns. See
  [`prep_for_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/prep_for_map.md)
  for details.

- primary_color:

  A character string specifying the color used for point features on the
  map (hex code or color name). Defaults to WaSHI red (#a60f2d).

## Value

A leaflet map widget.

## See also

[`make_static_map()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_static_map.md)

## Examples

``` r
gis_df <- washi_data |>
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
  dplyr::select(c(latitude, longitude, farm_name, crop)) |>
  head(3) |>
  prep_for_map(label_heading = farm_name, label_body = crop)

dplyr::glimpse(gis_df)
#> Rows: 3
#> Columns: 6
#> $ latitude  <dbl> 49, 47, 47
#> $ longitude <dbl> -119, -123, -122
#> $ farm_name <chr> "Farm 150", "Farm 085", "Farm 058"
#> $ crop      <chr> "Hay/Silage", "Green Manure", "Vegetable"
#> $ label     <glue> "<strong>Farm 150</strong>", "<strong>Farm 085</strong>", "<…
#> $ popup     <glue> "<strong>Farm 150</strong><br>Hay/Silage", "<strong>Farm 085…

make_interactive_map(gis_df)

{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",null,"Satellite",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]},{"method":"addTiles","args":["https://server.arcgisonline.com/ArcGIS/rest/services//World_Topo_Map/MapServer/tile/{z}/{y}/{x}",null,"Topographic",{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false}]},{"method":"addCircleMarkers","args":[[49,47,47],[-119,-123,-122],10,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":true,"riseOffset":250,"stroke":false,"color":"#a60f2d","weight":5,"opacity.1":0.5,"fill":true,"fillColor":"#a60f2d","fillOpacity":0.7},null,null,["<strong>Farm 150<\/strong><br>Hay/Silage","<strong>Farm 085<\/strong><br>Green Manure","<strong>Farm 058<\/strong><br>Vegetable"],{"maxWidth":300,"minWidth":50,"autoPan":true,"keepInView":false,"closeButton":true,"closeOnClick":true,"className":""},["<strong>Farm 150<\/strong>","<strong>Farm 085<\/strong>","<strong>Farm 058<\/strong>"],{"interactive":false,"permanent":true,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"style":{"font-size":"15px"},"className":"","sticky":true},null]},{"method":"addLayersControl","args":[["Satellite","Topographic"],[],{"collapsed":false,"autoZIndex":true,"position":"topright"}]},{"method":"addEasyButton","args":[{"icon":"ion-arrow-shrink","title":"Reset view","onClick":"function(btn, map){ map.setView(map._initialCenter, map._initialZoom); }","position":"topleft"}]}],"limits":{"lat":[47,49],"lng":[-123,-119]}},"evals":["calls.4.args.0.onClick"],"jsHooks":{"render":[{"code":"function(el, x, data) {\n  return (\nfunction(el, x){\n  var map = this;\n  map.whenReady(function(){\n    map._initialCenter = map.getCenter();\n    map._initialZoom = map.getZoom();\n  });\n}).call(this.getMap(), el, x, data);\n}","data":null}]}}
```
