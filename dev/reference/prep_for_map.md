# Prep data to gis df

Prep data to gis df

## Usage

``` r
prep_for_map(df, label_heading, label_body)
```

## Arguments

- df:

  Dataframe containing columns: `longitude`, `latitude`, and two columns
  with values you want to appear in the map label and popup.

- label_heading:

  Column in `df` that you want to appear as the bold point label on your
  map, as well as the first line of the popup when the user clicks a
  point.

- label_body:

  Column in `df` that you want to appear as body text below the
  `label_heading` in the popup.

## Value

Dataframe to be input into
[`make_leaflet()`](https://wa-department-of-agriculture.github.io/soils/dev/reference/make_leaflet.md).

## Examples

``` r
washi_data |>
  dplyr::distinct(latitude, longitude, .keep_all = TRUE) |>
  head(3) |>
  prep_for_map(label_heading = field_id, label_body = crop) |>
  dplyr::glimpse()
#> Rows: 3
#> Columns: 44
#> $ year                   <dbl> 2023, 2022, 2022
#> $ sample_id              <chr> "23-WUY05-01", "22-RHM05-02", "22-ENR07-02"
#> $ farm_name              <chr> "Farm 150", "Farm 085", "Farm 058"
#> $ producer_id            <chr> "WUY05", "RHM05", "ENR07"
#> $ field_name             <chr> "Field 01", "Field 02", "Field 02"
#> $ field_id               <dbl> 1, 2, 2
#> $ county                 <chr> "County 9", "County 18", "County 11"
#> $ crop                   <chr> "Hay/Silage", "Green Manure", "Vegetable"
#> $ longitude              <dbl> -119, -123, -122
#> $ latitude               <dbl> 49, 47, 47
#> $ texture                <chr> "Clay Loam", "Sandy Loam", "Silt Loam"
#> $ bd_g_cm3               <dbl> 1.30, 0.88, 1.21
#> $ pmn_lb_ac              <dbl> 67.13, 129.97, 122.17
#> $ nh4_n_mg_kg            <dbl> 1.6, 21.6, 8.1
#> $ no3_n_mg_kg            <dbl> 9.2, 6.1, 25.3
#> $ poxc_mg_kg             <dbl> 496, 571, 419
#> $ ph                     <dbl> 6.7, 5.9, 6.3
#> $ ec_mmhos_cm            <dbl> 0.42, 0.05, 0.60
#> $ k_mg_kg                <dbl> 498, 198, 294
#> $ ca_mg_kg               <dbl> 1380, 780, 1760
#> $ mg_mg_kg               <dbl> 145.2, 96.8, 266.2
#> $ na_mg_kg               <dbl> 16.1, 20.7, 20.7
#> $ cec_meq_100g           <dbl> 7.8, 10.5, 13.0
#> $ b_mg_kg                <dbl> 0.22, 0.09, 0.41
#> $ cu_mg_kg               <dbl> 0.6, 0.4, 4.2
#> $ fe_mg_kg               <dbl> 26, 28, 141
#> $ mn_mg_kg               <dbl> 1.5, 2.7, 4.1
#> $ s_mg_kg                <dbl> 4.29, 9.41, 26.73
#> $ zn_mg_kg               <dbl> 1.7, 0.8, 4.2
#> $ total_c_percent        <dbl> 1.85, 2.88, 1.68
#> $ total_n_percent        <dbl> 0.16, 0.18, 0.14
#> $ ace_g_protein_kg_soil  <dbl> 6.74, 21.50, 10.90
#> $ sand_percent           <dbl> 44, 69, 11
#> $ silt_percent           <dbl> 23, 21, 79
#> $ clay_percent           <dbl> 33, 10, 10
#> $ min_c_96hr_mg_c_kg_day <dbl> 35.6, 30.0, 15.0
#> $ p_olsen_mg_kg          <dbl> 15, 37, 73
#> $ wsa_percent            <dbl> 88.5, 92.6, 91.3
#> $ om_percent             <dbl> 4.5, 5.8, 2.4
#> $ toc_percent            <dbl> 1.85, 2.88, 1.68
#> $ whc_in_ft              <dbl> 1.01, 1.08, 2.77
#> $ inorganic_c_percent    <dbl> NA, NA, NA
#> $ label                  <glue> "<strong>1</strong>", "<strong>2</strong>", "<s…
#> $ popup                  <glue> "<strong>1</strong><br>Hay/Silage", "<strong>2<…
```
