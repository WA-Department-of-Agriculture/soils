# Setup --------------------------------------------------------------

# EDIT: You will need to add your own cleaned lab data to this data
# folder, using 'exampleData.csv' as a template.
#
# 'dataDictionary.csv' must also be updated to match your own
# data set.

# Load lab results
data <- read.csv(
  paste0(here::here(), "/inst/extdata/exampleData.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
)

# Load data dictionary for pretty labels
dictionary <- read.csv(
  paste0(here::here(), "/inst/extdata/dataDictionary.csv"),
  check.names = FALSE,
  # setting encoding is important for using subscripts and superscripts
  encoding = "UTF-8"
)

# Style variables --------------------------------------------------------

# EDIT: Replace any font names and colors to match your branding.

header_font <- "Lato"
body_font <- "Poppins"

# Flextable header background color
header_color <- "#023B2C"
# Flextable header text color
header_text_color <- "white"
# Flextable body darker background color
darker_color <- "#ccc29c"
# Flextable body lighter background color
lighter_color <- "#F2F0E6"
# Flextable border color
border_color <- "#3E3D3D"
# Plot point color for producer samples
primary_color <- "#a60f2d"
# Plot point color for samples in same categories as producer
secondary_color <- "#3E3D3D"
# Plot point color for all other samples in project
other_color <- "#ccc29c"
# Plot facet strip background color
strip_color <- "#335c67"
# Plot facet strip text color
strip_text_color <- "white"

# Tidy longer -----------------------------------------------------

# Tidy data into long format and join with data dictionary
results_long <- data |>
  dplyr::mutate(dplyr::across(dplyr::contains("_"), as.numeric)) |>
  tidyr::pivot_longer(
    cols = dplyr::matches("_|pH"),
    names_to = "measurement"
  ) |>
  dplyr::inner_join(dictionary, by = c("measurement" = "column_name")) |>
  dplyr::arrange(measurement_group, order) |>
  dplyr::mutate(
    abbr = factor(
      abbr,
      levels = dictionary$abbr,
      ordered = is.ordered(dictionary$order)
    ),
    abbr_unit = factor(
      abbr_unit,
      levels = dictionary$abbr_unit,
      ordered = is.ordered(dictionary$order)
    )
  ) |>
  dplyr::filter(!is.na(value))

# Producer info --------------------------------------------------------

# If field name is blank, use field ID
data$fieldName <- ifelse(is.na(data$fieldName), data$fieldId, data$fieldName)

# If farm name is blank, use producer ID
data$farmName <- ifelse(is.na(data$farmName), data$producerId, data$farmName)

# Subset to producer samples
producer_samples <- results_long |>
  dplyr::filter(producerId == pid & year == yr)

# Extract producer sample IDs, crops, counties, and farm name into
# producer list
producer <- list("sampleId", "crop", "county", "farmName", "measurement") |>
  rlang::set_names() |>
  purrr::map(\(x) pull_unique(
    df = producer_samples,
    target = x
  ))

# In case of multiple farm names, grab first one
producer$farmName <- producer$farmName[[1]]

# Remove measurements that producer did not have tested
results_long <- results_long |>
  dplyr::filter(measurement %in% producer_samples$measurement)

# Prep data for GIS table and map
gis_df <- prep_for_map(
  producer_samples,
  label_heading = fieldName,
  label_body = crop
)

# Map ------------------------------------------------------------------

map <- make_leaflet(gis_df)

# Tables ---------------------------------------------------------------

# Make GIS table
table_gis <- gis_df |>
  dplyr::select(
    `Sample ID` = sampleId,
    `Field ID` = fieldId,
    `Field Name` = fieldName,
    Crop = crop,
    Longitude = longitude,
    Latitude = latitude
  ) |>
  flextable::flextable() |>
  style_ft() |>
  flextable::set_table_properties(layout = "autofit")

## Calculate averages  -----------------------------------------------------

crop_summary <- summarize_by_var(
  results_long,
  producer_samples,
  var = crop
)

county_summary <- summarize_by_var(
  results_long,
  producer_samples,
  var = county
)

project_summary <- summarize_by_project(results_long)

## Combine producer table with summaries --------------------------------------

producer_table <- producer_samples |>
  dplyr::select(
    measurement_group,
    abbr,
    value,
    "Field or Average" = fieldName,
    Texture = texture
  )

## Bind together into one df and round values to 2 digits
df_table <- dplyr::bind_rows(
  producer_table,
  crop_summary,
  county_summary,
  project_summary
) |>
  dplyr::mutate(
    value = as.numeric(formatC(value, 2, drop0trailing = TRUE))
  )

## Create list of dfs for measurement groups ----------------------------------

# Split into list with each measurement group as its own df and pivot wider
groups <- df_table |>
  split(df_table$measurement_group) |>
  purrr::map(\(x) {
    tidyr::pivot_wider(
      x,
      id_cols = c("Field or Average", Texture),
      names_from = abbr
    )
  })

## Special wrangling for texture -----------------------------------------

# Extract physical df from averages list
physical <- list(physical = groups$physical)

# Remove texture from all dataframes except physical
groups <- purrr::map(
  subset(
    groups,
    !(names(groups) == "physical")
  ),
  \(x) dplyr::select(x, -Texture)
)

# Add physical df back to the averages list
groups <- c(groups, physical)

# Remove this intermediate variable from env
rm(physical)

## Final tweaks ------------------------------------------------

# Delete any county or crop averages where n = 1
tables <- groups |>
  purrr::map(
    subset,
    !grepl("(1 Fields)", `Field or Average`)
  )

# Map function to each measurement group, resulting in a new df with
# abbreviations and units in a list
headers <- results_long |>
  pull_unique(target = measurement_group) |>
  as.list() |>
  rlang::set_names() |>
  purrr::map(\(group) get_table_headers(dictionary, group))

## Make flextables ------------------------------------------------

table_list <- list2DF(
  list(
    table = tables,
    header = headers
  )
) |>
  purrr::pmap(\(table, header) {
    make_ft(table, header) |>
      format_ft_colors(
        lighter_color = lighter_color,
        darker_color = darker_color
      ) |>
      style_ft(
        header_font = header_font,
        body_font = body_font,
        header_color = header_color,
        header_text_color = header_text_color,
        border_color = border_color
      ) |>
      unit_hline(header = header) |>
      flextable::set_table_properties(layout = "autofit")
  })

# Plots -------------------------------------------------------------

## Prep data --------------------------------------------------------

df_plot <- results_long |>
  dplyr::mutate(
    # Dummy column to set x-axis in same place for each facet
    dummy = "dummy",
    # Set category to group samples
    category = dplyr::case_when(
      sampleId %in% producer$sampleId ~ "Your fields",
      crop %in% producer$crop ~ "Same crop",
      county %in% producer$county ~ "Same county",
      .default = "Other fields"
    ),
    # Set category factors so producer samples are plotted last
    category = factor(
      category,
      levels = c("Your fields", "Same crop", "Same county", "Other fields")
    ),
    # Label for tooltip
    label = dplyr::case_when(
      category == "Your fields" ~ glue::glue(
        "{fieldName}<br>{crop}<br>{value} {unit}"
      ),
      .default = glue::glue(
        "{county}<br>",
        "{crop}<br>",
        "{value} {unit}"
      )
    )
  )

# Order the df so producer's points are plotted on top

df_plot <- df_plot[order(df_plot$category, decreasing = TRUE), ]

## Make plots -----------------------------------------------------------

# Texture triangle
triangle_df <- df_plot |>
  subset(abbr %in% c("Sand", "Silt", "Clay")) |>
  tidyr::pivot_wider(id_cols = c(sampleId, category), names_from = measurement)

texture_triangle <- make_texture_triangle(body_font = body_font) +
  add_texture_points(
    triangle_df,
    sand = `sand_%`,
    silt = `silt_%`,
    clay = `clay_%`,
    color = category,
    size = category,
    shape = category,
    alpha = category
  )

texture_triangle <- set_scales(
  texture_triangle,
  primary_color = primary_color,
  secondary_color = secondary_color,
  other_color = other_color
)

# Strip plots

plot_list <- df_plot |>
  split(df_plot$measurement_group) |>
  purrr::map(\(group) {
    plot_name <- group |>
      pull_unique(measurement_group)

    plot <- make_strip_plot(
      group,
      color = category,
      size = category,
      shape = category,
      alpha = category
    ) |>
      set_scales(
        primary_color = primary_color,
        secondary_color = secondary_color,
        other_color = other_color
      ) +
      theme_facet_strip(
        body_font = body_font,
        strip_color = strip_color,
        strip_text_color = strip_text_color
      )
  })
