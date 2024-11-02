# data_dictionary ==============================================================
data_dictionary <- read.csv(
  here::here("inst/templates/english/data/data-dictionary.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
  )

usethis::use_data(data_dictionary, overwrite = TRUE)

# washi_data ===================================================================
washi_data <- read.csv(
  here::here("inst/templates/english/data/washi-data.csv"),
  check.names = FALSE)

usethis::use_data(washi_data, overwrite = TRUE)

# df_plot ======================================================================

# get producer info
producer <- washi_data |>
  dplyr::filter(producer_id == "WUY05")

# Tidy data into long format and join with data dictionary
results_long <- washi_data |>
  dplyr::mutate(dplyr::across(12:42, as.numeric)) |>
  tidyr::pivot_longer(
    cols = 12:42,
    names_to = "measurement"
  ) |>
  dplyr::inner_join(data_dictionary, by = c("measurement" = "column_name")) |>
  dplyr::arrange(measurement_group, order) |>
  dplyr::mutate(
    abbr = factor(
      abbr,
      levels = data_dictionary$abbr,
      ordered = is.ordered(data_dictionary$order)
    ),
    abbr_unit = factor(
      abbr_unit,
      levels = data_dictionary$abbr_unit,
      ordered = is.ordered(data_dictionary$order)
    )
  ) |>
  dplyr::filter(!is.na(value))

df_plot <- results_long |>
  dplyr::mutate(
    # Dummy column to set x-axis in same place for each facet
    dummy = "dummy",
    # Set category to group samples
    category = dplyr::case_when(
      sample_id %in% producer$sample_id ~ "Your fields",
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
        "{field_name}<br>{crop}<br>{value} {unit}"
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

saveRDS(df_plot, here::here("inst/extdata/df_plot.RDS"))

# tables =======================================================================

producer_samples <- results_long |>
  dplyr::filter(producer_id == "WUY05" & year == 2023)

# Calculate averages by crop, county, and project
crop_summary <- soils::summarize_by_var(
  results_long,
  producer_samples,
  var = crop
)

county_summary <- soils::summarize_by_var(
  results_long,
  producer_samples,
  var = county
)

project_summary <- soils::summarize_by_project(results_long)

producer_table <- producer_samples |>
  dplyr::select(
    measurement_group,
    abbr,
    value,
    "Field or Average" = field_name,
    Texture = texture
  )

# Bind together into one df and round values to 2 digits
df_table <- dplyr::bind_rows(
  producer_table,
  crop_summary,
  county_summary,
  project_summary
) |>
  dplyr::mutate(
    value = as.numeric(formatC(value, 2, drop0trailing = TRUE))
  )

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

# Special wrangling for texture

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

# Delete any county or crop averages where n = 1
tables <- groups |>
  purrr::map(
    subset,
    !grepl("(1 Fields)", `Field or Average`)
  )

saveRDS(tables, here::here("inst/extdata/tables.RDS"))

# headers ======================================================================

# Map function to each measurement group, resulting in a new df with
# abbreviations and units in a list for make_ft()
headers <- results_long |>
  soils::pull_unique(target = measurement_group) |>
  as.list() |>
  rlang::set_names() |>
  purrr::map(\(group) get_table_headers(data_dictionary, group))

saveRDS(headers, here::here("inst/extdata/headers.RDS"))
