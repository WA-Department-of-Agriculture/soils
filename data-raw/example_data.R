# data dictionary
data_dictionary <- read.csv(
  here::here("inst/template/data/data-dictionary.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
  )

usethis::use_data(data_dictionary, overwrite = TRUE)

# example data
washi_data <- read.csv(
  here::here("inst/template/data/washi-data.csv"),
  check.names = FALSE)

usethis::use_data(washi_data, overwrite = TRUE)

# wrangling for example data for plot example

# get producer info
producer <- washi_data |>
  dplyr::filter(producer_id == "WUY05")

# Tidy data into long format and join with data dictionary
results_long <- washi_data |>
  dplyr::mutate(dplyr::across(11:49, as.numeric)) |>
  tidyr::pivot_longer(
    cols = 11:49,
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

saveRDS(df_plot, here::here("inst/extdata/df-plot.RDS"))
