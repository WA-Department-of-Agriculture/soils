# data dictionary
dataDictionary <- read.csv(
  here::here("inst/template/inst/extdata/dataDictionary.csv"),
  check.names = FALSE,
  encoding = "UTF-8"
  )

usethis::use_data(dataDictionary, overwrite = TRUE)

# example data
exampleData <- read.csv(
  here::here("inst/template/inst/extdata/exampleData.csv"),
  check.names = FALSE)

usethis::use_data(exampleData, overwrite = TRUE)

# wrangling for example data for plot example

# get producer info
producer <- exampleData |>
  subset(producerId == "WUY05")

# Tidy data into long format and join with data dictionary
results_long <- exampleData |>
  dplyr::mutate(dplyr::across(dplyr::contains("_"), as.numeric)) |>
  tidyr::pivot_longer(
    cols = dplyr::matches("_|pH"),
    names_to = "measurement"
  ) |>
  dplyr::inner_join(dataDictionary, by = c("measurement" = "column_name")) |>
  dplyr::arrange(measurement_group, order) |>
  dplyr::mutate(
    abbr = factor(
      abbr,
      levels = dataDictionary$abbr,
      ordered = is.ordered(dataDictionary$order)
    ),
    abbr_unit = factor(
      abbr_unit,
      levels = dataDictionary$abbr_unit,
      ordered = is.ordered(dataDictionary$order)
    )
  ) |>
  dplyr::filter(!is.na(value))

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

saveRDS(df_plot, here::here("inst/template/inst/extdata/df_plot.RDS"))
