# Setup --------------------------------------------------------------

# EDIT: You will need to add your own cleaned lab data to this data
# folder, using 'exampleData.csv' as a template.
#
# 'dataDictionary.csv' must also be updated to match your own
# data set.

# Load lab results
data <- read.csv(
  paste0(here::here(), "/inst/extdata/exampleData.csv"),
  check.names = FALSE
)

# Load data dictionary for pretty labels
dictionary <- read.csv(
  paste0(here::here(), "/inst/extdata/dataDictionary.csv"),
  encoding = "UTF-8"
)

# Get producer info --------------------------------------------------------

# If field name is blank, use field ID
data$fieldName <- ifelse(is.na(data$fieldName), data$fieldId, data$fieldName)

# If farm name is blank, use producer ID
data$farmName <- ifelse(is.na(data$farmName), data$producerId, data$farmName)

# Subset to producer samples
producer_samples <- data |>
  subset(producerId == pid & year == yr)

# Identify measurements that producer has results for
cols_to_keep <- colSums(
  is.na(producer_samples)
) != nrow(producer_samples)

# Extract only measurements that producer has results for
producer_samples <- producer_samples[, cols_to_keep, drop = FALSE]

# Extract producer sample IDs, crops, counties, and farm name into
# producer list
producer <- list("sampleId", "crop", "county", "farmName") |>
  rlang::set_names() |>
  purrr::map(\(x) pull_unique(
    df = producer_samples,
    target = x
  ))

# In case of multiple farm names, grab first one
producer$farmName <- producer$farmName[[1]]

# Add producer dataframe to producer list
producer <- list(info = producer, data = producer_samples)

# Prepare results df ---------------------------------------------------------

# Prepare results dataframe with categories and sample labels
results <- data |>
  dplyr::mutate(
    .before = sampleId,
    # Dummy variable for strip plots
    dummy = "dummy",
    # Category variable for producer's samples, samples in same crop
    # or county, and others in the project
    category =
      dplyr::case_when(
        sampleId %in% producer$info$sampleId ~ "4producer",
        crop %in% producer$info$crop ~ "3sameCrop",
        county == producer$info$county ~ "2sameCounty",
        TRUE ~ "1other"
      )
  ) |>
  # Create sample labels for plot legend
  dplyr::mutate(
    .after = category,
    sampleLabel = dplyr::case_when(
      category == "producer" ~ paste(fieldName, "<br>Result:"),
      category %in% c("3sameCrop", "2sameCounty") ~ paste(
        county,
        crop,
        "Result:",
        sep = "<br>"
      ),
      TRUE ~ paste("Result:")
    )
  ) |>
  dplyr::arrange(category)

# Set category with correct factor level order this is important so
# producer's samples are plotted on top of the others
results$category <- factor(
  results$category,
  levels = c(
    "1other",
    "2sameCounty",
    "3sameCrop",
    "4producer"
  ),
  labels = c(
    "Other fields",
    "Same county",
    "Same crop",
    "Your fields"
  )
)

# Tidy into long format -----------------------------------------------------

# Tidy data into long format and join with data dictionary
results_long <- results |>
  dplyr::mutate(dplyr::across(dplyr::contains("_"), as.numeric)) |>
  tidyr::pivot_longer(
    cols = !c(
      dummy,
      category,
      sampleLabel,
      sampleId,
      farmName,
      producerName,
      producerId,
      fieldName,
      fieldId,
      county,
      crop,
      year,
      texture,
      longitude,
      latitude
    ),
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
  )

# Remove irrelevant measurements ----------------------------------------------

# Get measurements tested for producer's fields
producer_measurements <- results_long |>
  subset(category == "Your fields" & !is.na(value)) |>
  pull_unique(measurement)

# Subset results_long to exclude any measurements that the producer
# didn't have tested
results_long <- results_long |>
  subset(measurement %in% producer_measurements)

# Split data into measurement groups -----------------------------------------

# Split df into list with each measurement group as its own df
groups <- results_long |>
  split(results_long$measurement_group)

# Subsets producer samples and pivots a measurement group
# df wider for use in tables

subset_widen_producer <- function(df) {
  df |>
    subset(category == "Your fields") |>
    dplyr::select(
      `Field or Average` = fieldName,
      abbr,
      value
    ) |>
    tidyr::pivot_wider(
      id_cols = `Field or Average`,
      names_from = abbr
    )
}

# Map function to each measurement group df, resulting in a new df
# containing producer's samples within a list
groups_producer <- groups |>
  rlang::set_names(paste0) |>
  purrr::map(subset_widen_producer)

# Calculate averages for each measurement group ----------------------------

# This function calculates the county, crop, and project averages for
# each measurement group

group_averages <- function(data) {
  # By county
  n_county <- results |>
    subset(county %in% producer$info$county) |>
    dplyr::group_by(county) |>
    dplyr::summarize(n = dplyr::n_distinct(sampleId))

  county_texture <- groups$physical |>
    dplyr::group_by(county) |>
    dplyr::summarize(Texture = calculate_mode(texture)) |>
    subset(county %in% producer$info$county)

  county <- data |>
    dplyr::group_by(abbr, county) |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .groups = "keep"
    ) |>
    subset(county %in% producer$info$county) |>
    dplyr::left_join(n_county) |>
    dplyr::left_join(county_texture) |>
    dplyr::mutate(
      `Field or Average` = paste0(
        county,
        " Average \n(",
        n,
        " Fields)"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(county, n))

  # By crop
  n_crop <- results |>
    subset(crop %in% producer$info$crop) |>
    dplyr::group_by(crop) |>
    dplyr::summarize(n = dplyr::n_distinct(sampleId))

  crop_texture <- groups$physical |>
    dplyr::group_by(crop) |>
    dplyr::summarize(Texture = calculate_mode(texture)) |>
    subset(crop %in% producer$info$crop)

  crop <- data |>
    dplyr::group_by(abbr, crop) |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .groups = "keep"
    ) |>
    subset(crop %in% producer$info$crop) |>
    dplyr::left_join(n_crop) |>
    dplyr::left_join(crop_texture) |>
    dplyr::mutate(
      `Field or Average` = paste0(
        crop,
        " Average \n(",
        n,
        " Fields)"
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-c(crop, n))

  # By project
  n_project <- results |>
    nrow()

  texture_project <- calculate_mode(groups$physical$texture)

  project <- data |>
    dplyr::group_by(abbr) |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .groups = "keep"
    ) |>
    dplyr::mutate(
      "Field or Average" = paste0(
        "Project Average \n(",
        n_project,
        " Fields)"
      ),
      Texture = texture_project
    ) |>
    dplyr::ungroup()

  # Bind all together into one df
  dplyr::bind_rows(county, crop, project) |>
    dplyr::mutate(value = as.numeric(formatC(
      value,
      2,
      drop0trailing = TRUE
    ))) |>
    tidyr::pivot_wider(
      id_cols = c(`Field or Average`, Texture),
      names_from = abbr
    )
}

# Map function to each measurement group, resulting in new dfs of crop, county, project averages in a list
averages <- groups |>
  purrr::map(group_averages)

# Special wrangling for texture -----------------------------------------

# Extract physical df from averages list
physical <- list(physical = averages$physical)

# Remove texture from all dataframes except physical
averages <- purrr::map(
  subset(
    averages,
    !(names(averages) == "physical")
  ),
  \(x) dplyr::select(x, -Texture)
)

# Add physical df back to the averages list
averages <- c(averages, physical)

# Prepare data for flextable ------------------------------------------------

# Combine two lists into one
tables <- c(groups_producer, averages)

# Split the list by name then use bind rows to combine the df for each group
tables <- purrr::map(split(tables, names(tables)), dplyr::bind_rows)

# Delete any county or crop averages where n = 0
tables <- purrr::map(
  tables,
  subset,
  !grepl("(1 Fields)", `Field or Average`)
)

# This function uses the data dictionary to create a new dataframe of
# the abbreviations and units for each measurement group for flextable
table_headers <- function(group) {
  dictionary |>
    subset(measurement_group == group) |>
    dplyr::select(abbr, unit) |>
    dplyr::mutate(key = abbr, .after = abbr) |>
    rbind(c("Field or Average", "Field or Average", ""))
}

# Map function to each measurement group, resulting in a new df with
# abbreviations and units in a list
headers <- results_long |>
  pull_unique(target = measurement_group) |>
  as.list() |>
  rlang::set_names() |>
  purrr::map(table_headers)

# Wrangle physical data for the texture triangle ---------------------------
texture <- groups$physical |>
  subset(
    measurement %in% c("sand_%", "silt_%", "clay_%")
  ) |>
  dplyr::select(
    category,
    sampleId,
    Field = fieldName,
    Texture = texture,
    measurement,
    value
  ) |>
  tidyr::pivot_wider(
    id_cols = c(sampleId, category, Field, Texture),
    names_from = measurement
  ) |>
  dplyr::rename(
    Sand = `sand_%`,
    Silt = `silt_%`,
    Clay = `clay_%`
  ) |>
  dplyr::mutate(Field = as.character(Field))

# Modify physical table to include texture of producer fields and
# texture mode across county, crop, project
tables$physical <- texture |>
  subset(category == "Your fields") |>
  dplyr::select(Field, Texture) |>
  dplyr::full_join(
    tables$physical,
    by = c("Field" = "Field or Average")
  ) |>
  dplyr::mutate(
    Texture = ifelse(
      is.na(Texture.x),
      Texture.y,
      Texture.x
    ),
    .after = Field
  ) |>
  dplyr::rename(`Field or Average` = "Field") |>
  dplyr::select(-c(Texture.x, Texture.y))

# Set up data for leaflet function --------------------------------------

gis <- producer$data |>
  subset(!duplicated(sampleId)) |>
  dplyr::arrange(fieldId) |>
  dplyr::mutate(dplyr::across(
    dplyr::where(is.numeric),
    \(x) round(x, 4)
  )) |>
  dplyr::select(c(
    "Sample ID" = sampleId,
    "Field ID" = fieldId,
    "Field Name" = fieldName,
    "Crop" = crop,
    "Longitude" = longitude,
    "Latitude" = latitude
  )) |>
  dplyr::mutate(`Field ID` = as.character(`Field ID`))
