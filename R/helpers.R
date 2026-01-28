#' Coerce measurement columns to numeric and warn on coercion
#'
#' Coerces all non-metadata columns in a data frame to numeric. Character values
#' commonly used to represent missing or censored measurements (e.g. `"ND"`,
#' `"<1"`, `"-"`, or `""`) will be converted to `NA`. A warning is emitted if any
#' non-missing values are coerced to `NA` during conversion.
#'
#' @param data A data frame containing both metadata and quantitative
#'   measurement columns.
#' @param measurement_cols A character vector of column names to coerce to numeric.
#'
#' @return A data frame where specified measurement columns have been coerced to numeric.
#'
#' @details
#' This function is intended for use during data ingestion and validation.
#' Measurement values that cannot be safely interpreted as numeric are set to
#' `NA` and excluded from downstream summaries, tables, and plots.
#'
#' @examples
#' # Example data
#' example_data <- data.frame(
#'   year       = c(2023, 2023, 2024),
#'   sample_id  = c("S1", "S2", "S3"),
#'   field_id   = c("A", "A", "B"),
#'   texture    = c("Silt loam", "Silt loam", "Clay loam"),
#'   ph         = c("6.5", "ND", "7.1"),
#'   nh4_n_mg_kg = c("12.3", "<1", ""),
#'   no3_n_mg_kg = c("5.2", "8.1", "NA"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Specify the measurement columns
#' measurement_cols = c("ph", "nh4_n_mg_kg", "no3_n_mg_kg")
#'
#' # Coerce measurement columns to numeric
#' clean_data <- coerce_to_numeric(example_data, measurement_cols)
#'
#' @export
coerce_to_numeric <- function(
  data,
  measurement_cols
) {
  # Abort if measurement_cols is missing
  if (missing(measurement_cols)) {
    cli::cli_abort(
      "Please provide a vector of `measurement_cols` to coerce to numeric."
    )
  }

  # Abort if any measurement columns are not present in data
  missing_cols <- setdiff(measurement_cols, colnames(data))
  if (length(missing_cols) > 0) {
    cli::cli_abort(
      c(
        "The following columns are missing from `data`:",
        rlang::set_names(missing_cols, rep("*", length(missing_cols)))
      )
    )
  }

  # Preserve original values for NA-coercion check
  data_original <- data |>
    dplyr::select(dplyr::all_of(measurement_cols))

  # Coerce measurements to numeric
  # Suppress warnings because this function emits its own warnings
  suppressWarnings(
    data_numeric <- data |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(measurement_cols),
          as.numeric
        )
      )
  )
  # Count NAs introduced by coercion
  na_created <- purrr::map_int(
    measurement_cols,
    ~ sum(!is.na(data_original[[.x]]) & is.na(data_numeric[[.x]]))
  )

  # Assign names
  names(na_created) <- measurement_cols

  # Create warning with bullet points of affected columns and how many values
  # were coerced
  if (any(na_created > 0)) {
    affected <- na_created[na_created > 0]

    bullets <- purrr::imap_chr(
      affected,
      ~ glue::glue(
        "{{.field { .y }}} ({ .x } {if (.x == 1) 'value' else 'values'})"
      )
    )

    # Set names for the bullets to render in cli
    names(bullets) <- rep("*", length(bullets))

    cli::cli_warn(
      c(
        "i" = "Some measurement values were coerced to `NA` during numeric\
        conversion due to character values such as `ND`, `<1`, or `-`.",
        "i" = "{.strong Affected columns}:",
        bullets
      )
    )
  }

  return(data_numeric)
}

#' Calculate the mode of a categorical variable
#'
#' Returns the most frequently occurring value in a character vector, ignoring
#' missing values.
#'
#' @param x Character vector to calculate the mode from.
#'
#' @return A single value (character) that occurs most often in `x`.
#'
#' @examples
#' calculate_mode(washi_data$crop)
#'
#' @export

calculate_mode <- function(x) {
  x <- stats::na.omit(x)
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}

#' Extract unique values from a single column of a data frame
#'
#' @param df A data frame containing the column of interest.
#' @param target Column to extract unique values from (string or unquoted symbol).
#'
#' @return A vector of unique values from the specified column.
#'
#' @examples
#' pull_unique(washi_data, "crop")
#'
#' @export

pull_unique <- function(df, target) {
  df |>
    dplyr::pull({{ target }}) |>
    unique()
}

#' Check if a column is empty
#'
#' Returns TRUE if all values in a vector are missing (`NA`) or blank strings (`""`).
#'
#' @param column A vector to check.
#'
#' @return Logical scalar. TRUE if all values are `NA` or empty strings, FALSE otherwise.
#'
#' @export
is_column_empty <- function(column) {
  # Handle both NA and empty strings "" for character vectors
  all(is.na(column) | column == "")
}

#' Check if a data frame has at least one complete row for the specified columns
#'
#' Returns TRUE if there is at least one row in the data frame where all
#' specified columns are non-missing (no `NA` values).
#'
#' @param df A data frame.
#' @param cols A character vector of column names to check.
#'
#' @return Logical scalar. TRUE if at least one row has all specified columns
#'   complete, FALSE otherwise.
#'
#' @examples
#' has_complete_row(washi_data, c("crop", "county"))
#'
#' @export
has_complete_row <- function(df, cols) {
  # Required columns must exist
  if (!all(cols %in% colnames(df))) {
    return(FALSE)
  }

  df |>
    dplyr::select(dplyr::all_of(cols)) |>
    stats::complete.cases() |>
    any()
}


#' Calculate number of samples and most frequent texture (deprecated)
#'
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Its functionality has been incorporated
#' directly into `summarize_by_var()`. Do not use directly in new code.
#'
#' @param results_long Data frame in tidy, long format containing `sample_id` and `texture`.
#' @param producer_info Vector of producer values for the grouping variable.
#' @param var Variable to group and summarize by.
#'
#' @return Deprecated. Previously returned a data frame with `n` and `Texture` by group.
#'
#' @seealso summarize_by_var
#' @export
get_n_texture_by_var <- function(results_long, producer_info, var) {
  testthat::expect_contains(names(results_long), c("sample_id", "texture"))

  lifecycle::deprecate_warn(
    "1.0.2",
    "get_n_texture_by_var()",
    details = "This helper has been incorporated directly into `summarize_by_var()`."
  )

  results_long |>
    dplyr::filter({{ var }} %in% producer_info) |>
    dplyr::summarize(
      n = dplyr::n_distinct(sample_id),
      Texture = calculate_mode(texture),
      .by = {{ var }}
    )
}

#' Summarize producer's samples with averages by grouping variable
#'
#' @param results_long Dataframe in tidy, long format with columns: `sample_id`,
#'   `measurement_group`, `abbr`, and `value`. If a `texture` column is present,
#'   the most frequent texture (mode) is included in the output.
#' @param producer_samples Dataframe in tidy, long format with columns:
#'   `measurement_group`, `abbr`, `value`.
#' @param var Variable to summarize by.
#'
#' @return A data frame with a "Field or Average" column summarizing the average
#'   values by measurement group, `abbr`, and the specified grouping variable.
#'   If a `texture` column is present, the data frame will contain a `Texture`
#'   column containing the most frequent texture class per grouping variable.
#'
#' @export
summarize_by_var <- function(results_long, producer_samples, var) {
  testthat::expect_contains(
    names(producer_samples),
    c("measurement_group", "abbr", "value")
  )

  testthat::expect_contains(
    names(results_long),
    c("sample_id", "measurement_group", "abbr", "value")
  )

  # Get the unique group that producer has samples for
  producer_info <- producer_samples |>
    pull_unique({{ var }})

  # Filter to samples in the same group as producer's samples
  filtered <- results_long |>
    dplyr::filter({{ var }} %in% producer_info)

  # Get the number of samples per group
  n <- filtered |>
    dplyr::summarize(n = dplyr::n_distinct(sample_id), .by = {{ var }})

  # Calculate the averages per group
  summary <- filtered |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(measurement_group, abbr, {{ var }})
    )

  # If texture is present, calculate the mode of the texture and join to the
  # summary
  if ("texture" %in% names(results_long)) {
    texture_mode <- filtered |>
      dplyr::summarize(
        Texture = calculate_mode(texture),
        .by = {{ var }}
      )
    summary <- summary |>
      dplyr::left_join(texture_mode, by = dplyr::join_by({{ var }}))
  }

  # Join summary with number of samples and create "Field or Average" column
  summary <- summary |>
    dplyr::left_join(n, by = dplyr::join_by({{ var }})) |>
    dplyr::mutate(
      var_label = tools::toTitleCase(
        .data[[rlang::as_name(rlang::ensym(var))]]
      ),
      "Field or Average" = glue::glue("{var_label} Average \n({n} Fields)")
    ) |>
    dplyr::select(-c(n, {{ var }}, var_label))

  # Filter out n = 1 or NA
  summary <- summary |>
    dplyr::filter(!grepl("\\(1 Fields\\)|NA", `Field or Average`))

  return(summary)
}

#' Summarize samples across the project
#'
#' @param results_long Dataframe in tidy, long format with columns: `sample_id`,
#'   `measurement_group`, `abbr`, `value`.
#'
#' @return A data frame summarizing the average values across the project by
#'   `measurement_group` and `abbr`. Includes a "Field or Average" column and,
#'   if present, a `Texture` column.
#'
#' @export
summarize_by_project <- function(results_long) {
  testthat::expect_contains(
    names(results_long),
    c("sample_id", "measurement_group", "abbr", "value")
  )

  # Get the number of samples in the project
  n <- dplyr::n_distinct(results_long$sample_id)

  # Calculate the averages across the project
  summary <- results_long |>
    dplyr::summarize(
      value = mean(value, na.rm = TRUE),
      .by = c(measurement_group, abbr)
    ) |>
    dplyr::mutate(
      "Field or Average" = glue::glue("Project Average \n({n} Fields)")
    )

  # If texture is present, calculate the mode of the texture and join to the
  # summary
  if ("texture" %in% names(results_long)) {
    texture_mode <- calculate_mode(results_long$texture)
    summary <- summary |>
      dplyr::mutate(Texture = texture_mode)
  }

  return(summary)
}
