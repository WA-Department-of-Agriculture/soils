#' Process validated soil health data for reporting
#'
#' Processes validated soil health data and data dictionary objects into
#' standardized formats used for report generation. This function is designed
#' to be run within the data preparation pipeline in `prepare-data.R`.
#'
#' Processing steps include:
#' \itemize{
#'   \item Converting `sample_id` and `field_id` columns to character
#'   \item Converting measurement columns to numeric
#'   \item Classifying soil texture
#'   \item Synchronizing texture-related rows in the data dictionary
#'   \item Creating formatted labels and ordering variables for reports
#'   \item Pivoting measurement data into tidy long format
#' }
#'
#' The returned object is intended to be saved as an `.rds` file and loaded
#' into the Quarto report template `01_producer-report.qmd`.
#'
#' @param gate_result A named list created from
#'   [`check_input_structure()`] and validated with
#'   [`run_all_checks()`]. Must contain:
#'   \itemize{
#'     \item `data`: cleaned lab results data frame
#'     \item `data_dict`: validated data dictionary data frame
#'     \item `passed`: logical indicating whether validation passed without
#'     errors
#'   }
#'
#' @param language Character string specifying the report language used for
#'   texture dictionary synchronization. Must be either `"English"` or
#'   `"Spanish"`. Defaults to `"English"`.
#'
#' @return A named list containing:
#' \itemize{
#'   \item `results_wide`: processed wide-format data
#'   \item `results_long`: tidy long-format measurement data joined with the
#'   data dictionary
#'   \item `data_dict`: processed data dictionary
#' }
#'
#' @export

process_data <- function(gate_result, language = c("English", "Spanish")) {
  language <- rlang::arg_match(language)

  # Make sure data and data_dict exist in gate_result
  if (!all(c("data", "data_dict") %in% names(gate_result))) {
    cli::cli_abort(c(
      "x" = "{.arg gate_result} must be a named list with elements {.field data} and {.field data_dict}.",
      "i" = "Create {.arg gate_result} with {.fun read_soils_input} and {.fun check_input_structure} in the {.file R/prepare-data.R} pipeline."
    ))
  }

  # Make sure data passed validation
  passed <- gate_result$passed
  if (isFALSE(passed) | is.null(passed)) {
    cli::cli_abort(c(
      "x" = "{.arg gate_result} has not passed validation.",
      "i" = "Fix all errors from {.fun run_all_checks} in the {.file R/prepare-data.R} pipeline."
    ))
  }

  # Set data and dictionary objects
  results_wide <- gate_result$data
  data_dict <- gate_result$data_dict

  # Make sure sample_id and field_id are character
  results_wide <- results_wide |>
    dplyr::mutate(dplyr::across(c(sample_id, field_id), as.character))

  # Convert measurement columns to numeric
  results_wide <- soils::convert_to_numeric(
    results_wide,
    data_dict,
    validate = FALSE
  )

  # Classify soil texture
  results_wide <- soils::classify_texture(results_wide, validate = FALSE)

  # Add texture and fraction rows to the dictionary if created during classification
  data_dict <- soils::sync_dictionary_texture(
    results_wide,
    data_dict,
    language = language
  )

  data_dict <- data_dict |>
    dplyr::mutate(
      # Concatenate abbr and unit with html break for the table and plot labels
      abbr_unit = dplyr::if_else(
        is.na(unit) | unit == "",
        abbr,
        glue::glue("{abbr}<br>{unit}")
      ),
      # Set the order of how measurement groups will appear within the report
      # based on the order found in the data dictionary
      group_order = dplyr::cur_group_id(),
      # Set the order of how measurements will appear within each measurement
      # group based on the order found in the data dictionary
      measurement_order = seq_along(column_name),
      .by = measurement_group
    )

  measurement_cols <- data_dict$column_name[data_dict$column_name != "texture"]

  # Tidy data into long format and join with data dictionary
  results_long <- results_wide |>
    tidyr::pivot_longer(
      cols = dplyr::any_of(measurement_cols),
      names_to = "measurement"
    ) |>
    dplyr::inner_join(data_dict, by = c("measurement" = "column_name")) |>
    dplyr::mutate(
      group_order = factor(
        group_order,
        levels = unique(data_dict$group_order),
        ordered = unique(is.ordered(data_dict$group_order))
      ),
      abbr = factor(
        abbr,
        levels = data_dict$abbr,
        ordered = is.ordered(data_dict$measurement_order)
      ),
      abbr_unit = factor(
        abbr_unit,
        levels = data_dict$abbr_unit,
        ordered = is.ordered(data_dict$measurement_order)
      )
    ) |>
    dplyr::arrange(group_order, measurement_order) |>
    dplyr::filter(!is.na(value))

  return(list(
    results_wide = results_wide,
    results_long = results_long,
    data_dict = data_dict
  ))
}
