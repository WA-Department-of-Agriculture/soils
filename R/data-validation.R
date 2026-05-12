#' Data validation utilities and checks
#'
#' Internal validation framework used by the `{soils}` package and
#' Dirt Data Reports application.
#'
#' This file defines a structured system for:
#'
#'   - Creating and managing validation issues (`new_issue()`)
#'   - Formatting output for CLI and UI contexts (`format_issues()`)
#'   - Performing gate checks on input structure (`check_input_structure()`)
#'   - Running independent validation checks on data and data dictionaries
#'
#' Validation functions return lists of issues rather than stopping execution,
#' enabling flexible handling in both console and Shiny environments.
#'

# Utilities --------------------------------------------------------------------

#' Create a validation issue
#'
#' Constructs a standardized validation issue object used throughout
#' the validation framework.
#'
#' @param severity Character. One of `"error"` or `"warning"`.
#' @param message Character. Description of the issue. May include
#'   `cli` formatting.
#'
#' @returns
#' A named list with elements:
#' \describe{
#'   \item{severity}{Issue severity (`"error"` or `"warning"`)}
#'   \item{message}{Formatted message string}
#' }
#'
#' @keywords internal
new_issue <- function(severity = c("error", "warning"), message) {
  severity <- rlang::arg_match(severity)
  list(severity = severity, message = message)
}

#' Format validation issues for CLI or UI output
#'
#' Converts a list of validation issues into formatted output suitable
#' for either console (`cli`) or Shiny UI contexts.
#'
#' @param issues List of issues created by `new_issue()`.
#' @param output Character. One of `"cli"` (default) or `"ui"`.
#' @param context Optional named list providing custom header messages:
#'   \itemize{
#'     \item `"error"`: header for error messages
#'     \item `"warning"`: header for warning messages
#'   }
#'
#' @returns
#' \itemize{
#'   \item `"cli"`: prints formatted messages; errors trigger `cli_abort()`,
#'   warnings trigger `cli_warn()`. Returns invisibly.
#'   \item `"ui"`: returns the issue list with ANSI formatting removed.
#' }
#'
#' @export
format_issues <- function(issues, output = c("cli", "ui"), context = NULL) {
  output <- rlang::arg_match(output)

  if (length(issues) == 0) {
    return(issues)
  }

  # cli output -----------------------------------------------------------------
  if (output == "cli") {
    errors <- purrr::keep(issues, ~ identical(.x$severity, "error"))
    warnings <- purrr::keep(issues, ~ identical(.x$severity, "warning"))

    # Errors
    if (length(errors) > 0) {
      cli::cli_rule(
        left = "{cli::symbol$cross} Errors (must fix to continue)"
      )
      bullets <- cli::ansi_strip(
        unlist(lapply(errors, \(x) x$message))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_inform(bullets)
    }

    # Warnings
    if (length(warnings) > 0) {
      cli::cli_rule(
        left = "{cli::symbol$warning} Warnings (review recommended)"
      )
      bullets <- cli::ansi_strip(
        unlist(lapply(warnings, \(x) x$message))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_inform(bullets)
    }
    return(invisible(issues))
  }

  # ui output ------------------------------------------------------------------
  purrr::map(issues, function(x) {
    x$message <- cli::ansi_strip(x$message)
    x
  })
}

#' Split validation issues by severity
#'
#' Separates a list of issues into errors and warnings.
#'
#' @param issues List of issues created by `new_issue()`.
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{errors}{List of error issues}
#'   \item{warnings}{List of warning issues}
#' }
#'
#' @keywords internal
split_issues <- function(issues) {
  list(
    errors = purrr::keep(issues, ~ identical(.x$severity, "error")),
    warnings = purrr::keep(issues, ~ identical(.x$severity, "warning"))
  )
}

# Gate check -------------------------------------------------------------------

#' Validate input structure
#'
#' Performs blocking ("gate") checks on input data before running
#' downstream validation rules.
#'
#' Checks include:
#' \itemize{
#'   \item Required object structure (`data`, `data_dict`)
#'   \item Duplicate column names
#'   \item Presence of required columns
#'   \item At least one data row
#' }
#'
#' @param input Named list produced by `read_soils_input()`.
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{data}{Input data frame.}
#'   \item{data_dict}{Input data dictionary data frame.}
#'   \item{issues}{A formatted list of validation issue objects. Empty if no issues were found.}
#'   \item{passed}{Logical indicating whether the input passed all gate checks.}
#'   \item{source}{Character string indicating detected input type: `"excel"` or `"csv"`.}
#'   \item{file}{Original input file path(s).}
#' }
#'
#' @details
#' Gate checks are blocking validations required before downstream validation
#' and processing can occur. If any gate checks fail, subsequent validation
#' functions should not be run.
#'
#' This function validates only structural requirements and does not evaluate
#' measurement values, ranges, or data consistency rules.
#'
#' @export
check_input_structure <- function(input) {
  issues <- list()

  # Validate structure

  if (!all(c("data", "data_dict") %in% names(input))) {
    msg <- cli::format_inline(
      "Input must be a named list with {.val data} and {.val data_dict}."
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (!is.data.frame(input$data) && !is.data.frame(input$data_dict)) {
    msg <- cli::format_inline(
      "{.val data} and {.val data_dict} must be dataframes."
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(issues) > 0) {
    return(list(
      data = input$data,
      data_dict = input$data_dict,
      issues = issues,
      passed = length(issues) == 0,
      source = input$source,
      file = input$file
    ))
  }

  # Single unified map

  df_map <- list(
    data = list(
      df = input$data,
      label = "Data",
      type = "data"
    ),
    data_dict = list(
      df = input$data_dict,
      label = "Data Dictionary",
      type = "dictionary"
    )
  )

  # Shared blocking checks

  for (nm in names(df_map)) {
    obj <- df_map[[nm]]
    df <- obj$df
    label <- obj$label
    type <- obj$type

    # 1. Duplicate headers
    dups <- names(df)[duplicated(names(df))]
    if (length(dups) > 0) {
      msg <- cli::format_inline(
        "{.env {.strong {label}}} has duplicate column headers: {.val {dups}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    # 2. Required columns
    required <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::pull(var)

    missing <- setdiff(required, colnames(df))
    if (length(missing) > 0) {
      msg <- cli::format_inline(
        "{.strong {label}} is missing required columns: {.val {missing}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    # 3. Must have at least one row
    if (nrow(df) == 0) {
      msg <- cli::format_inline(
        "{.strong {label}} contains headers but no rows."
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  # Return

  return(list(
    data = input$data,
    data_dict = input$data_dict,
    issues = issues,
    passed = length(issues) == 0,
    source = input$source,
    file = input$file
  ))
}

#' Check if gate validation passed
#'
#' Determines whether the result of `check_input_structure()` is valid
#' data or a list of issues.
#'
#' @param gate_result Output from `check_input_structure()`.
#'
#' @returns
#' Logical. `TRUE` if valid data is returned, `FALSE` otherwise.
#'
#' @keywords internal
is_gate_pass <- function(gate_result) {
  !is.null(gate_result$data) && !is.null(gate_result$data_dict)
}

# Errors: independent checks ---------------------------------------------------

#' Check uniqueness constraints
#'
#' Validates that specified fields meet uniqueness requirements,
#' either globally or within grouping variables.
#'
#' @param x Named list with `data` and `data_dict`.
#'
#' @returns
#' A list of error issues.
#'
#' @keywords internal
check_uniqueness <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    return(list(new_issue("error", msg)))
  }

  df_map <- list(
    data = list(df = x$data, label = "Data", type = "data"),
    data_dict = list(
      df = x$data_dict,
      label = "Data Dictionary",
      type = "dictionary"
    )
  )

  # Loop over datasets
  for (nm in names(df_map)) {
    obj <- df_map[[nm]]
    df <- obj$df
    label <- obj$label
    type <- obj$type

    unique_checks <- required_fields |>
      dplyr::filter(.data$type == .env$type, !is.na(unique_by))

    if (nrow(unique_checks) == 0) {
      next
    }

    findings <- list()

    for (i in seq_len(nrow(unique_checks))) {
      var_name <- unique_checks$var[i]

      group_by_vars <- if (
        stringr::str_detect(unique_checks$unique_by[i], "^c\\(")
      ) {
        stringr::str_extract_all(unique_checks$unique_by[i], '"([^"]+)"')[[
          1
        ]] |>
          stringr::str_remove_all('"')
      } else {
        stringr::str_split(unique_checks$unique_by[i], ",\\s*")[[1]]
      }

      # Skip if columns not present
      if (
        !var_name %in% colnames(df) ||
          !all(group_by_vars %in% colnames(df))
      ) {
        next
      }

      if (length(group_by_vars) == 1 && group_by_vars == var_name) {
        # Global uniqueness
        duplicates <- df |>
          dplyr::count(!!rlang::sym(var_name)) |>
          dplyr::filter(n > 1 & !is.na(!!rlang::sym(var_name)))

        if (nrow(duplicates) > 0) {
          findings[[length(findings) + 1]] <- list(
            var = var_name,
            group = NULL,
            values = duplicates[[var_name]]
          )
        }
      } else {
        # Grouped uniqueness
        duplicates <- df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_by_vars))) |>
          dplyr::add_count(!!rlang::sym(var_name), name = "field_count") |>
          dplyr::filter(field_count > 1 & !is.na(!!rlang::sym(var_name))) |>
          dplyr::distinct(
            dplyr::across(dplyr::all_of(c(group_by_vars, var_name)))
          ) |>
          dplyr::ungroup()

        if (nrow(duplicates) > 0) {
          findings[[length(findings) + 1]] <- list(
            var = var_name,
            group = group_by_vars,
            values = unique(duplicates[[var_name]])
          )
        }
      }
    }

    # Emit one grouped message
    if (length(findings) > 0) {
      bullets <- purrr::map_chr(findings, function(f) {
        if (is.null(f$group)) {
          cli::format_inline(
            "{.field {f$var}}: {.val {soils_cli_vec(f$values)}}"
          )
        } else {
          group_str <- paste(f$group, collapse = " and ")
          cli::format_inline(
            "{.field {f$var}} (must be unique within {group_str}): {.val {soils_cli_vec(f$values)}}"
          )
        }
      })

      msg <- c(
        cli::format_inline("{.strong {label}} has duplicate values:"),
        stats::setNames(bullets, rep("*", length(bullets)))
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  issues
}

#' Check data types against requirements
#'
#' Ensures that column data types match expected types defined in
#' `required_fields`.
#'
#' @param x Named list with `data` and `data_dict`.
#'
#' @returns
#' A list of error issues.
#'
#' @keywords internal
check_data_types <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    return(list(new_issue("error", msg)))
  }

  df_map <- list(
    data = list(df = x$data, label = "Data", type = "data"),
    data_dict = list(
      df = x$data_dict,
      label = "Data Dictionary",
      type = "dictionary"
    )
  )

  # Normalize numeric types
  normalize_type <- function(x) {
    dplyr::case_when(
      x %in% c("integer", "double") ~ "numeric",
      TRUE ~ x
    )
  }

  # Loop datasets
  for (nm in names(df_map)) {
    obj <- df_map[[nm]]
    df <- obj$df
    label <- obj$label
    type <- obj$type

    check_fields <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::filter(var %in% colnames(df), !is.na(var_type))

    if (nrow(check_fields) == 0) {
      next
    }

    # Skip fully empty columns
    non_blank <- sapply(
      df[check_fields$var],
      \(col) !all(is.na(col))
    )

    if (!any(non_blank)) {
      next
    }

    # Actual types
    actual_types <- sapply(
      df[, names(non_blank)[non_blank], drop = FALSE],
      typeof
    ) |>
      normalize_type()

    # Find mismatches
    mismatched <- check_fields |>
      dplyr::filter(var %in% names(actual_types)) |>
      dplyr::mutate(actual_type = actual_types[.data$var]) |>
      dplyr::filter(var_type != actual_type)

    # Collect findings
    if (nrow(mismatched) > 0) {
      bullets <- purrr::pmap_chr(
        list(
          var = mismatched$var,
          expected = mismatched$var_type,
          actual = mismatched$actual_type
        ),
        \(var, expected, actual) {
          cli::format_inline(
            "{.field {var}} (expected {expected}, found {actual})"
          )
        }
      )

      msg <- c(
        cli::format_inline("{.strong {label}} has incorrect data types:"),
        stats::setNames(bullets, rep("*", length(bullets)))
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  issues
}

#' Check for missing values in required fields
#'
#' Identifies missing values in columns where missing values are not allowed.
#'
#' @param x Named list with `data` and `data_dict`.
#'
#' @returns
#' A list of error issues.
#'
#' @keywords internal
check_missing_values <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    return(list(new_issue("error", msg)))
  }

  df_map <- list(
    data = list(df = x$data, label = "Data", type = "data"),
    data_dict = list(
      df = x$data_dict,
      label = "Data Dictionary",
      type = "dictionary"
    )
  )

  # Loop over datasets
  for (nm in names(df_map)) {
    obj <- df_map[[nm]]
    df <- obj$df
    label <- obj$label
    type <- obj$type

    required_cols <- required_fields |>
      dplyr::filter(.data$type == .env$type, missing_allowed == FALSE) |>
      dplyr::filter(var %in% colnames(df)) |>
      dplyr::pull(var)

    if (length(required_cols) == 0) {
      next
    }

    # Collect missing counts
    missing_counts <- purrr::map_int(
      required_cols,
      ~ sum(is.na(df[[.x]]))
    )
    names(missing_counts) <- required_cols

    missing_counts <- missing_counts[missing_counts > 0]

    # Emit one grouped issue per dataset
    if (length(missing_counts) > 0) {
      bullets <- purrr::imap_chr(
        missing_counts,
        \(n, col) {
          cli::format_inline(
            "{.field {col}}: {n} missing value{?s}"
          )
        }
      )

      msg <- c(
        cli::format_inline(
          "{.strong {label}} has missing values in required columns:"
        ),
        stats::setNames(bullets, rep("*", length(bullets)))
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  issues
}

#' Check for presence of additional measurement columns
#'
#' Ensures that the data contains at least one column beyond the required fields.
#'
#' @param data Data frame of input data.
#'
#' @returns
#' A list of error issues.
#'
#' @keywords internal
check_additional_columns <- function(data) {
  issues <- list()

  required <- required_fields |>
    dplyr::filter(type == "data") |>
    dplyr::pull(var)

  additional <- setdiff(colnames(data), required)

  if (length(additional) < 1) {
    msg <- cli::format_inline(
      "Data has no columns beyond the required columns. Add at least one measurement column."
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  return(issues)
}

#' Validate coordinate fields
#'
#' Checks that latitude and longitude columns exist, are within valid ranges,
#' and are provided as complete pairs.
#'
#' @param data Data frame of input data.
#'
#' @returns
#' A list of error issues.
#'
#' @keywords internal
check_coordinates <- function(data) {
  issues <- list()

  coord_fields <- c("latitude", "longitude")

  missing <- setdiff(coord_fields, names(data))
  if (length(missing) > 0) {
    msg <- cli::format_inline(
      "Missing coordinate columns: {.val {missing}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  has_sample_id <- "sample_id" %in% names(data)

  # Skip if both are entirely NA
  if (all(is.na(data$latitude)) && all(is.na(data$longitude))) {
    return(issues)
  }

  # Coerce safely
  lat <- suppressWarnings(as.numeric(data$latitude))
  lon <- suppressWarnings(as.numeric(data$longitude))

  # Latitude out of range

  bad_lat <- which(!is.na(lat) & (lat < -90 | lat > 90))

  if (length(bad_lat) > 0) {
    ids <- if (has_sample_id) data$sample_id[bad_lat] else bad_lat
    msg <- cli::format_inline(
      "{.field latitude} must be within -90 to 90.",
      "\nAffected samples:",
      "\n{.val {soils_cli_vec(ids)}}",
      collapse = FALSE
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Longitude out of range

  bad_lon <- which(!is.na(lon) & (lon < -180 | lon > 180))

  if (length(bad_lon) > 0) {
    ids <- if (has_sample_id) data$sample_id[bad_lon] else bad_lon

    msg <- cli::format_inline(
      "{.field longitude} must be within -180 to 180.",
      "\nAffected samples:",
      "\n{.val {soils_cli_vec(ids)}}",
      collapse = FALSE
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Incomplete coordinate pairs

  incomplete <- which(is.na(lat) != is.na(lon))

  if (length(incomplete) > 0) {
    ids <- if (has_sample_id) data$sample_id[incomplete] else incomplete

    msg <- cli::format_inline(
      "Incomplete coordinate pair (one of {.field latitude} or {.field longitude} is missing).",
      "\nAffected samples:",
      "\n{.val {soils_cli_vec(ids)}}",
      collapse = FALSE
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  issues
}

# Warnings: independent checks -------------------------------------------------

#' Check for mismatches between data and data dictionary
#'
#' Identifies discrepancies between columns in the data and entries in the
#' data dictionary.
#'
#' @param data Data frame of input data.
#' @param data_dict Data frame of data dictionary.
#'
#' @returns
#' A list of warning issues.
#'
#' @keywords internal
check_dict_mismatch <- function(data, data_dict) {
  issues <- list()

  # Guard: can't run this check without column_name in the dictionary
  if (!"column_name" %in% colnames(data_dict)) {
    return(issues)
  }

  required <- required_fields |>
    dplyr::filter(type == "data") |>
    dplyr::pull(var)
  additional <- setdiff(colnames(data), required)
  dict_names <- data_dict$column_name

  missing_in_dict <- setdiff(additional, dict_names)
  missing_in_data <- setdiff(dict_names, colnames(data))

  if (length(missing_in_dict) > 0) {
    msg <- cli::format_inline(
      "Columns in Data not documented in Data Dictionary: {.val {soils_cli_vec(missing_in_dict)}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  if (length(missing_in_data) > 0) {
    msg <- cli::format_inline(
      "Columns in Data Dictionary not found in Data: {.val {soils_cli_vec(missing_in_data)}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

#' Detect non-numeric values in measurement columns
#'
#' Attempts numeric conversion and identifies values that are coerced to `NA`.
#'
#' @param data Data frame of input data.
#' @param data_dict Data frame of data dictionary.
#'
#' @returns
#' A list of warning issues.
#'
#' @keywords internal
check_numeric_conversion <- function(data, data_dict) {
  issues <- list()

  # Validate inputs

  non_measurement <- c("texture", "Texture")

  measurement_cols <- data_dict$column_name |>
    setdiff(non_measurement) |>
    intersect(names(data))

  # Preserve original values

  data_original <- data[, measurement_cols, drop = FALSE]

  # Convert (suppress warnings)

  suppressWarnings(
    data_numeric <- data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(measurement_cols), as.numeric)
      )
  )

  # Detect NA coercion

  na_created <- purrr::map_int(
    measurement_cols,
    ~ sum(!is.na(data_original[[.x]]) & is.na(data_numeric[[.x]]))
  )
  names(na_created) <- measurement_cols

  partial_na <- na_created[na_created > 0]

  if (length(partial_na) > 0) {
    bullets <- purrr::imap_chr(
      partial_na,
      ~ cli::format_inline(
        "{.field { .y }} ({ .x } {if (.x == 1) 'value' else 'values'})"
      )
    )

    msg <- c(
      "Non-numeric values were converted to NA (e.g., ND, <1).",
      "Measurement columns affected:",
      stats::setNames(bullets, rep("*", length(bullets)))
    )

    issues <- c(issues, list(new_issue("warning", msg)))
  }

  issues
}

#' Validate measurement group values
#'
#' Ensures that `measurement_group` values match expected controlled
#' vocabularies.
#'
#' @param data_dict Data frame of data dictionary.
#' @inheritParams create_issue_xlsx
#' @returns A list of warning issues.
#'
#' @keywords internal
check_measurement_groups <- function(
  data_dict,
  language = c("English", "Spanish")
) {
  language <- rlang::arg_match(language)
  issues <- list()

  measurement_groups <- list(
    english = c(
      "Physical",
      "Biological",
      "Chemical",
      "Plant Essential Macro Nutrients",
      "Plant Essential Micro Nutrients"
    ),
    spanish = c(
      "Mediciones f\u00edsicas",
      "Mediciones biol\u00f3gicas",
      "Mediciones qu\u00edmicas",
      "Macronutrientes esenciales para plantas",
      "Micronutriente es esenciales para plantas"
    )
  )

  language <- tolower(language)
  if (!language %in% names(measurement_groups)) {
    return(issues)
  }

  valid <- enc2native(measurement_groups[[language]])

  if (!"measurement_group" %in% colnames(data_dict)) {
    msg <- cli::format_inline(
      "Missing {.field measurement_group} column in Data Dictionary"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
    return(issues)
  }

  actual <- enc2native(
    data_dict$measurement_group[!is.na(data_dict$measurement_group)]
  )
  invalid <- setdiff(actual, valid)

  if (length(invalid) > 0) {
    lang_label <- stringr::str_to_title(language)
    msg <- cli::format_inline(
      "Invalid {.field measurement_group} values: {.val {invalid}}.",
      "\nValid options for {lang_label}:",
      "\n{.val {valid}}",
      collapse = FALSE
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

# Wrapper to run all check functions -------------------------------------------

#' Run all validation checks
#'
#' Executes all non-gate validation checks and aggregates validation issues.
#'
#' @param gate_result Named list produced by `check_input_structure()`.
#' @inheritParams create_issue_xlsx
#'
#' @returns
#' A named list with:
#' \describe{
#'   \item{data}{Validated input data frame.}
#'   \item{data_dict}{Validated data dictionary data frame.}
#'   \item{issues}{A list of validation issue objects containing errors and warnings. Empty if no issues were found.}
#'   \item{passed}{Logical indicating whether validation passed with no errors.}
#'   \item{source}{Character string indicating detected input type: `"excel"` or `"csv"`.}
#'   \item{file}{Original input file path(s).}
#' }
#'
#' @details
#' This function assumes that gate checks have already passed and does not
#' perform structural validation.
#'
#' Validation checks include:
#' \itemize{
#'   \item Missing required values
#'   \item Duplicate identifiers
#'   \item Invalid data types
#'   \item Non-numeric measurement values
#'   \item Mismatches between the data and data dictionary
#'   \item Texture fraction validation
#'   \item Coordinate validation
#'   \item Measurement group validation
#' }
#'
#' Validation passes when no issues with severity `"error"` are present.
#' Warnings do not prevent downstream processing.
#'
#' @export
run_all_checks <- function(
  gate_result,
  language = c("English", "Spanish")
) {
  language <- rlang::arg_match(language)

  # Make sure data and data_dict exist in gate_result
  if (!all(c("data", "data_dict") %in% names(gate_result))) {
    cli::cli_abort(c(
      "x" = "{.arg gate_result} must be a named list with elements {.field data} and {.field data_dict}.",
      "i" = "Create {.arg gate_result} with {.fun read_soils_input} and {.fun check_input_structure} in the {.file R/prepare-data.R} pipeline."
    ))
  }

  issues <- c(
    check_missing_values(gate_result),
    check_uniqueness(gate_result),
    check_data_types(gate_result),
    check_numeric_conversion(
      gate_result$data,
      gate_result$data_dict
    ),
    check_additional_columns(gate_result$data),
    check_dict_mismatch(
      gate_result$data,
      gate_result$data_dict
    ),
    check_texture_fractions(gate_result$data),
    check_coordinates(gate_result$data),
    check_measurement_groups(
      gate_result$data_dict,
      language = language
    )
  )

  issues <- unique(issues)

  has_errors <- purrr::some(
    issues,
    ~ identical(.x$severity, "error")
  )

  passed <- !has_errors

  list(
    data = gate_result$data,
    data_dict = gate_result$data_dict,
    issues = issues,
    passed = passed,
    source = gate_result$source,
    file = gate_result$file
  )
}
