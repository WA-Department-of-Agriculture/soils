# This file contains validation functions for use in the {soils} R package (cli
# messaging) and Dirt Data Reports (UI messaging in shiny app).
#
# Organization:
# 1. Utilities  — new_issue(), format_output(), split_issues()
# 2. Gate check — check_file_readable(), is_gate_pass()
# 3. Checks     — one function per validation check
#

# 1. Utilities -----------------------------------------------------------

#' Create a validation issue
#'
#' @param severity "error" or "warning"
#' @param message A character string describing the issue
#'
#' @return A named list with severity and message.
new_issue <- function(severity = c("error", "warning"), message) {
  severity <- rlang::arg_match(severity)
  list(severity = severity, message = message)
}

#' Check context for format_output
#'
#' @param context
#'
#' @return
check_context <- function(context) {
  if (!is.list(context) || is.null(names(context))) {
    cli::cli_abort(
      "{.arg context} must be a named list with elements {.val error} and {.val warning}.",
      call = NULL
    )
  }

  required <- c("error", "warning")

  if (!all(required %in% names(context))) {
    cli::cli_abort(
      "{.arg context} must contain {.val error} and {.val warning}.",
      call = NULL
    )
  }

  if (!all(vapply(context[required], is.character, logical(1)))) {
    cli::cli_abort(
      "{.arg context$error} and {.arg context$warning} must be character.",
      call = NULL
    )
  }

  if (!all(vapply(context[required], length, integer(1)) == 1)) {
    cli::cli_abort(
      "{.arg context$error} and {.arg context$warning} must be length-1.",
      call = NULL
    )
  }

  invisible(TRUE)
}

#' Format validation issues for CLI or UI output
#'
#' For CLI: errors trigger cli_abort(), warnings trigger cli_warn().
#' For UI: ANSI codes are stripped, issue list is returned with clean strings.
#'
#' @param issues List of issues created by new_issue().
#' @param output "cli" (default) for console, "ui" for Shiny.
#' @param context
#'
#' @return For "ui", cleaned issue list. For "cli", invisible after
#'   printing/aborting.
format_output <- function(issues, output = c("cli", "ui"), context = NULL) {
  output <- rlang::arg_match(output)

  if (length(issues) == 0) {
    return(issues)
  }

  # cli output -----------------------------------------------------------------
  if (output == "cli") {
    errors <- Filter(\(x) x$severity == "error", issues)
    warnings <- Filter(\(x) x$severity == "warning", issues)

    # Default context messages
    if (is.null(context)) {
      context <- list(
        error = "Please correct the following {.strong errors}:",
        warning = "Please review the following {.strong warnings}:"
      )
    }

    # Verify that context is a named list with error and warning text
    if (!is.null(context)) {
      check_context(context)
    }

    # Warnings
    if (length(warnings) > 0) {
      bullets <- cli::ansi_strip(
        unlist(lapply(warnings, \(x) x$message))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_warn(c(
        "!" = context$warning,
        bullets,
        call = NULL
      ))
    }

    # Errors
    if (length(errors) > 0) {
      bullets <- cli::ansi_strip(
        unlist(lapply(errors, \(x) x$message))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_abort(
        c(
          "x" = context$error,
          bullets
        ),
        call = NULL
      )
    }

    return(invisible(issues))
  }

  # ui output ------------------------------------------------------------------
  lapply(issues, \(x) {
    x$message <- cli::ansi_strip(x$message)
    x
  })
}

#' Split validation issues into errors and warnings
#'
#' @param issues List of issues created by new_issue().
#'
#' @return Named list with $errors and $warnings.
split_issues <- function(issues) {
  list(
    errors = Filter(\(x) x$severity == "error", issues),
    warnings = Filter(\(x) x$severity == "warning", issues)
  )
}

#' Read soils input
#'
#' @param file
#'
#' @returns
#' @export
#'
#' @examples
read_soils_input <- function(file, ..., output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  # Detect file type -----------------------------------------------------------

  is_excel <- length(file) == 1 && grepl("\\.xlsx$", file, ignore.case = TRUE)
  is_csv <- length(file) == 2 && all(grepl("\\.csv$", file, ignore.case = TRUE))

  if (!is_excel && !is_csv) {
    msg <- c(
      "Provide either:",
      cli::format_inline(
        "One {.strong .xlsx} file with {.val Data} and {.val Data Dictionary} sheets, or"
      ),
      cli::format_inline(
        "Two {.strong .csv} files with data first and dictionary second as {.code c(\"data.csv\", \"data-dictionary.csv\")}."
      )
    )
    issues <- c(issues, list(new_issue("error", msg)))

    return(format_output(
      issues,
      output,
      context = list(
        error = "Invalid input.",
        warning = ""
      )
    ))
  }

  # Read Excel -----------------------------------------------------------------

  if (is_excel) {
    wb <- tryCatch(
      openxlsx2::wb_load(file),
      error = function(e) NULL
    )

    sheets <- tryCatch(
      openxlsx2::wb_get_sheet_names(wb),
      error = function(e) NULL
    )

    if (is.null(wb) || is.null(sheets)) {
      return(format_output(
        list(new_issue("error", "Could not read Excel file.")),
        output,
        context = list(
          error = "Failed to load input data.",
          warning = ""
        )
      ))
    }

    required <- c("Data", "Data Dictionary")
    missing <- setdiff(required, sheets)

    if (length(missing) > 0) {
      msg <- cli::format_inline(
        "Missing required sheets: {.val {missing}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
      return(format_output(
        issues,
        output,
        context = list(
          error = "Failed to load input data.",
          warning = ""
        )
      ))
    }

    data <- tryCatch(
      openxlsx2::wb_to_df(wb, "Data"),
      error = function(e) NULL
    )

    data_dict <- tryCatch(
      openxlsx2::wb_to_df(wb, "Data Dictionary"),
      error = function(e) NULL
    )

    if (is.null(data) && is.null(data_dict)) {
      msg <- cli::format_inline(
        "Could not read both Data and Data Dictionary sheets."
      )

      issues <- c(issues, list(new_issue("error", msg)))
    } else if (is.null(data)) {
      msg <- cli::format_inline(
        "Could not read Data sheet"
      )

      issues <- c(issues, list(new_issue("error", msg)))
    } else if (is.null(data_dict)) {
      msg <- cli::format_inline(
        "Could not read Data Dictionary sheet."
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (length(issues) > 0) {
      return(format_output(
        issues,
        output,
        context = list(
          error = "Failed to load input data."
        )
      ))
    }
  }

  # Read CSV -------------------------------------------------------------------

  if (is_csv) {
    if (!grepl("data", file[1], ignore.case = TRUE)) {
      msg <- cli::format_inline(
        "First file must be the data file and include {.val data} in the filename. You provided: {.file {file[1]}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (!grepl("dictionary", file[2], ignore.case = TRUE)) {
      msg <- cli::format_inline(
        "Second file must be the data dictionary and include {.val dictionary} in the filename. You provided: {.file {file[2]}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (length(issues) > 0) {
      return(format_output(
        issues,
        output,
        context = list(
          error = "Failed to load input data.",
          warning = ""
        )
      ))
    }

    data <- tryCatch(
      read.csv(
        file[1],
        check.names = FALSE,
        encoding = "UTF-8",
        strip.white = TRUE
      ),
      error = function(e) NULL
    )

    data_dict <- tryCatch(
      read.csv(
        file[2],
        check.names = FALSE,
        encoding = "UTF-8",
        strip.white = TRUE
      ),
      error = function(e) NULL
    )

    if (is.null(data) && is.null(data_dict)) {
      msg <- cli::format_inline(
        "Could not read both {.file {basename(file[1])}} and {.file {basename(file[2])}}."
      )

      issues <- c(issues, list(new_issue("error", msg)))
    } else if (is.null(data)) {
      msg <- cli::format_inline(
        "Could not read {.file {basename(file[1])}}."
      )

      issues <- c(issues, list(new_issue("error", msg)))
    } else if (is.null(data_dict)) {
      msg <- cli::format_inline(
        "Could not read {.file {basename(file[2])}}."
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (length(issues) > 0) {
      return(format_output(
        issues,
        output,
        context = list(
          error = "Failed to load input data.",
          warning = ""
        )
      ))
    }
  }

  # Success --------------------------------------------------------------------

  list(
    data = data,
    data_dict = data_dict,
    source = if (is_excel) "excel" else "csv",
    file = file
  )
}

# 2. Gate check ----------------------------------------------------------

#' Check that inputs return loaded data with the correct structure
#'
#' Runs the blocking checks in sequence: there are no duplicate headers and data
#' has rows. If all pass, returns the loaded data frames. If any fail, returns
#' the issue list.
#'
#' @param input Named list with data and data dictionary
#' @param output "cli" (default) or "ui".
#'
#' @return On success: a named list with $data and $data_dict data frames. On
#'   failure: a list of issues (same structure as other check functions). Use
#'   is_gate_pass() to check which one you got back.
check_input_structure <- function(input, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  # Validate structure

  if (!all(c("data", "data_dict") %in% names(input))) {
    msg <- cli::format_inline(
      "Input must be a named list with {.val data} and {.val data_dict}."
    )
    return(list(new_issue("error", msg)))
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

  if (length(issues) > 0) {
    return(format_output(
      issues,
      output,
      context = list(
        error = "Detected issues with input structure.",
        warning = ""
      )
    ))
  }

  return(input) # pass through unchanged
}

#' Check if the gate returned loaded data (pass) or issues (fail)
#'
#' @param gate_result The return value from check_file_readable().
#'
#' @return TRUE if the gate passed (data frames returned), FALSE if issues.
is_gate_pass <- function(gate_result) {
  !is.null(gate_result$data) && !is.null(gate_result$data_dict)
}

# 3. Errors: independent checks --------------------------------------------

# Uniqueness constraints
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
          dplyr::filter(n > 1)

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
          dplyr::filter(field_count > 1) |>
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

# Data types match requirements
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

# Missing values in required columns
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

# Data has at least one column beyond required fields
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

# 4. Warnings: independent checks ------------------------------------------

# Data Dictionary "column_name" matches Data
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

# Check for non-numeric data in measurement columns
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

# Valid measurement groups
check_measurement_groups <- function(data_dict, language = "english") {
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
      "{.field latitude} has values outside valid range (-90 to 90) for: {.val {soils_cli_vec(ids)}}."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Longitude out of range

  bad_lon <- which(!is.na(lon) & (lon < -180 | lon > 180))

  if (length(bad_lon) > 0) {
    ids <- if (has_sample_id) data$sample_id[bad_lon] else bad_lon

    msg <- cli::format_inline(
      "{.field longitude} has values outside valid range (-180 to 180) for: {.val {soils_cli_vec(ids)}}."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Incomplete coordinate pairs

  incomplete <- which(is.na(lat) != is.na(lon))

  if (length(incomplete) > 0) {
    ids <- if (has_sample_id) data$sample_id[incomplete] else incomplete

    msg <- cli::format_inline(
      "Incomplete coordinate pair (one of {.field latitude} or {.field longitude} is missing) for: {.val {soils_cli_vec(ids)}}."
    )

    issues <- c(issues, list(new_issue("error", msg)))
  }

  issues
}

# 5. Wrapper to run all check functions ----------------------------------------

run_all_checks <- function(gate_result, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  issues <- c(
    check_missing_values(gate_result),
    check_uniqueness(gate_result),
    check_data_types(gate_result),
    check_numeric_conversion(gate_result$data, gate_result$data_dict),
    check_additional_columns(gate_result$data),
    check_dict_mismatch(gate_result$data, gate_result$data_dict),
    check_texture_fractions(gate_result$data),
    check_coordinates(gate_result$data),
    check_measurement_groups(gate_result$data_dict)
  )

  issues <- unique(issues)

  return(issues)
}
