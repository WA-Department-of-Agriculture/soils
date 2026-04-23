# This file contains validation functions for use in the {soils} R package (cli
# messaging) and Dirt Data Reports (UI messaging in shiny app).
#
# Organization:
# 1. Utilities  — new_issue(), format_output(), split_issues()
# 2. Gate check — check_file_readable(), is_gate_pass()
# 3. Checks     — one function per validation check
#

# --- 1. Utilities -----------------------------------------------------------

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

  # cli output for {soils}
  if (output == "cli") {
    errors <- Filter(\(x) x$severity == "error", issues)
    warnings <- Filter(\(x) x$severity == "warning", issues)

    # Default context messages ------------------------------------------------
    if (is.null(context)) {
      context <- list(
        error = "Process failed.",
        warning = "Process completed with warnings."
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

  # UI output for create_issue_xlsx() and Dirt Data Reports: strip ANSI codes
  # from messages
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

  # Detect file type ----------------------------------------------------------

  is_excel <- length(file) == 1 && grepl("\\.xlsx$", file, ignore.case = TRUE)
  is_csv <- length(file) == 2 && all(grepl("\\.csv$", file, ignore.case = TRUE))

  if (!is_excel && !is_csv) {
    msg1 <- cli::format_inline(
      "Provide either:"
    )
    issues <- c(issues, list(new_issue("error", msg1)))

    msg2 <- cli::format_inline(
      "a single {.file .xlsx} file with {.val Data} and {.val Data Dictionary} sheets, or"
    )
    issues <- c(issues, list(new_issue("error", msg2)))
    msg3 <- cli::format_inline(
      "two {.file .csv} files as {.code c(\"data.csv\", \"data-dictionary.csv\")}."
    )
    issues <- c(issues, list(new_issue("error", msg3)))
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
          error = "Failed to load input data."
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

# --- 2. Gate check ----------------------------------------------------------

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

  # Validate structure --------------------------------------------------------

  if (!all(c("data", "data_dict") %in% names(input))) {
    msg <- cli::format_inline(
      "Input must contain {.val data} and {.val data_dict}."
    )
    return(list(new_issue("error", msg)))
  }

  data <- input$data
  data_dict <- input$data_dict

  # Shared blocking checks -----------------------------------------------------

  # Duplicate headers (data)
  dups <- names(data)[duplicated(names(data))]
  if (length(dups) > 0) {
    msg <- cli::format_inline(
      "Data has duplicate column headers: {.val {dups}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Duplicate headers (dictionary)
  dups <- names(data_dict)[duplicated(names(data_dict))]
  if (length(dups) > 0) {
    msg <- cli::format_inline(
      "Data Dictionary has duplicate column headers: {.val {dups}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Data must have rows
  if (nrow(data) == 0) {
    msg <- cli::format_inline(
      "Data contains headers but no rows."
    )
    return(c(issues, list(new_issue("error", msg))))
  }

  # Return ---------------------------------------------------------------------

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

# --- 3. Errors: independent checks --------------------------------------------

# Check 4: Required columns
check_required_columns <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  # Map list elements to required_fields$type values
  df_map <- list(
    data = "data",
    data_dict = "dictionary"
  )

  for (nm in names(df_map)) {
    df <- x[[nm]]
    type <- df_map[[nm]]

    # Get required columns for this type
    required <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::pull(var)

    # Check missing
    missing <- setdiff(required, colnames(df))

    if (length(missing) > 0) {
      msg <- cli::format_inline(
        "{.envvar {type}} is missing required columns: {.val {missing}}"
      )

      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  return(issues)
}

# Check 5: Uniqueness constraints
check_uniqueness <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  # Map list elements to required_fields$type values
  df_map <- list(
    data = "data",
    data_dict = "dictionary"
  )

  for (nm in names(df_map)) {
    df <- x[[nm]]
    type <- df_map[[nm]]

    # Filter rules for this type
    unique_checks <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::filter(!is.na(unique_by))

    for (i in seq_len(nrow(unique_checks))) {
      var_name <- unique_checks$var[i]

      group_by_vars <- if (
        stringr::str_detect(unique_checks$unique_by[i], "^c\\(")
      ) {
        stringr::str_extract_all(
          unique_checks$unique_by[i],
          '"([^"]+)"'
        )[[1]] |>
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
        # Globally unique
        duplicates <- df |>
          dplyr::count(!!rlang::sym(var_name)) |>
          dplyr::filter(n > 1)

        if (nrow(duplicates) > 0) {
          dup_vals <- duplicates[[var_name]]
          msg <- cli::format_inline(
            "{.envvar {type}} has duplicate values in {.field {var_name}}: {.val {soils_cli_vec(dup_vals)}}."
          )
          issues <- c(issues, list(new_issue("error", msg)))
        }
      } else {
        # Unique within groups
        duplicates <- df |>
          dplyr::group_by(dplyr::across(dplyr::all_of(group_by_vars))) |>
          dplyr::add_count(!!rlang::sym(var_name), name = "field_count") |>
          dplyr::filter(field_count > 1) |>
          dplyr::distinct(
            dplyr::across(dplyr::all_of(c(group_by_vars, var_name)))
          ) |>
          dplyr::ungroup()

        if (nrow(duplicates) > 0) {
          group_str <- paste(group_by_vars, collapse = " and ")
          dup_vals <- duplicates[[var_name]]
          msg <- cli::format_inline(
            "{.envvar {type}} has duplicate values in {.field {var_name}} within the combination of {group_str}: {.val {soils_cli_vec(dup_vals)}}."
          )
          issues <- c(issues, list(new_issue("error", msg)))
        }
      }
    }
  }

  return(issues)
}

# Check 6: Data types match requirements
check_data_types <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  # Map list elements to required_fields$type values
  df_map <- list(
    data = "data",
    data_dict = "dictionary"
  )

  for (nm in names(df_map)) {
    df <- x[[nm]]
    type <- df_map[[nm]]

    # Filter rules for this type
    check_fields <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::filter(var %in% colnames(df), !is.na(var_type))

    if (nrow(check_fields) == 0) {
      next
    }

    # Skip columns that are entirely NA
    non_blank <- sapply(
      df[check_fields$var],
      \(col) !all(is.na(col))
    )

    if (!any(non_blank)) {
      next
    }

    # Helper function to normalize integer and double outputs from typeof() to
    # numeric
    normalize_type <- function(x) {
      dplyr::case_when(
        x %in% c("integer", "double") ~ "numeric",
        TRUE ~ x
      )
    }

    actual_types <- sapply(
      df[, names(non_blank)[non_blank], drop = FALSE],
      typeof
    ) |>
      normalize_type()

    mismatched <- check_fields |>
      dplyr::filter(var %in% names(actual_types)) |>
      dplyr::mutate(actual_type = actual_types[.data$var]) |>
      dplyr::filter(var_type != actual_type)

    if (nrow(mismatched) > 0) {
      for (i in seq_len(nrow(mismatched))) {
        r <- mismatched[i, ]
        msg <- cli::format_inline(
          "{.envvar {type}} has incorrect data type in {.field {r$var}} (expected {.val {r$var_type}}, found {.val {r$actual_type}})."
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    }
  }

  return(issues)
}

# Check 7: Missing values in required columns
check_missing_values <- function(x) {
  issues <- list()

  # Validate input structure
  if (!all(c("data", "data_dict") %in% names(x))) {
    msg <- cli::format_inline(
      "Input must be a list with {.val data} and {.val data_dict}."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  df_map <- list(
    data = "data",
    data_dict = "dictionary"
  )

  for (nm in names(df_map)) {
    df <- x[[nm]]
    type <- df_map[[nm]]

    required_cols <- required_fields |>
      dplyr::filter(.data$type == .env$type) |>
      dplyr::filter(missing_allowed == FALSE) |>
      dplyr::filter(var %in% colnames(df)) |>
      dplyr::pull(var)

    if (length(required_cols) == 0) {
      next
    }

    for (col in required_cols) {
      n_missing <- sum(is.na(df[[col]]))

      if (n_missing > 0) {
        msg <- cli::format_inline(
          "{.envvar {type}} has {n_missing} missing value{?s} in {.field {col}}. This column does not allow blank values."
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    }
  }

  return(issues)
}

# --- 4. Warnings: independent checks ------------------------------------------

# Check 8: Data has at least one column beyond required fields
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
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

# Check 9: data and data dictionary mismatch
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
      "Columns in {.envvar data} not documented in {.envvar dictionary}: {.val {missing_in_dict}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  if (length(missing_in_data) > 0) {
    msg <- cli::format_inline(
      "Columns in {.envvar dictionary} not found in {.envvar data}: {.val {missing_in_data}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

# Check 10: Check for non-numeric data in measurement columns
check_numeric_conversion <- function(data, data_dict) {
  issues <- list()

  # Validate inputs -----------------------------------------------------------

  non_measurement <- c("texture", "Texture")

  measurement_cols <- data_dict$column_name |>
    setdiff(non_measurement) |>
    intersect(names(data))

  # Preserve original values --------------------------------------------------

  data_original <- data[, measurement_cols, drop = FALSE]

  # Convert (suppress warnings) -----------------------------------------------

  suppressWarnings(
    data_numeric <- data |>
      dplyr::mutate(
        dplyr::across(dplyr::all_of(measurement_cols), as.numeric)
      )
  )

  # Detect NA coercion --------------------------------------------------------

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
      "Non-numeric values were converted to `NA` (e.g., `ND`, `<1`).",
      "Measurement columns affected:",
      stats::setNames(bullets, rep("*", length(bullets)))
    )

    issues <- c(issues, list(new_issue("warning", msg)))
  }

  issues
}

# Check 11: Valid measurement groups
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
      "Missing {.field measurement_group} column in {.envvar dictionary}"
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
      "Invalid {.field measurement_group} values: {.val {invalid}}. Valid options for {lang_label}: {.val {valid}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

# 5. Wrapper to run all check functions ----------------------------------------

validate_dataset <- function(gate_result) {
  issues <- c(
    check_required_columns(gate_result),
    check_uniqueness(gate_result),
    check_data_types(gate_result),
    check_missing_values(gate_result),
    check_additional_columns(gate_result$data),
    check_texture_fractions(gate_result$data),
    check_dict_mismatch(gate_result$data, gate_result$data_dict),
    check_numeric_conversion(gate_result$data, gate_result$data_dict),
    check_measurement_groups(gate_result$data_dict)
  )

  issues <- unique(issues)
  return(issues)
}

# 6. Create issue xlsx ---------------------------------------------------------

create_issue_xlsx <- function(
  input_path,
  output_path,
  issues
) {
  wb <- openxlsx2::wb_load(input_path)

  # Issues tab -----------------------------------------------------------------

  issues <- format_output(issues, "ui")

  error_df <- data.frame(
    Severity = vapply(issues, \(x) x$severity, character(1)),
    Message = vapply(issues, \(x) x$message, character(1)),
    stringsAsFactors = FALSE
  )

  wb$add_worksheet("Issues")
  wb$add_data(sheet = "Issues", x = error_df)

  # Style the header row (bold + bottom border)
  wb$add_font(
    sheet = "Issues",
    dims = "A1:B1",
    bold = TRUE
  )
  wb$add_border(
    sheet = "Issues",
    dims = "A1:B1",
    bottom_border = "thin"
  )

  # Style error rows
  error_rows <- which(error_df$Severity == "error") + 1
  if (length(error_rows) > 0) {
    error_dims <- openxlsx2::wb_dims(rows = error_rows, cols = 1:2)
    wb$add_font(
      sheet = "Issues",
      dims = error_dims,
      color = openxlsx2::wb_color(hex = "#9C0006")
    )
  }

  # Style warning rows
  warning_rows <- which(error_df$Severity == "warning") + 1
  if (length(warning_rows) > 0) {
    warning_dims <- openxlsx2::wb_dims(rows = warning_rows, cols = 1:2)
    wb$add_font(
      sheet = "Issues",
      dims = warning_dims,
      color = openxlsx2::wb_color(hex = "#9C6500")
    )
  }

  # Set column widths
  wb$set_col_widths(sheet = "Issues", cols = 1, widths = 12)
  wb$set_col_widths(sheet = "Issues", cols = 2, widths = 120)

  # Style error conditional formatting
  wb$add_dxfs_style(
    name = "error_style",
    font_color = openxlsx2::wb_color(hex = "#9C0006"),
    bg_fill = openxlsx2::wb_color(hex = "#FFC7CE")
  )

  # Style warning conditional formatting
  wb$add_dxfs_style(
    name = "warning_style",
    font_color = openxlsx2::wb_color(hex = "#9C5700"),
    bg_fill = openxlsx2::wb_color(hex = "#FFEB9C")
  )

  # Data tab -------------------------------------------------------------------

  if ("Data" %in% wb$sheet_names) {
    # Get column headers from Data sheet to map names to positions
    data_headers <- openxlsx2::wb_to_df(
      wb,
      sheet = "Data",
      rows = 1,
      col_names = FALSE
    )
    data_headers <- as.character(unlist(data_headers[1, ]))

    # Figure out how many rows are in the Data sheet
    data_full <- openxlsx2::wb_to_df(wb, sheet = "Data", col_names = TRUE)
    max_row <- nrow(data_full) + 1 # +1 because row 1 is the header
    if (max_row < 2) {
      max_row <- 1000
    } # fallback

    # Helper to get column index by name
    col_index <- function(col_name) {
      which(data_headers == col_name)
    }

    # 1. Blanks in required columns (missing_allowed == FALSE)
    required_cols <- required_fields |>
      dplyr::filter(missing_allowed == "FALSE", var %in% data_headers) |>
      dplyr::pull(var)

    for (col_name in required_cols) {
      idx <- col_index(col_name)
      if (length(idx) == 1) {
        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
          type = "containsBlanks",
          style = "error_style"
        )
      }
    }

    # 2. Duplicate sample_id
    if ("sample_id" %in% data_headers) {
      idx <- col_index("sample_id")
      if (length(idx) == 1) {
        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
          type = "duplicatedValues",
          style = "error_style"
        )
      }
    }

    # 3. Duplicate field_id within producer_id + year combo
    if (all(c("producer_id", "year", "field_id") %in% data_headers)) {
      idx_prod <- col_index("producer_id")
      idx_year <- col_index("year")
      idx_field <- col_index("field_id")

      if (all(lengths(list(idx_prod, idx_year, idx_field)) == 1)) {
        # Convert to Excel column letters
        col_prod <- openxlsx2::int2col(idx_prod)
        col_year <- openxlsx2::int2col(idx_year)
        col_field <- openxlsx2::int2col(idx_field)

        # COUNTIFS across all three columns
        rule <- sprintf(
          "COUNTIFS($%s$2:$%s$%d,$%s2,$%s$2:$%s$%d,$%s2,$%s$2:$%s$%d,$%s2)>1",
          col_prod,
          col_prod,
          max_row,
          col_prod,
          col_year,
          col_year,
          max_row,
          col_year,
          col_field,
          col_field,
          max_row,
          col_field
        )

        # Apply only to field_id column
        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx_field),
          type = "expression",
          rule = rule,
          style = "error_style"
        )
      }
    }

    # 4. Texture fraction validation (from check_texture_fractions)
    texture_cols <- c("sand_percent", "silt_percent", "clay_percent")

    if (all(texture_cols %in% data_headers)) {
      idx_sand <- col_index("sand_percent")
      idx_silt <- col_index("silt_percent")
      idx_clay <- col_index("clay_percent")

      if (all(lengths(list(idx_sand, idx_silt, idx_clay)) == 1)) {
        col_sand <- openxlsx2::int2col(idx_sand)
        col_silt <- openxlsx2::int2col(idx_silt)
        col_clay <- openxlsx2::int2col(idx_clay)
        col_tex <- openxlsx2::int2col(col_index("texture"))

        # Error: values outside 0–100
        for (col_name in texture_cols) {
          idx <- col_index(col_name)
          if (length(idx) == 1) {
            col_letter <- openxlsx2::int2col(idx)

            # Values < 0
            wb$add_conditional_formatting(
              sheet = "Data",
              dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
              rule = paste0(col_letter, "2<0"),
              style = "error_style"
            )

            # Values > 100
            wb$add_conditional_formatting(
              sheet = "Data",
              dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
              rule = paste0(col_letter, "2>100"),
              style = "error_style"
            )
          }
        }

        # Error: sum not ~100 (only when all 3 present)
        rule_sum <- sprintf(
          "AND((ISNUMBER($%s2)+ISNUMBER($%s2)+ISNUMBER($%s2))=3,OR($%s2+$%s2+$%s2<99,$%s2+$%s2+$%s2>101))",
          col_sand,
          col_silt,
          col_clay,
          col_sand,
          col_silt,
          col_clay,
          col_sand,
          col_silt,
          col_clay
        )

        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(
            rows = 2:max_row,
            cols = c(idx_sand, idx_silt, idx_clay)
          ),
          type = "expression",
          rule = rule_sum,
          style = "error_style"
        )

        # Warning: two fractions missing + texture class is missing
        rule_insufficient <- sprintf(
          "AND(ISBLANK($%s2), (ISBLANK($%s2)+ISBLANK($%s2)+ISBLANK($%s2))>=2)",
          col_tex,
          col_sand,
          col_silt,
          col_clay
        )

        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx_sand:idx_clay),
          type = "expression",
          rule = rule_insufficient,
          style = "warning_style"
        )

        # Warning: one texture fraction is missing
        for (col_name in texture_cols) {
          idx <- col_index(col_name)

          if (length(idx) == 1) {
            col_letter <- openxlsx2::int2col(idx)

            rule <- sprintf(
              "AND(
        ISBLANK(%s2),
        (ISBLANK(%s2)+ISBLANK(%s2)+ISBLANK(%s2))=1
      )",
              col_letter,
              openxlsx2::int2col(col_index("sand_percent")),
              openxlsx2::int2col(col_index("silt_percent")),
              openxlsx2::int2col(col_index("clay_percent"))
            )

            wb$add_conditional_formatting(
              sheet = "Data",
              dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
              type = "expression",
              rule = rule,
              style = "warning_style"
            )
          }
        }
      }
    }
  }

  # Data dictionary tab --------------------------------------------------------

  if ("Data Dictionary" %in% wb$sheet_names) {
    # Get column headers from Data Dictionary sheet to map names to positions
    dd_headers <- openxlsx2::wb_to_df(
      wb,
      sheet = "Data Dictionary",
      rows = 1,
      col_names = FALSE
    )
    dd_headers <- as.character(unlist(dd_headers[1, ]))

    # Figure out how many rows are in the Data Dictionary sheet
    dd_full <- openxlsx2::wb_to_df(
      wb,
      sheet = "Data Dictionary",
      col_names = TRUE
    )
    max_row <- nrow(dd_full) + 1 # +1 because row 1 is the header
    if (max_row < 2) {
      max_row <- 1000
    } # fallback

    # Helper to get column index by name
    col_index <- function(col_name) {
      which(dd_headers == col_name)
    }

    # 1. Blanks in required columns (missing_allowed == FALSE)
    required_cols <- required_fields |>
      dplyr::filter(missing_allowed == "FALSE", var %in% dd_headers) |>
      dplyr::pull(var)

    for (col_name in required_cols) {
      idx <- col_index(col_name)
      if (length(idx) == 1) {
        wb$add_conditional_formatting(
          sheet = "Data Dictionary",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
          type = "containsBlanks",
          style = "error_style"
        )
      }
    }

    # 2. Duplicate abbr + unit combo
    if (all(c("abbr", "unit") %in% dd_headers)) {
      idx_abbr <- col_index("abbr")
      idx_unit <- col_index("unit")

      if (length(idx_abbr) == 1 && length(idx_unit) == 1) {
        # Convert to Excel column letters
        col_abbr <- openxlsx2::int2col(idx_abbr)
        col_unit <- openxlsx2::int2col(idx_unit)

        # COUNTIFS: same unit + same abbr appears more than once
        rule <- sprintf(
          "COUNTIFS($%s$2:$%s$%d,$%s2,$%s$2:$%s$%d,$%s2)>1",
          col_unit,
          col_unit,
          max_row,
          col_unit,
          col_abbr,
          col_abbr,
          max_row,
          col_abbr
        )

        # Apply only to abbr column
        wb$add_conditional_formatting(
          sheet = "Data Dictionary",
          dims = openxlsx2::wb_dims(
            rows = 2:max_row,
            cols = c(idx_abbr, idx_unit)
          ),
          type = "expression",
          rule = rule,
          style = "error_style"
        )
      }
    }
  }

  openxlsx2::wb_save(wb, output_path, overwrite = TRUE)

  if (interactive()) {
    cli::cli_bullets(c(
      "v" = "Issue report written to {.file {output_path}}",
      "i" = "Click to copy to console and run to open:",
      " " = sprintf(
        "{.run fs::file_show(%s)}",
        shQuote(output_path)
      )
    ))
  }
}
