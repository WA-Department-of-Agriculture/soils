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

#' Format validation issues for CLI or UI output
#'
#' For CLI: errors trigger cli_abort(), warnings trigger cli_warn().
#' For UI: ANSI codes are stripped, issue list is returned with clean strings.
#'
#' @param issues List of issues created by new_issue().
#' @param output "cli" (default) for console, "ui" for Shiny.
#'
#' @return For "ui", cleaned issue list. For "cli", invisible after
#'   printing/aborting.
format_output <- function(issues, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  if (length(issues) == 0) {
    return(issues)
  }

  if (output == "cli") {
    errors <- Filter(\(x) x$severity == "error", issues)
    warnings <- Filter(\(x) x$severity == "warning", issues)

    if (length(errors) > 0) {
      bullets <- cli::ansi_strip(
        vapply(errors, \(x) x$message, character(1))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_abort(c("x" = "Data validation failed.", bullets), call = NULL)
    }

    if (length(warnings) > 0) {
      bullets <- cli::ansi_strip(
        vapply(warnings, \(x) x$message, character(1))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_warn(c(
        "!" = "Data validation completed with warnings.",
        bullets,
        call = NULL
      ))
    }

    return(invisible(issues))
  }

  # UI: strip ANSI codes from messages
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


# --- 2. Gate check ----------------------------------------------------------

#' Check that file input(s) are readable and return loaded data
#'
#' Runs the blocking checks in sequence: required sheets (.xlsx) or files (.csv)
#' exist, no duplicate headers, both sheets load, and data has rows. If all
#' pass, returns the loaded data frames. If any fail, returns the issue list.
#'
#' @param file Path to the .xlsx file (must contain sheets "Data" and "Data
#'   Dictionary" Or a character vector of length 2 with paths to .csv files
#'   named {.file data.csv} and {.file data-dictionary.csv} (in that order).
#' @param output "cli" (default) or "ui".
#'
#' @return On success: a named list with $data and $data_dict data frames. On
#'   failure: a list of issues (same structure as other check functions). Use
#'   is_gate_pass() to check which one you got back.
check_file_readable <- function(file, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  # Detect file type
  is_excel <- length(file) == 1 && grepl("\\.xlsx$", file, ignore.case = TRUE)
  is_csv <- all(grepl("\\.csv$", file, ignore.case = TRUE))

  if (!is_excel && !is_csv) {
    msg <- cli::format_inline(
      "Unsupported file type. Provide a .xlsx file or .csv files."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  # xlsx file ------------------------------------------------------------------

  if (is_excel) {
    # Check 1: Required sheets "Data" and "Data Dictionary"
    sheets <- readxl::excel_sheets(file)
    required <- c("Data", "Data Dictionary")
    missing <- setdiff(required, sheets)

    if (length(missing) > 0) {
      msg <- cli::format_inline("Missing required sheets: {.val {missing}}")
      issues <- c(issues, list(new_issue("error", msg)))
      return(issues)
    }

    # Load both sheets
    data <- tryCatch(
      openxlsx::read.xlsx(file, sheet = "Data"),
      error = function(e) NULL
    )
    data_dict <- tryCatch(
      openxlsx::read.xlsx(file, sheet = "Data Dictionary"),
      error = function(e) NULL
    )

    if (is.null(data) || is.null(data_dict)) {
      msg <- cli::format_inline(
        "Could not read one or both of {.val Data} or {.val Data Dictionary} sheet(s)."
      )
      issues <- c(issues, list(new_issue("error", msg)))
      return(issues)
    }
  }

  # csv files ------------------------------------------------------------------

  if (is_csv) {
    # Check 1: Required two csv files: data and dictionary
    if (length(file) != 2) {
      msg <- cli::format_inline(
        ".csv input requires exactly two files: {.val data.csv} and {.val data_dictionary.csv}."
      )
      issues <- c(issues, list(new_issue("error", msg)))
      return(issues)
    }

    file_names <- basename(file)

    # Check naming convention
    if (!grepl("data", file_names[1], ignore.case = TRUE)) {
      msg <- cli::format_inline(
        "The first .csv file must be the data file and its name must contain {.envvar data}. You provided: {.file {file_names[1]}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (!grepl("dictionary", file_names[2], ignore.case = TRUE)) {
      msg <- cli::format_inline(
        "The second .csv file must be the data dictionary and its name must contain {.val dictionary}. You provided: {.file {file_names[2]}}"
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }

    if (length(issues) > 0) {
      return(issues)
    }

    # Try loading both
    data <- tryCatch(
      read.csv(
        file[1],
        check.names = FALSE,
        # Set encoding for using subscripts, superscripts, special characters
        encoding = "UTF-8",
        strip.white = TRUE
      ),
      error = function(e) NULL
    )

    data_dict <- tryCatch(
      read.csv(
        file[2],
        check.names = FALSE,
        # Set encoding for using subscripts, superscripts, special characters
        encoding = "UTF-8",
        strip.white = TRUE
      ),
      error = function(e) NULL
    )

    if (is.null(data) || is.null(data_dict)) {
      return(issues)
    }
  }

  # Shared checks --------------------------------------------------------------

  # Check 2a: Duplicate headers (data)
  dups <- names(data)[duplicated(names(data))]
  if (length(dups) > 0) {
    msg <- cli::format_inline(
      "Duplicate column headers in {.envvar data}: {.val {dups}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Check 2b: Duplicate headers (data dictionary)
  dups <- names(data_dict)[duplicated(names(data_dict))]
  if (length(dups) > 0) {
    msg <- cli::format_inline(
      "Duplicate column headers in {.envvar dictionary}: {.val {dups}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Check 3: Data has rows
  if (nrow(data) == 0) {
    msg <- cli::format_inline(
      "{.envvar data} contains headers but no rows. Please add your measurement data."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(issues)
  }

  if (length(issues) > 0) {
    return(issues)
  }

  # All gates passed — return the loaded data
  list(data = data, data_dict = data_dict)
}

#' Check if the gate returned loaded data (pass) or issues (fail)
#'
#' @param gate_result The return value from check_file_readable().
#'
#' @return TRUE if the gate passed (data frames returned), FALSE if issues.
is_gate_pass <- function(gate_result) {
  !is.null(gate_result$data) && !is.null(gate_result$data_dict)
}

# --- 3. Independent checks (ERRORS) ----------------------------------------

#' Check 4: Required columns
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

#' Check 5: Uniqueness constraints
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

#' Check 6: Data types match requirements
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

#' Check 7: Missing values in required columns
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

# --- 4. Independent checks (WARNINGS) --------------------------------------

#' Check 6: Data has at least one column beyond required fields
check_additional_columns <- function(data) {
  issues <- list()

  required <- required_fields |>
    dplyr::filter(type == "data") |>
    dplyr::pull(var)

  additional <- setdiff(colnames(data), required)

  if (length(additional) < 1) {
    msg <- cli::format_inline(
      "{.envvar data} has no columns beyond the required columns. Add at least one measurement column."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

#' Check 8.5: Percent columns within 0-100
check_percent_range <- function(data) {
  issues <- list()

  percent_cols <- intersect(
    c("sand_percent", "silt_percent", "clay_percent"),
    colnames(data)
  )

  for (col in percent_cols) {
    bad <- !is.na(data[[col]]) & (data[[col]] < 0 | data[[col]] > 100)

    if (any(bad)) {
      if ("sample_id" %in% colnames(data)) {
        bad_ids <- data$sample_id[bad]
        bad_ids <- bad_ids[!is.na(bad_ids)]
      } else {
        bad_ids <- character(0)
      }

      if (length(bad_ids) > 0 && length(bad_ids) <= 5) {
        msg <- cli::format_inline(
          "{.field {col}} has values outside [0, 100]. Check sample IDs: {.val {bad_ids}}"
        )
      } else if (length(bad_ids) > 5) {
        msg <- cli::format_inline(
          "{.field {col}} has values outside [0, 100]. {length(bad_ids)} samples are out of range."
        )
      } else {
        msg <- cli::format_inline(
          "{.field {col}} has values outside [0, 100]. {sum(bad)} row{?s} affected."
        )
      }

      issues <- c(issues, list(new_issue("warning", msg)))
    }
  }

  return(issues)
}

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

#' Check 10: Valid measurement groups
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
    return(format_output(issues, output))
  }

  valid <- enc2native(measurement_groups[[language]])

  if (!"measurement_group" %in% colnames(data_dict)) {
    msg <- cli::format_inline(
      "Missing {.field measurement_group} column in {.envvar dictionary}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
    return(format_output(issues, output))
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

  format_output(issues, output)
}

# --- 5. Excel spreadsheet with issues -----------------------------------------

create_error_xlsx <- function(
  input_path,
  output_path,
  issues,
  req_fields_data = NULL
) {
  wb <- openxlsx::loadWorkbook(input_path)

  # --- Errors tab ---

  error_df <- data.frame(
    Severity = vapply(issues, \(x) x$severity, character(1)),
    Message = vapply(issues, \(x) x$message, character(1)),
    stringsAsFactors = FALSE
  )

  openxlsx::addWorksheet(wb, "Errors")
  openxlsx::writeData(wb, "Errors", error_df)

  # Style the header row
  header_style <- openxlsx::createStyle(
    textDecoration = "bold",
    border = "Bottom",
    borderStyle = "thin"
  )
  openxlsx::addStyle(
    wb,
    "Errors",
    style = header_style,
    rows = 1,
    cols = 1:2
  )

  # Style error rows (red fill)
  error_rows <- which(error_df$Severity == "error") + 1
  if (length(error_rows) > 0) {
    error_style <- openxlsx::createStyle(
      fontColour = "#9C0006"
    )
    openxlsx::addStyle(
      wb,
      "Errors",
      style = error_style,
      rows = error_rows,
      cols = 1:2,
      gridExpand = TRUE
    )
  }

  # Style warning rows (yellow fill)
  warning_rows <- which(error_df$Severity == "warning") + 1
  if (length(warning_rows) > 0) {
    warning_style <- openxlsx::createStyle(
      fontColour = "#9C6500"
    )
    openxlsx::addStyle(
      wb,
      "Errors",
      style = warning_style,
      rows = warning_rows,
      cols = 1:2,
      gridExpand = TRUE
    )
  }

  openxlsx::setColWidths(wb, "Errors", cols = 1, widths = 12)
  openxlsx::setColWidths(wb, "Errors", cols = 2, widths = 80)

  # --- Conditional formatting on Data sheet ---

  if (!is.null(req_fields_data) && "Data" %in% names(wb)) {
    # Get column headers from Data sheet to map names to positions
    data_headers <- openxlsx::read.xlsx(
      wb,
      sheet = "Data",
      rows = 1,
      colNames = FALSE
    )
    data_headers <- as.character(data_headers[1, ])

    # Number of data rows (excluding header)
    n_rows <- wb$worksheets[[which(names(wb) == "Data")]]$sheet_data$rows
    max_row <- max(as.numeric(n_rows), na.rm = TRUE)
    if (max_row < 2) {
      max_row <- 1000
    } # fallback

    # Helper to get column index by name
    col_index <- function(col_name) {
      which(data_headers == col_name)
    }

    #1. Blanks in required columns (missing_allowed == FALSE)
    required_cols <- req_fields_data |>
      dplyr::filter(missing_allowed == "FALSE", var %in% data_headers) |>
      dplyr::pull(var)

    for (col_name in required_cols) {
      idx <- col_index(col_name)
      if (length(idx) == 1) {
        openxlsx::conditionalFormatting(
          wb,
          "Data",
          cols = idx,
          rows = 2:max_row,
          type = "blanks"
        )
      }
    }

    # 2. Percent columns outside 0-100
    percent_cols <- intersect(
      c("sand_percent", "silt_percent", "clay_percent"),
      data_headers
    )

    for (col_name in percent_cols) {
      idx <- col_index(col_name)
      if (length(idx) == 1) {
        col_letter <- openxlsx::int2col(idx)

        # Values < 0
        openxlsx::conditionalFormatting(
          wb,
          "Data",
          cols = idx,
          rows = 2:max_row,
          type = "expression",
          rule = paste0(col_letter, "2<0")
        )

        # Values > 100
        openxlsx::conditionalFormatting(
          wb,
          "Data",
          cols = idx,
          rows = 2:max_row,
          type = "expression",
          rule = paste0(col_letter, "2>100")
        )
      }
    }

    # 3. Duplicate sample_id
    if ("sample_id" %in% data_headers) {
      idx <- col_index("sample_id")
      if (length(idx) == 1) {
        openxlsx::conditionalFormatting(
          wb,
          "Data",
          cols = idx,
          rows = 2:max_row,
          type = "duplicates"
        )
      }
    }
  }

  openxlsx::saveWorkbook(wb, output_path, overwrite = TRUE)
}
