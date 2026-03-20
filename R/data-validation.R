# ============================================================================
# data_validation.R — POC for soils package validation pattern
#
# This file demonstrates the proposed architecture for validation functions
# that serve both CLI (soils package) and UI (Shiny app) consumers.
#
# Organization:
#   1. Utilities  — new_issue(), format_output(), split_issues()
#   2. Gate check — check_file_readable()
#   3. Checks     — one function per validation check
#
# PROPOSED FOR SOILS PACKAGE: Everything in this file would eventually live
# in the soils package as individual exported functions (one file per
# function). The Shiny app would call them with output = "ui".
#
# For the POC, source this file and use the demo script to see both outputs.
# ============================================================================

# --- 1. Utilities -----------------------------------------------------------

#' Create a validation issue
#'
#' @param severity "error" or "warning"
#' @param message A character string describing the issue
#'
#' @return A named list with severity and message.
new_issue <- function(severity = c("error", "warning"), message) {
  severity <- match.arg(severity)
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
  output <- match.arg(output)

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
      cli::cli_abort(c("x" = "Data validation failed.", bullets))
    }

    if (length(warnings) > 0) {
      bullets <- cli::ansi_strip(
        vapply(warnings, \(x) x$message, character(1))
      )
      names(bullets) <- rep("*", length(bullets))
      cli::cli_warn(c(
        "!" = "Data validation completed with warnings.",
        bullets
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

#' Check that a file is readable and return loaded data
#'
#' Runs the blocking checks in sequence: required sheets exist, no duplicate
#' headers, both sheets load, and data has rows. If all pass, returns the
#' loaded data frames. If any fail, returns the issue list.
#'
#' @param file Path to the .xlsx file.
#' @param output "cli" (default) or "ui".
#'
#' @return On success: a named list with $data and $data_dict data frames.
#'   On failure: a list of issues (same structure as other check functions).
#'   Use is_gate_pass() to check which one you got back.
check_file_readable <- function(file, output = c("cli", "ui")) {
  output <- match.arg(output)
  issues <- list()

  # Check 1: Required sheets
  sheets <- readxl::excel_sheets(file)
  required <- c("Data", "Data Dictionary")
  missing <- setdiff(required, sheets)

  if (length(missing) > 0) {
    msg <- cli::format_inline("Missing required sheets: {.val {missing}}")
    issues <- c(issues, list(new_issue("error", msg)))
    return(format_output(issues, output))
  }

  # Check 2: Duplicate headers in Data sheet
  tryCatch(
    {
      headers <- as.character(
        readxl::read_xlsx(file, sheet = "Data", col_names = FALSE, n_max = 1)[
          1,
        ]
      )
      headers <- headers[!is.na(headers)]
      dups <- unique(headers[duplicated(headers)])

      if (length(dups) > 0) {
        msg <- cli::format_inline(
          "Duplicate column headers in Data sheet: {.val {dups}}"
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    },
    error = function(e) NULL
  )

  # Check 2b: Duplicate headers in Data Dictionary sheet
  tryCatch(
    {
      headers <- as.character(
        readxl::read_xlsx(
          file,
          sheet = "Data Dictionary",
          col_names = FALSE,
          n_max = 1
        )[1, ]
      )
      headers <- headers[!is.na(headers)]
      dups <- unique(headers[duplicated(headers)])

      if (length(dups) > 0) {
        msg <- cli::format_inline(
          "Duplicate column headers in Data Dictionary sheet: {.val {dups}}"
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    },
    error = function(e) NULL
  )

  if (length(issues) > 0) {
    return(format_output(issues, output))
  }

  # Check 3: Load both sheets
  data <- tryCatch(
    readxl::read_excel(file, sheet = "Data"),
    error = function(e) NULL
  )
  data_dict <- tryCatch(
    readxl::read_excel(file, sheet = "Data Dictionary"),
    error = function(e) NULL
  )

  if (is.null(data) || is.null(data_dict)) {
    msg <- cli::format_inline(
      "Could not read Data or Data Dictionary sheet."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(format_output(issues, output))
  }

  # Check 4: Data has rows
  if (nrow(data) == 0) {
    msg <- cli::format_inline(
      "The Data sheet contains headers but no data rows. Please add your measurement data."
    )
    issues <- c(issues, list(new_issue("error", msg)))
    return(format_output(issues, output))
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

#' Check 3: Required columns in Data
check_required_columns <- function(
  data,
  req_fields_data,
  output = c("cli", "ui")
) {
  output <- match.arg(output)
  issues <- list()

  required <- req_fields_data$var
  missing <- setdiff(required, colnames(data))

  if (length(missing) > 0) {
    msg <- cli::format_inline(
      "Missing required columns in Data: {.val {missing}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  format_output(issues, output)
}

#' Check 4: Required fields in Data Dictionary
check_required_dict_fields <- function(
  data_dict,
  req_fields_dd,
  output = c("cli", "ui")
) {
  output <- match.arg(output)
  issues <- list()

  required <- req_fields_dd$var
  missing <- setdiff(required, colnames(data_dict))

  if (length(missing) > 0) {
    msg <- cli::format_inline(
      "Missing required fields in Data Dictionary: {.val {missing}}"
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  format_output(issues, output)
}

#' Check 5: Uniqueness constraints
check_uniqueness <- function(data, req_fields_data, output = c("cli", "ui")) {
  output <- match.arg(output)
  issues <- list()

  unique_checks <- req_fields_data |> dplyr::filter(unique_by != "-")

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

    if (
      !var_name %in% colnames(data) ||
        !all(group_by_vars %in% colnames(data))
    ) {
      next
    }

    if (length(group_by_vars) == 1 && group_by_vars == var_name) {
      # Globally unique
      duplicates <- data |>
        dplyr::count(!!rlang::sym(var_name)) |>
        dplyr::filter(n > 1)

      if (nrow(duplicates) > 0) {
        dup_vals <- duplicates[[var_name]]
        msg <- cli::format_inline(
          "Duplicate values in {.field {var_name}} (must be unique): {.val {head(dup_vals, 5)}}"
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    } else {
      # Unique within groups
      duplicates <- data |>
        dplyr::group_by(dplyr::across(dplyr::all_of(group_by_vars))) |>
        dplyr::add_count(!!rlang::sym(var_name), name = "field_count") |>
        dplyr::filter(field_count > 1) |>
        dplyr::distinct(
          dplyr::across(dplyr::all_of(c(group_by_vars, var_name)))
        ) |>
        dplyr::ungroup()

      if (nrow(duplicates) > 0) {
        group_str <- paste(group_by_vars, collapse = ", ")
        msg <- cli::format_inline(
          "Duplicate values in {.field {var_name}} within grouping ({group_str}). {nrow(duplicates)} duplicate row{?s} found."
        )
        issues <- c(issues, list(new_issue("error", msg)))
      }
    }
  }

  format_output(issues, output)
}

#' Check 7: Data types match requirements
check_data_types <- function(data, req_fields_data, output = c("cli", "ui")) {
  output <- match.arg(output)
  issues <- list()

  map_r_type <- function(data_type) {
    dplyr::case_when(
      data_type == "int" ~ "integer",
      data_type == "double" ~ "double",
      data_type == "char" ~ "character",
      data_type == "-" ~ "any",
      TRUE ~ "unknown"
    )
  }

  check_fields <- req_fields_data |>
    dplyr::filter(var %in% colnames(data), var_type != "-")

  if (nrow(check_fields) == 0) {
    return(format_output(issues, output))
  }

  non_blank <- sapply(
    data[check_fields$var],
    \(col) !all(is.na(col))
  )

  if (!any(non_blank)) {
    return(format_output(issues, output))
  }

  actual_types <- sapply(
    data[, names(non_blank)[non_blank], drop = FALSE],
    typeof
  )

  mismatched <- check_fields |>
    dplyr::filter(var %in% names(actual_types)) |>
    dplyr::filter(map_r_type(var_type) != actual_types[var]) |>
    dplyr::mutate(actual_type = actual_types[var])

  if (nrow(mismatched) > 0) {
    for (i in seq_len(nrow(mismatched))) {
      r <- mismatched[i, ]
      msg <- cli::format_inline(
        "{.field {r$var}} has wrong data type (expected {.val {r$var_type}}, found {.val {r$actual_type}})."
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  format_output(issues, output)
}

#' Check 8: Missing values in required columns
check_missing_values <- function(
  data,
  req_fields_data,
  output = c("cli", "ui")
) {
  output <- match.arg(output)
  issues <- list()

  required_cols <- req_fields_data |>
    dplyr::filter(missing_allowed == "FALSE") |>
    dplyr::filter(var %in% colnames(data)) |>
    dplyr::pull(var)

  for (col in required_cols) {
    n_missing <- sum(is.na(data[[col]]))

    if (n_missing > 0) {
      msg <- cli::format_inline(
        "{.field {col}} has {n_missing} missing value{?s}. This column does not allow blank values."
      )
      issues <- c(issues, list(new_issue("error", msg)))
    }
  }

  format_output(issues, output)
}


# --- 3. Independent checks (WARNINGS) --------------------------------------

#' Check 6: Data has at least one column beyond required fields
check_additional_columns <- function(
  data,
  req_fields_data,
  output = c("cli", "ui")
) {
  output <- match.arg(output)
  issues <- list()

  required <- req_fields_data$var
  additional <- setdiff(colnames(data), required)

  if (length(additional) < 1) {
    msg <- cli::format_inline(
      "The Data sheet has no columns beyond the required fields. Add at least one measurement column."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  format_output(issues, output)
}

#' Check 8.5: Percent columns within 0-100
check_percent_range <- function(data, output = c("cli", "ui")) {
  output <- match.arg(output)
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

  format_output(issues, output)
}


check_dict_mismatch <- function(
  data,
  data_dict,
  req_fields_data,
  output = c("cli", "ui")
) {
  output <- match.arg(output)
  issues <- list()

  # Guard: can't run this check without column_name in the dictionary
  if (!"column_name" %in% colnames(data_dict)) {
    return(format_output(issues, output))
  }

  required <- req_fields_data$var
  additional <- setdiff(colnames(data), required)
  dict_names <- data_dict$column_name

  missing_in_dict <- setdiff(additional, dict_names)
  missing_in_data <- setdiff(dict_names, colnames(data))

  if (length(missing_in_dict) > 0) {
    msg <- cli::format_inline(
      "Columns in Data not documented in Data Dictionary: {.val {missing_in_dict}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  if (length(missing_in_data) > 0) {
    msg <- cli::format_inline(
      "Columns in Data Dictionary not found in Data: {.val {missing_in_data}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  format_output(issues, output)
}

#' Check 10: Valid measurement groups
check_measurement_groups <- function(
  data_dict,
  language = "english",
  output = c("cli", "ui")
) {
  output <- match.arg(output)
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
      "Missing {.field measurement_group} column in Data Dictionary."
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
      "Invalid measurement_group values: {.val {invalid}}. Valid options for {lang_label}: {.val {valid}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  format_output(issues, output)
}

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
