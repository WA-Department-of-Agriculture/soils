#' Create an Excel validation report with conditional formatting
#'
#' Generates an Excel workbook that includes:
#' \itemize{
#'   \item Original `Data` and `Data Dictionary` sheets
#'   \item An `Issues` sheet summarizing validation results
#'   \item Conditional formatting to highlight errors and warnings directly in the data
#' }
#'
#' The function supports both Excel and CSV input sources and reconstructs
#' a formatted workbook for review and correction.
#'
#' @param gate_result Named list produced by `read_soils_input()` and
#'   validated by `check_input_structure()`. Must include:
#'   \itemize{
#'     \item `data`: data frame of input data
#'     \item `data_dict`: data frame of data dictionary
#'     \item `source`: `"excel"` or `"csv"`
#'     \item `file`: original input file path(s)
#'   }
#' @param output_path Character. File path where the Excel report will be saved.
#' @param issues List of validation issues produced by `run_all_checks()`
#'   or individual check functions.
#' @param language Character. `"English"` or `"Spanish"`.
#'
#' @returns
#' Writes an Excel file to `output_path`.
#'
#' @details
#' The generated workbook includes:
#'
#' \strong{Issues sheet}
#' \itemize{
#'   \item Guidance text for interpreting issue results
#'   \item Summary of errors and warnings
#'   \item Styled rows by severity
#' }
#'
#' \strong{Data and Data Dictionary sheets}
#' \itemize{
#'   \item Conditional formatting for:
#'     \itemize{
#'       \item Missing required values
#'       \item Duplicate identifiers
#'       \item Invalid data types
#'       \item Out-of-range values (e.g., coordinates, texture fractions)
#'       \item Non-numeric measurement values
#'       \item Mismatches between data and dictionary
#'     }
#' }
#'
#' Conditional formatting mirrors validation rules to provide a
#' spreadsheet-based review and correction workflow.
#'
#' @examples
#' \dontrun{
#' # Example pipeline
#' # Read data
#' input <- read_soils_input("soil-data.xlsx")
#'
#' # Check input structure
#' gate_result <- check_input_structure(input)
#'
#' # Run all validation checks
#' issues <- run_all_checks(gate_result)
#'
#' # If issues exist, create spreadsheet with conditional formatting
#' if (length(issues) > 0) {
#'   create_issue_xlsx(gate_result, issues_file, issues)
#' } else {
#'   cli::cli_alert_success("No issues to report!")
#' }
#' }
#' @export

create_issue_xlsx <- function(
  gate_result,
  output_path,
  issues,
  lanugage = c("English", "Spanish")
) {
  language <- rlang::arg_match(language)
  data_full <- gate_result$data
  dd_full <- gate_result$data_dict

  # Initialize workbook --------------------------------------------------------

  if (gate_result$source == "excel") {
    # Preserve original formatting by loading workbook directly
    wb <- tryCatch(
      openxlsx2::wb_load(gate_result$file),
      error = function(e) {
        cli::cli_abort("Failed to load Excel file: {.file {gate_result$file}}")
      }
    )

    # Validate expected structure exists
    if (!all(c("Data", "Data Dictionary") %in% wb$sheet_names)) {
      cli::cli_abort(
        "Excel file must contain 'Data' and 'Data Dictionary' sheets."
      )
    }
  } else if (gate_result$source == "csv") {
    # Build fresh workbook
    wb <- openxlsx2::wb_workbook()

    wb$add_worksheet("Data")
    wb$add_data(sheet = "Data", x = data_full, na.strings = "")
    wb$set_col_widths(sheet = "Data", cols = 1:ncol(data_full), widths = "auto")

    wb$add_worksheet("Data Dictionary")
    wb$add_data(sheet = "Data Dictionary", x = dd_full, na.strings = "")
    wb$set_col_widths(
      sheet = "Data Dictionary",
      cols = 1:ncol(dd_full),
      widths = "auto"
    )
  } else {
    cli::cli_abort(
      "Unknown input data source type: {.val {gate_result$source}}"
    )
  }

  data_headers <- colnames(data_full)
  dd_headers <- colnames(dd_full)

  # Issues tab -----------------------------------------------------------------

  issues <- format_output(issues, "ui")

  guidance <- c(
    "Errors must be corrected before proceeding.",
    "Warnings indicate potential issues. Processing can continue, but review is recommended."
  )

  error_df <- data.frame(
    Severity = vapply(issues, \(x) x$severity, character(1)),
    Message = vapply(
      issues,
      \(x) paste(x$message, collapse = "\n"),
      character(1)
    ),
    stringsAsFactors = FALSE
  )

  wb$add_worksheet("Issues")
  wb$add_data(sheet = "Issues", x = guidance)
  wb$add_data(sheet = "Issues", x = error_df, start_row = 4)

  # Styling --------------------------------------------------------------------

  # Style the guidance
  wb$add_font(
    sheet = "Issues",
    dims = openxlsx2::wb_dims(rows = 1:2, cols = 1:2),
    italic = TRUE,
    color = openxlsx2::wb_color(hex = "#595959")
  )

  # Style the header row (bold + bottom border)
  wb$add_font(
    sheet = "Issues",
    dims = "A4:B4",
    bold = TRUE
  )
  wb$add_border(
    sheet = "Issues",
    dims = "A4:B4",
    bottom_border = "thin",
    right_border = "none"
  )

  # Style error rows
  error_rows <- which(error_df$Severity == "error") + 4
  if (length(error_rows) > 0) {
    error_dims <- openxlsx2::wb_dims(rows = error_rows, cols = 1:2)
    wb$add_named_style(
      sheet = "Issues",
      dims = error_dims,
      name = "Bad"
    )
    wb$add_border(
      sheet = "Issues",
      dims = error_dims,
      inner_hgrid = "thin",
      right_border = "none"
    )
  }

  # Style warning rows
  warning_rows <- which(error_df$Severity == "warning") + 4
  if (length(warning_rows) > 0) {
    warning_dims <- openxlsx2::wb_dims(rows = warning_rows, cols = 1:2)
    wb$add_named_style(
      sheet = "Issues",
      dims = warning_dims,
      name = "Neutral"
    )
    wb$add_border(
      sheet = "Issues",
      dims = warning_dims,
      inner_hgrid = "thin",
      right_border = "none"
    )
  }

  # Wrap text
  n <- nrow(error_df) + 4
  issue_dims = openxlsx2::wb_dims(rows = 5:n, cols = 1:2)

  wb$add_cell_style(
    sheet = "Issues",
    dims = issue_dims,
    vertical = "top",
    wrap_text = TRUE
  )

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

  max_row <- nrow(data_full) + 1 # +1 because row 1 is the header
  if (max_row < 2) {
    max_row <- 1000
  } # fallback

  # Helper to get column index by name
  col_index <- function(col_name) {
    which(data_headers == col_name)
  }

  ## Blanks in required columns (missing_allowed == FALSE) ---------------------
  required_cols <- required_fields |>
    dplyr::filter(missing_allowed == FALSE, var %in% data_headers) |>
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

  ## Duplicate sample_id -------------------------------------------------------

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

  ## Duplicate field_id within producer_id + year combo ------------------------

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
        dims = openxlsx2::wb_dims(
          rows = 2:max_row,
          cols = c(idx_year, idx_prod, idx_field)
        ),
        type = "expression",
        rule = rule,
        style = "error_style"
      )
    }
  }

  ## Data columns not in dictionary --------------------------------------------

  measurement_cols <- dd_full |>
    dplyr::pull(column_name) |>
    trimws()

  measurement_cols <- measurement_cols[
    !is.na(measurement_cols) &
      measurement_cols != ""
  ]

  extra_cols <- setdiff(colnames(data_full), required_fields$var)
  extra_cols <- setdiff(extra_cols, measurement_cols)

  for (col_name in extra_cols) {
    idx <- col_index(col_name)
    if (length(idx) == 1) {
      wb$add_fill(
        sheet = "Data",
        dims = openxlsx2::wb_dims(rows = 1, cols = idx),
        color = openxlsx2::wb_color(hex = "#FFEB9C")
      )
    }
  }

  ## Wrong data type -----------------------------------------------------------

  check_fields <- required_fields |>
    dplyr::filter(type == "data") |>
    dplyr::filter(var %in% data_headers, !is.na(var_type))

  for (i in seq_len(nrow(check_fields))) {
    col_name <- check_fields$var[i]
    expected_type <- tolower(check_fields$var_type[i])

    idx <- col_index(col_name)

    if (length(idx) == 1) {
      col_letter <- openxlsx2::int2col(idx)

      # Excel rules
      rule <- switch(
        expected_type,

        "numeric" = sprintf(
          "AND($%s2<>\"\",NOT(ISNUMBER($%s2)))",
          col_letter,
          col_letter
        ),

        "character" = sprintf(
          "AND($%s2<>\"\",ISNUMBER($%s2))",
          col_letter,
          col_letter
        ),

        NULL
      )

      if (!is.null(rule)) {
        wb$add_conditional_formatting(
          sheet = "Data",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
          type = "expression",
          rule = rule,
          style = "error_style"
        )
      }
    }
  }

  ## Non-numeric values in measurement columns ---------------------------------

  measurement_cols <- intersect(measurement_cols, names(data_full))
  measurement_cols <- setdiff(measurement_cols, "texture")

  for (col_name in measurement_cols) {
    idx <- col_index(col_name)

    if (length(idx) == 1) {
      col_letter <- openxlsx2::int2col(idx)

      rule_non_numeric <- sprintf(
        "AND(%s2<>\"\",NOT(ISNUMBER(%s2)))",
        col_letter,
        col_letter
      )

      wb$add_conditional_formatting(
        sheet = "Data",
        dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
        type = "expression",
        rule = rule_non_numeric,
        style = "warning_style"
      )
    }
  }

  ## Texture fraction validation -----------------------------------------------

  texture_cols <- c("sand_percent", "silt_percent", "clay_percent")

  if (all(texture_cols %in% data_headers)) {
    idx_sand <- col_index("sand_percent")
    idx_silt <- col_index("silt_percent")
    idx_clay <- col_index("clay_percent")
    idx_tex <- col_index("texture")

    if (all(lengths(list(idx_sand, idx_silt, idx_clay)) == 1)) {
      col_sand <- openxlsx2::int2col(idx_sand)
      col_silt <- openxlsx2::int2col(idx_silt)
      col_clay <- openxlsx2::int2col(idx_clay)
      col_tex <- if (length(idx_tex) == 1) {
        openxlsx2::int2col(idx_tex)
      } else {
        NA_character_
      }

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
        ISBLANK($%s2),
        (ISBLANK($%s2)+ISBLANK($%s2)+ISBLANK($%s2))=1
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

  ## Coordinate validation ------------------------------------------------------

  idx_lat <- which(data_headers == "latitude")
  idx_lon <- which(data_headers == "longitude")

  if (length(idx_lat) == 1) {
    col_lat <- openxlsx2::int2col(idx_lat)

    # Latitude out of range
    rule_lat <- sprintf(
      "AND(%s2<>\"\",OR(%s2<-90,%s2>90))",
      col_lat,
      col_lat,
      col_lat
    )

    wb$add_conditional_formatting(
      sheet = "Data",
      dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx_lat),
      type = "expression",
      rule = rule_lat,
      style = "error_style"
    )
  }

  if (length(idx_lon) == 1) {
    col_lon <- openxlsx2::int2col(idx_lon)

    # Longitude out of range
    rule_lon <- sprintf(
      "AND(%s2<>\"\",OR(%s2<-180,%s2>180))",
      col_lon,
      col_lon,
      col_lon
    )

    wb$add_conditional_formatting(
      sheet = "Data",
      dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx_lon),
      type = "expression",
      rule = rule_lon,
      style = "error_style"
    )
  }

  # One coordinate missing (but not both)
  if (length(idx_lat) == 1 && length(idx_lon) == 1) {
    col_lat <- openxlsx2::int2col(idx_lat)
    col_lon <- openxlsx2::int2col(idx_lon)

    rule_missing_pair <- sprintf(
      "AND(OR(%s2=\"\",%s2=\"\"),NOT(AND(%s2=\"\",%s2=\"\")))",
      col_lat,
      col_lon,
      col_lat,
      col_lon
    )

    wb$add_conditional_formatting(
      sheet = "Data",
      dims = openxlsx2::wb_dims(rows = 2:max_row, cols = c(idx_lat, idx_lon)),
      type = "expression",
      rule = rule_missing_pair,
      style = "error_style"
    )
  }

  # Data dictionary tab --------------------------------------------------------

  max_row <- nrow(dd_full) + 1 # +1 because row 1 is the header
  if (max_row < 2) {
    max_row <- 1000
  } # fallback

  # Helper to get column index by name
  col_index <- function(col_name) {
    which(dd_headers == col_name)
  }

  ## Blanks in required columns (missing_allowed == FALSE) ---------------------
  required_cols <- required_fields |>
    dplyr::filter(missing_allowed == FALSE, var %in% dd_headers) |>
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

  ## Duplicate abbr + unit combo -----------------------------------------------
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

  ## Dictionary rows not in data -----------------------------------------------

  if ("column_name" %in% dd_headers) {
    idx <- which(dd_headers == "column_name")
    col_letter <- openxlsx2::int2col(idx)

    rule <- sprintf(
      "ISNA(MATCH(%s2,Data!$1:$1,0))",
      col_letter
    )

    wb$add_conditional_formatting(
      sheet = "Data Dictionary",
      dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
      type = "expression",
      rule = rule,
      style = "warning_style"
    )
  }

  ## Wrong data type -----------------------------------------------------------

  check_fields <- required_fields |>
    dplyr::filter(type == "data dictionary") |>
    dplyr::filter(var %in% dd_headers, !is.na(var_type))

  for (i in seq_len(nrow(check_fields))) {
    col_name <- check_fields$var[i]
    expected_type <- tolower(check_fields$var_type[i])

    idx <- col_index(col_name)

    if (length(idx) == 1) {
      col_letter <- openxlsx2::int2col(idx)

      # Excel rules
      rule <- switch(
        expected_type,

        "numeric" = sprintf(
          "AND($%s2<>\"\",NOT(ISNUMBER($%s2)))",
          col_letter,
          col_letter
        ),

        "character" = sprintf(
          "AND($%s2<>\"\",ISNUMBER($%s2))",
          col_letter,
          col_letter
        ),

        NULL
      )

      if (!is.null(rule)) {
        wb$add_conditional_formatting(
          sheet = "Data Dictionary",
          dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
          type = "expression",
          rule = rule,
          style = "error_style"
        )
      }
    }
  }

  ## Invalid measurement groups -----------------------------------------------

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

  if ("measurement_group" %in% dd_headers) {
    idx <- which(dd_headers == "measurement_group")

    if (length(idx) == 1) {
      col_letter <- openxlsx2::int2col(idx)

      # Add reference sheet with valid groups
      wb$add_worksheet("Reference", visible = FALSE)
      wb$add_data(sheet = "Reference", x = data.frame(groups = valid))
      valid_rows <- length(valid) + 1
      wb$add_named_region(
        sheet = "Reference",
        dims = openxlsx2::wb_dims(cols = "A", rows = 2:valid_rows),
        name = "valid_groups"
      )

      rule_invalid_group <- sprintf(
        "AND(%s2<>\"\",ISNA(MATCH(%s2,valid_groups,0)))",
        col_letter,
        col_letter
      )

      wb$add_conditional_formatting(
        sheet = "Data Dictionary",
        dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx),
        type = "expression",
        rule = rule_invalid_group,
        style = "warning_style"
      )
    }
  }

  # Save -----------------------------------------------------------------------

  openxlsx2::wb_save(wb, output_path, overwrite = TRUE)

  if (interactive()) {
    cli::cli_bullets(c(
      "!" = "Review issue report at {.file {output_path}}",
      "i" = "Click to copy to console and run to open:",
      " " = sprintf(
        "{.run fs::file_show(%s)}",
        shQuote(output_path)
      )
    ))
  }
}
