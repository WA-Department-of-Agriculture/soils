create_issue_xlsx <- function(
  input_path,
  output_path,
  issues
) {
  # Load data ------------------------------------------------------------------
  wb <- openxlsx2::wb_load(input_path)

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
  } else {
    cli::cli_abort("Missing required sheet: Data.")
  }

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
  } else {
    cli::cli_abort("Missing required sheet: Data Dictionary.")
  }

  # Issues tab -----------------------------------------------------------------

  issues <- format_output(issues, "ui")

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
  wb$add_data(sheet = "Issues", x = error_df)

  # Styling --------------------------------------------------------------------

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
      color = openxlsx2::wb_color(hex = "#9C5700")
    )
  }

  # Wrap text
  n <- nrow(error_df) + 1
  issue_dims = openxlsx2::wb_dims(rows = 1:n, cols = 1:2)

  wb$add_cell_style(
    sheet = "Issues",
    dims = issue_dims,
    vertical = "top",
    wrap_text = TRUE
  )

  # Set column widths
  wb$set_col_widths(sheet = "Issues", cols = 1, widths = 12)
  wb$set_col_widths(sheet = "Issues", cols = 2, widths = 120)

  # Style error
  wb$add_dxfs_style(
    name = "error_style",
    font_color = openxlsx2::wb_color(hex = "#9C0006"),
    bg_fill = openxlsx2::wb_color(hex = "#FFC7CE")
  )

  # Style warning
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
        dims = openxlsx2::wb_dims(rows = 2:max_row, cols = idx_field),
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
