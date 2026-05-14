#' Check soil texture fractions
#'
#' Validates soil particle-size fractions (sand, silt, and clay) in the input
#' dataset. Requires a `sample_id` column and at least two of `sand_percent`,
#' `silt_percent`, and `clay_percent` to perform validation.
#'
#' This function does not modify the data. It returns a list of validation
#' issues that can be combined with other `check_*()` functions and formatted
#' using `format_issues()`.
#'
#' Validation rules:
#' \itemize{
#'   \item Warns if fewer than two fraction columns are provided (texture
#'   validation cannot be performed).
#'   \item Warns when exactly one fraction is missing (it can be computed later).
#'   \item Errors if any fraction values are outside the range 0–100.
#'   \item Errors if all three fractions are present but do not sum to 100
#'   (&plusmn;1 tolerance).
#' }
#'
#' @param df A data frame containing the columns `sample_id`,
#'   `sand_percent`, `silt_percent`, and `clay_percent`. An optional
#'   `texture` column can also be provided.
#'
#' @return A list of issues (errors and/or warnings). Returns an empty list if
#'   no issues are found.
#'
#' @keywords internal
check_texture_fractions <- function(df) {
  issues <- list()

  if (!"sample_id" %in% names(df)) {
    msg <- cli::format_inline(
      "Data must contain {.field sample_id}."
    )
    return(list(new_issue("error", msg)))
  }

  # Setup ---------------------------------------------------------------------

  fraction_cols <- c("sand_percent", "silt_percent", "clay_percent")
  present <- intersect(fraction_cols, names(df))
  missing <- setdiff(fraction_cols, present)

  # Not enough fractions (warning) ---------------------------------------------

  if (length(present) < 2) {
    msg <- cli::format_inline(
      "Provide at least two of {.field sand_percent}, {.field silt_percent}, {.field clay_percent} to enable texture validation."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
    return(issues)
  }

  # Fractions are not numeric (warning) ----------------------------------------
  non_numeric_cols <- present[
    !purrr::map_lgl(df[present], function(col) {
      is.numeric(col) || all(is.na(col))
    })
  ]

  if (length(non_numeric_cols) > 0) {
    msg <- cli::format_inline(
      "{.field {non_numeric_cols}} must be numeric to validate texture."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
    return(issues)
  }

  # Prepare calculations -------------------------------------------------------

  df_calc <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(present), round)
    )

  df_calc$missing_n <- rowSums(is.na(df_calc[present])) +
    length(missing)

  has_texture <- "texture" %in% names(df_calc)

  texture_provided <- if (has_texture) !is.na(df_calc$texture) else FALSE

  # Identify problem rows -----------------------------------------------------

  insufficient_ids <- df_calc$sample_id[
    df_calc$missing_n >= 2 & !texture_provided
  ]

  compute_ids <- df_calc$sample_id[df_calc$missing_n == 1]

  out_of_range_ids <- df_calc |>
    dplyr::filter(
      dplyr::if_any(
        dplyr::all_of(present),
        ~ !is.na(.) & (. < 0 | . > 100)
      )
    ) |>
    dplyr::pull(sample_id)

  invalid_sum_ids <- character(0)

  # All 3 fractions present -> must sum to 100 +/- 1
  if (length(present) == 3) {
    invalid_sum_ids <- df_calc$sample_id[
      df_calc$missing_n == 0 &
        (rowSums(df_calc[present]) < 99 |
          rowSums(df_calc[present]) > 101)
    ]
  }

  # Exactly 2 fractions present -> implied third fraction
  # must remain between 0 and 100
  if (length(present) == 2) {
    invalid_sum_ids <- df_calc$sample_id[
      rowSums(df_calc[present], na.rm = TRUE) > 100
    ]
  }

  # Errors --------------------------------------------------------------------

  if (length(out_of_range_ids) > 0) {
    msg <- c(
      "Texture fractions must be between 0 and 100.",
      "Affected samples:",
      cli::format_inline("{.val {soils_cli_vec(out_of_range_ids)}}")
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(invalid_sum_ids) > 0) {
    msg <- c(
      "Texture fractions must sum to 100 (+/- 1).",
      "Affected samples:",
      cli::format_inline("{.val {soils_cli_vec(invalid_sum_ids)}}")
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Warnings ------------------------------------------------------------------

  if (length(insufficient_ids) > 0) {
    msg <- c(
      "At least two texture fractions must be provided to classify texture class.",
      "Affected samples:",
      cli::format_inline("{.val {soils_cli_vec(insufficient_ids)}}")
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  if (length(compute_ids) > 0) {
    msg <- cli::format_inline(
      "One texture fraction ({.field sand_percent}, {.field silt_percent}, or {.field clay_percent}) is missing and will be computed as 100 minus the sum of the other two.",
      "\nAffected samples:",
      "\n{.val {soils_cli_vec(compute_ids)}}"
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  return(issues)
}

#' Complete missing soil texture fractions
#'
#' Internal helper that computes a missing soil fraction (sand, silt, or clay)
#' when exactly one is missing, using `100 - (sum of the other two)`.
#'
#' @param df A data frame containing `sand_percent`,
#'   `silt_percent`, and `clay_percent.`
#'
#' @return A data frame with completed soil fraction percentages.
#'
#' @keywords internal
complete_texture_fractions <- function(df) {
  fraction_cols <- c(
    "sand_percent",
    "silt_percent",
    "clay_percent"
  )

  # Add missing fraction columns as NA
  missing_cols <- setdiff(fraction_cols, names(df))

  if (length(missing_cols) > 0) {
    df[missing_cols] <- NA_real_
  }

  df |>
    dplyr::mutate(
      sand_percent = dplyr::if_else(
        is.na(sand_percent),
        100 - (silt_percent + clay_percent),
        sand_percent
      ),
      silt_percent = dplyr::if_else(
        is.na(silt_percent),
        100 - (sand_percent + clay_percent),
        silt_percent
      ),
      clay_percent = dplyr::if_else(
        is.na(clay_percent),
        100 - (sand_percent + silt_percent),
        clay_percent
      )
    )
}

#' Assign USDA soil texture class
#'
#' Internal helper that assigns a USDA soil texture class based on completed
#' sand, silt, and clay percentages.
#'
#' @param df A data frame containing completed `sand_percent`,
#'   `silt_percent`, and `clay_percent.`
#'
#' @return A data frame with an added `texture` column.
#'
#' @source Thresholds for texture classification are from the USDA NRCS Soil
#'   Texture Calculator found at
#'   <https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator>.
#'
#' @keywords internal
assign_texture_class <- function(df) {
  # Add texture column if not present
  if (!"texture" %in% colnames(df)) {
    df <- df |>
      dplyr::mutate(
        texture = NA_character_,
        .before = c("sand_percent", "silt_percent", "clay_percent")
      )
  }

  # Add texture class
  df |>
    dplyr::mutate(
      texture = dplyr::case_when(
        silt_percent >= 80 &
          clay_percent < 12 ~
          "Silt",

        (silt_percent >= 50 &
          clay_percent >= 12 &
          clay_percent <= 27) |
          (silt_percent >= 50 &
            silt_percent <= 80 &
            clay_percent < 12) ~
          "Silt Loam",

        clay_percent >= 27 &
          clay_percent <= 40 &
          sand_percent <= 20 ~
          "Silty Clay Loam",

        clay_percent >= 40 &
          silt_percent >= 40 ~
          "Silty Clay",

        clay_percent >= 40 &
          sand_percent <= 45 &
          silt_percent < 40 ~
          "Clay",

        clay_percent >= 27 &
          clay_percent <= 40 &
          sand_percent > 20 &
          sand_percent <= 46 ~
          "Clay Loam",

        clay_percent >= 7 &
          clay_percent <= 27 &
          silt_percent >= 28 &
          silt_percent <= 50 &
          sand_percent <= 52 ~
          "Loam",

        clay_percent >= 20 &
          clay_percent <= 35 &
          silt_percent < 28 &
          sand_percent > 45 ~
          "Sandy Clay Loam",

        clay_percent >= 35 &
          sand_percent >= 45 ~
          "Sandy Clay",

        sand_percent > 85 &
          (silt_percent + 1.5 * clay_percent) < 15 ~
          "Sand",

        sand_percent >= 70 &
          sand_percent <= 91 &
          (silt_percent + 1.5 * clay_percent) >= 15 &
          (silt_percent + 2 * clay_percent) < 30 ~
          "Loamy Sand",

        (clay_percent >= 7 &
          clay_percent <= 20 &
          sand_percent > 52 &
          (silt_percent + 2 * clay_percent) >= 30) |
          (clay_percent < 7 &
            silt_percent < 50 &
            sand_percent > 43) ~
          "Sandy Loam",

        !(is.na(texture)) ~ texture,

        .default = NA_character_
      )
    )
}

#' Classify USDA soil texture from particle-size fractions
#'
#' Validates soil particle-size fractions (`sand_percent`, `silt_percent`,
#' and `clay_percent`), completes missing values when possible, and assigns a
#' USDA soil texture class.
#'
#' @details `classify_texture()` applies the following validation rules and
#'   assumptions:
#'
#' \itemize{
#'   \item **Eligibility**
#'   \itemize{
#'     \item Each sample must contain values for at least two of
#'     `sand_percent`, `silt_percent`, and `clay_percent` to be eligible for
#'     fraction-based texture classification.
#'   }
#'
#'   \item **Errors** (cause validation to fail)
#'   \itemize{
#'     \item All fraction values must fall within the range 0–100.
#'     \item Samples with all three fractions must sum to 100 with a
#'     &plusmn;1 tolerance (allowable range 99–101).
#'   }
#'
#'   \item **Warnings** (validation continues)
#'   \itemize{
#'     \item When exactly one fraction is missing, it is calculated as
#'     `100 - (sum of the other two)`.
#'     \item Samples with fewer than two provided fractions **and no** provided
#'     `texture` do not have sufficient data for classification. These samples
#'     are retained and returned with an `NA` texture value.
#'   }
#'
#'   \item **Special cases** (no warning or error)
#'   \itemize{
#'     \item Samples with fewer than two provided fractions **and** a provided
#'     `texture` will preserve the texture without modification.
#'   }
#' }
#'
#' @param df A data frame containing the columns `sample_id`,
#'   `sand_percent`, `silt_percent`, and `clay_percent`. An optional
#'   `texture` column can also be provided.
#' @param validate Logical. If `TRUE` (default), validation checks are run using
#'   `check_texture_fractions()` before classification.
#' @param output Character. One of `"cli"` (default) or `"ui"`. Controls how
#'   validation issues are reported.
#'
#' @return A data frame with a `texture` column containing USDA soil texture
#'   classes when sufficient data are available. Soil fraction columns may be
#'   completed (if partially missing) and are rounded to whole numbers.
#'
#' @section Side Effects:
#' When `validate = TRUE`, validation issues are formatted using
#' `format_issues()`. Errors will stop execution in `"cli"` mode.
#'
#' @source Thresholds for texture classification are based on the
#'   [USDA NRCS Soil Texture Calculator](https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator).
#'
#' @examples
#'
#' # Three samples classified without error
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   sand_percent = c(20, 45, 75),
#'   silt_percent = c(65, 35, 15),
#'   clay_percent = c(15, 20, 10)
#' )
#'
#' classify_texture(df)
#'
#' # Error when any fraction is outside the allowable range (0–100)
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   sand_percent = c(40, 0, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, 110, 30)
#' )
#'
#' try(classify_texture(df))
#'
#' # Error when fractions do not sum to 100 ±1 (range: 99–101)
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   sand_percent = c(40, 0, 90),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, 45, 30)
#' )
#'
#' try(classify_texture(df))
#'
#' # Warning: one fraction is missing (it will be calculated)
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   sand_percent = c(NA, 60, 25),
#'   silt_percent = c(45, 10, 40),
#'   clay_percent = c(50, 30, 35)
#' )
#'
#' classify_texture(df)
#'
#' # Warning: insufficient data for one sample (texture remains NA)
#' df <- data.frame(
#'   sample_id = c("S1", "S2", "S3"),
#'   sand_percent = c(40, NA, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, NA, 30)
#' )
#'
#' classify_texture(df)
#'
#' # No fractions provided, but texture is supplied (preserved)
#' df <- data.frame(
#'   sample_id = c("S1", "S2"),
#'   sand_percent = c(NA, NA),
#'   silt_percent = c(NA, NA),
#'   clay_percent = c(NA, NA),
#'   texture = c("Loam", "Sandy loam")
#' )
#'
#' classify_texture(df)
#' @export
classify_texture <- function(df, validate = TRUE, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)

  if (isTRUE(validate)) {
    issues <- list()
    # Validate first
    issues <- check_texture_fractions(df)

    if (length(issues) > 0) {
      format_issues(
        issues,
        output
      )
    }
  }

  # Determine which fraction columns exist
  fraction_cols <- intersect(
    c("sand_percent", "silt_percent", "clay_percent"),
    names(df)
  )

  # Check whether at least one sample has enough fraction data
  has_fraction_data <- length(fraction_cols) >= 2 &&
    any(rowSums(!is.na(dplyr::select(df, dplyr::all_of(fraction_cols)))) >= 2)

  # Early return of unchanged data if there is insufficient data
  if (isFALSE(has_fraction_data)) {
    cli::cli_alert_warning("Insufficient data to classify soil texture.")
    return(df)
  } else {
    # Otherwise, complete texture fractions and classify texture
    result <- df |>
      complete_texture_fractions() |>
      assign_texture_class()
    return(result)
  }
}

#' Get the texture measurement group
#'
#' Identifies the measurement group containing texture-related measurements
#' in a soil health dictionary. Texture-related measurements include
#' `texture`, `sand_percent`, `silt_percent`, and `clay_percent`.
#'
#' If no texture-related measurements exist in the dictionary, the first
#' measurement group in the dictionary is returned as a fallback. If the
#' dictionary is empty, `NULL` is returned.
#'
#' @param dictionary A data frame containing the soil measurement dictionary.
#'   Must include `measurement_group` and `column_name` columns.
#'
#' @returns
#' A character string containing the name of the measurement group associated
#' with texture measurements, or a fallback measurement group if no texture
#' measurements exist. Returns `NULL` if no measurement groups are available.
#'
#' @examples
#' dictionary <- data.frame(
#'   measurement_group = c(
#'     "Physical",
#'     "Physical",
#'     "Biological"
#'   ),
#'   column_name = c(
#'     "sand_percent",
#'     "clay_percent",
#'     "soil_respiration"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' get_texture_group(dictionary)
#'
#' @export
get_texture_group <- function(dictionary) {
  texture_group <- dictionary |>
    dplyr::filter(
      column_name %in%
        c(
          "texture",
          "sand_percent",
          "silt_percent",
          "clay_percent"
        )
    ) |>
    dplyr::pull(measurement_group) |>
    unique()

  if (length(texture_group) > 0) {
    return(texture_group[[1]])
  }

  fallback_group <- dictionary |>
    dplyr::pull(measurement_group) |>
    unique()

  if (length(fallback_group) > 0) {
    return(fallback_group[[1]])
  }

  NULL
}

#' Synchronize dictionary with texture measurements
#'
#' Adds missing `texture` and soil particle-size fraction columns created by
#' `classify_texture()` to a soil measurement dictionary.
#'
#' Texture-related measurements are inserted in the order:
#' `texture`, `sand_percent`, `silt_percent`, `clay_percent`.
#'
#' The measurement group used for inserted rows is determined by
#' `get_texture_group()`. If texture-related measurements already exist in the
#' dictionary, their measurement group is reused. Otherwise, the first
#' measurement group in the dictionary is used as a fallback.
#'
#' Intended primarily for internal use but exported so it can be called in
#' report templates and custom workflows.
#'
#' @param data data frame potentially containing `texture`, `sand_percent`,
#'   `silt_percent`, and `clay_percent`.
#' @param dictionary data frame with columns `measurement_group`,
#'   `column_name`, `abbr`, and `unit`.
#' @param language Either `"English"` or `"Spanish"`. Default `"English"`.
#'
#' @returns
#' An updated dictionary with any missing texture-related rows added.
#' Returns the original dictionary unchanged if no texture rows need to be
#' inserted.
#'
#' @examples
#' data <- data.frame(
#'   texture = c("Loam", "Clay loam"),
#'   sand_percent = c(40, 32),
#'   silt_percent = c(40, 38),
#'   clay_percent = c(20, 30),
#'   pH = c(7.4, 6.8)
#' )
#'
#' dictionary <- data.frame(
#'   measurement_group = c("Physical", "Physical", "Physical", "Chemical"),
#'   column_name = c("sand_percent", "silt_percent", "clay_percent", "pH"),
#'   abbr = c("Sand", "Silt", "Clay", "pH"),
#'   unit = c("%", "%", "%", ""),
#'   stringsAsFactors = FALSE
#' )
#'
#' sync_dictionary_texture(data, dictionary)
#'
#' @export

#' @export
sync_dictionary_texture <- function(
  data,
  dictionary,
  language = c("English", "Spanish")
) {
  language <- rlang::arg_match(language)

  texture_cols <- c("texture", "sand_percent", "silt_percent", "clay_percent")

  abbr <- switch(
    language,
    English = c(
      texture = "Texture",
      sand_percent = "Sand",
      silt_percent = "Silt",
      clay_percent = "Clay"
    ),
    Spanish = c(
      texture = "Textura",
      sand_percent = "Arena",
      silt_percent = "Limo",
      clay_percent = "Arcilla"
    )
  )

  # Detect which texture columns are present in data but missing in dictionary
  cols_to_add <- texture_cols[
    texture_cols %in% names(data) & !texture_cols %in% dictionary$column_name
  ]
  if (length(cols_to_add) == 0) {
    return(dictionary)
  }

  # Get the measurement group containing texture variables
  texture_group <- get_texture_group(dictionary)

  # Get ordering from data dictionary
  dictionary <- dictionary |>
    dplyr::mutate(
      # Get measurement group order
      group_order = dplyr::cur_group_id(),
      # Get measurement order
      measurement_order = seq_along(column_name),
      .by = measurement_group
    )

  # Get texture group order
  texture_group_order <- dictionary |>
    dplyr::filter(measurement_group == texture_group) |>
    dplyr::pull(group_order) |>
    unique()

  # Build rows for missing texture measurements
  rows <- purrr::map(
    cols_to_add,
    function(col) {
      data.frame(
        measurement_group = texture_group,
        column_name = col,
        abbr = abbr[[col]],
        unit = ifelse(col == "texture", "", "%"),
        group_order = texture_group_order,
        stringsAsFactors = FALSE
      )
    }
  ) |>
    dplyr::bind_rows()

  dictionary <- dictionary |>
    dplyr::bind_rows(rows) |>
    dplyr::mutate(
      measurement_order = dplyr::case_when(
        column_name == "texture" ~ 1,
        column_name == "sand_percent" ~ 2,
        column_name == "silt_percent" ~ 3,
        column_name == "clay_percent" ~ 4,
        # If not texture, keep the same order as original dictionary but add
        # 1000 so it comes after the texture group
        .default = measurement_order + 1000
      )
    ) |>
    dplyr::arrange(group_order, measurement_order) |>
    dplyr::select(-c(group_order, measurement_order))

  return(dictionary)
}
