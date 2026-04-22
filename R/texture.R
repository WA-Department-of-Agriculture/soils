#' Check soil texture fractions
#'
#' Validates soil particle-size fractions (sand, silt, and clay) in the input
#' dataset. Requires a `sample_id` column and at least two of `sand_percent`,
#' `silt_percent`, and `clay_percent` to perform validation.
#'
#' This function does not modify the data. It returns a list of validation
#' issues that can be combined with other `check_*()` functions and formatted
#' using `format_output()`.
#'
#' Validation rules:
#' \itemize{
#'   \item Warns if fewer than two fraction columns are provided (texture
#'   validation cannot be performed).
#'   \item Warns when exactly one fraction is missing (it can be computed later).
#'   \item Errors if any fraction values are outside the range 0–100.
#'   \item Errors if all three fractions are present but do not sum to 100
#'   (±1 tolerance).
#' }
#'
#' @param x A named list containing at least a `data` data frame.
#'
#' @return A list of issues (errors and/or warnings). Returns an empty list if
#'   no issues are found.
#'
#' @keywords internal
check_texture_fractions <- function(x) {
  issues <- list()

  if (!"sample_id" %in% names(x)) {
    msg <- cli::format_inline(
      "Data must contain {.field sample_id}."
    )
    return(list(new_issue("error", msg)))
  }

  # Setup ---------------------------------------------------------------------

  fraction_cols <- c("sand_percent", "silt_percent", "clay_percent")
  present <- intersect(fraction_cols, names(x))
  missing <- setdiff(fraction_cols, present)

  # Not enough fractions (warning) --------------------------------------------

  if (length(present) < 2) {
    msg <- cli::format_inline(
      "Provide at least two of {.field sand_percent}, {.field silt_percent}, {.field clay_percent} to enable texture validation."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
    return(issues)
  }

  # Prepare calculations ------------------------------------------------------

  df_calc <- x |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(fraction_cols), round),
      missing_n = rowSums(
        is.na(dplyr::across(dplyr::all_of(fraction_cols)))
      )
    )

  has_texture <- "texture" %in% names(df_calc)

  texture_provided <- if (has_texture) !is.na(df_calc$texture) else FALSE

  # Identify problem rows -----------------------------------------------------

  insufficient_ids <- df_calc$sample_id[
    df_calc$missing_n >= 2 & !texture_provided
  ]

  compute_ids <- df_calc$sample_id[df_calc$missing_n == 1]

  out_of_range_ids <- df_calc$sample_id[
    apply(
      df_calc[fraction_cols],
      1,
      function(x) any(!is.na(x) & (x < 0 | x > 100))
    )
  ]

  invalid_sum_ids <- df_calc$sample_id[
    df_calc$missing_n == 0 &
      (rowSums(df_calc[fraction_cols]) < 99 |
        rowSums(df_calc[fraction_cols]) > 101)
  ]

  # Errors --------------------------------------------------------------------

  if (length(out_of_range_ids) > 0) {
    msg <- cli::format_inline(
      "{cli::qty(length(out_of_range_ids))}Sample{?s} {.val {out_of_range_ids}} must have texture fractions between 0 and 100."
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  if (length(invalid_sum_ids) > 0) {
    msg <- cli::format_inline(
      "{cli::qty(length(invalid_sum_ids))}Sample{?s} {.val {invalid_sum_ids}} must have texture fractions that sum to 100 (+/- 1)."
    )
    issues <- c(issues, list(new_issue("error", msg)))
  }

  # Warnings ------------------------------------------------------------------

  if (length(insufficient_ids) > 0) {
    msg <- cli::format_inline(
      "{cli::qty(length(insufficient_ids))}Sample{?s} {.val {insufficient_ids}} {cli::qty(length(insufficient_ids))}{?has/have} less than two texture fractions and cannot be classified."
    )
    issues <- c(issues, list(new_issue("warning", msg)))
  }

  if (length(compute_ids) > 0) {
    msg <- cli::format_inline(
      "{cli::qty(length(compute_ids))}Sample{?s} {.val {compute_ids}} {cli::qty(length(compute_ids))}{?is/are} missing one texture fraction and will be computed as 100 minus the others."
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
#' Validates soil particle-size fractions (sand, silt, and clay), completes
#' missing values when possible, and assigns a USDA soil texture class.
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
#'     texture do not have sufficient data for texture classification.
#'     These samples are retained and returned with an `NA` texture value.
#'   }
#'
#'   \item **Special cases** (no warning or error)
#'   \itemize{
#'     \item Samples with fewer than two provided fractions **and** a provided
#'     texture will preserve the texture without modification.
#'   }
#' }
#'
#' @param df A data frame containing the columns `sample_id`, `sand_percent`,
#'   `silt_percent`, and `clay_percent`. An optional `texture` column can also
#'   be provided.
#'
#' @return A data frame with a `texture` column containing USDA soil texture
#'   classes (if enough soil fraction data are provided). Soil fractions are
#'   rounded to whole numbers.
#'
#' @source Thresholds for texture classification are based on the [USDA NRCS
#'   Soil Texture
#'   Calculator](https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator).
#'
#' @examples
#'
#' # Three samples classified without error
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(20, 45, 75),
#'   silt_percent = c(65, 35, 15),
#'   clay_percent = c(15, 20, 10)
#' )
#'
#' classify_texture(df)
#'
#' # Error when any fraction is outside the allowable range (0-100)
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(40, 0, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, 110, 30)
#' )
#'
#' try(classify_texture(df))
#'
#' # Error when fractions do not sum to 100 +/- 1 (allowable range: 99-101)
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(40, 0, 90),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, 45, 30)
#' )
#'
#' try(classify_texture(df))
#'
#' # Warn when one fraction is missing
#' # The missing fraction is calculated as 100 minus the other two
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(NA, 60, 25),
#'   silt_percent = c(45, 10, 40),
#'   clay_percent = c(50, 30, 35)
#' )
#'
#' classify_texture(df)
#'
#' # Warn when a sample has insufficient data and classification is skipped
#' # One sample is missing two soil fractions
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(40, NA, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, NA, 30)
#' )
#'
#' classify_texture(df)
#'
#' # No fractions provided, but texture is supplied
#' # No warning; existing texture is preserved
#'
#' df <- data.frame(
#'   sample_id = c(1, 2),
#'   sand_percent = c(NA, NA),
#'   silt_percent = c(NA, NA),
#'   clay_percent = c(NA, NA),
#'   texture = c("Loam", "Sandy loam")
#' )
#'
#' classify_texture(df)
#' @export
classify_texture <- function(df, output = c("cli", "ui")) {
  output <- rlang::arg_match(output)
  issues <- list()

  # Validate first
  issues <- check_texture_fractions(df)

  if (length(issues) > 0) {
    format_output(
      issues,
      output,
      context = list(
        error = "Texture validation failed.",
        warning = "Texture validation completed with warnings. "
      )
    )
  }

  # Determine which fraction columns exist
  fraction_cols <- intersect(
    c("sand_percent", "silt_percent", "clay_percent"),
    names(df)
  )

  # Check whether at least one sample has enough fraction data
  has_fraction_data <- length(fraction_cols) >= 2 &&
    any(rowSums(!is.na(df[fraction_cols])) >= 2)

  # Early return of unchanged data if there is insufficient data
  if (isFALSE(has_fraction_data)) {
    return(df)
  }

  # Otherwise, complete texture fractions and classify texture
  df |>
    complete_texture_fractions() |>
    assign_texture_class()
}

#' Synchronize dictionary with texture and fractions added by classify_texture()
#'
#' Adds missing `texture` and soil particle-size fraction columns to the
#' dictionary in a fixed order for the physical measurement group. Intended for
#' internal use but exported so it can be called in templates.
#'
#' @param data Data frame potentially containing `texture`, `sand_percent`,
#'   `silt_percent`, and `clay_percent`.
#' @param dictionary Data frame with columns `measurement_group`,
#'   `column_name`, `abbr`, and `unit`.
#' @param language Either `"English"` or `"Spanish"`. Default `"English"`.
#'
#' @return Updated dictionary with any missing texture/fraction rows inserted
#'   in the order: `texture`, `sand_percent`, `silt_percent`, `clay_percent`.
#'   Returns the original dictionary if no rows were added.
#' @keywords internal
#' @export
sync_dictionary_texture <- function(data, dictionary, language = "English") {
  language <- rlang::arg_match(language, c("English", "Spanish"))

  texture_cols <- c("texture", "sand_percent", "silt_percent", "clay_percent")

  fraction_abbr <- switch(
    language,
    English = c(
      sand_percent = "Sand",
      silt_percent = "Silt",
      clay_percent = "Clay"
    ),
    Spanish = c(
      sand_percent = "Arena",
      silt_percent = "Limo",
      clay_percent = "Arcilla"
    )
  )

  texture_abbr <- switch(
    language,
    English = "Texture",
    Spanish = "Textura"
  )

  measurement_group <- switch(
    language,
    English = "Physical",
    # Mediciones físicas. Use unicode escape to avoid R CMD warning.
    Spanish = "Mediciones f\u00EDsicas"
  )

  # Detect which columns are present in data but missing in dictionary
  cols_to_add <- texture_cols[
    texture_cols %in% names(data) & !texture_cols %in% dictionary$column_name
  ]
  if (length(cols_to_add) == 0) {
    return(dictionary)
  }

  # Build rows
  rows <- purrr::map(
    texture_cols,
    function(col) {
      if (!col %in% cols_to_add) {
        return(NULL)
      }

      if (col == "texture") {
        data.frame(
          measurement_group = measurement_group,
          column_name = "texture",
          abbr = texture_abbr,
          unit = "",
          stringsAsFactors = FALSE
        )
      } else {
        data.frame(
          measurement_group = measurement_group,
          column_name = col,
          abbr = fraction_abbr[[col]],
          unit = "%",
          stringsAsFactors = FALSE
        )
      }
    }
  ) |>
    purrr::compact() |>
    dplyr::bind_rows()

  # Add rows then arrange texture, sand, silt, clay at top of physical group
  dictionary <- dictionary |>
    dplyr::group_by(measurement_group) |>
    dplyr::bind_rows(rows) |>
    dplyr::arrange(
      dplyr::case_when(
        measurement_group == measurement_group ~
          match(column_name, texture_cols),
        # Fallback if there is no physical measurement group, add to the end of
        # the dictionary
        .default = nrow(dictionary) + 1
      )
    ) |>
    dplyr::ungroup()

  dictionary
}
