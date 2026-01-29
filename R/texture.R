#' Validate soil texture fractions
#'
#' Internal helper that validates soil particle-size fractions (sand, silt, and
#' clay). If exactly two of the three fraction columns are present, the missing
#' column is created and filled with `NA` to be later computed in
#' `complete_texture_fractions()`. Samples with fewer than two provided
#' fractions are retained but skipped during texture validation and
#' classification.
#'
#' Fractions are rounded prior to validation. Warns if fewer than two fractions
#' are provided or if exactly one fraction is missing. Errors if fraction values
#' are outside the allowable range (0-100) or if complete fractions do not sum
#' to 100 (+/- 1).
#'
#'
#' @param df A data frame containing `sample_id` and at least two of
#'   `sand_percent`, `silt_percent`, and `clay_percent`.
#'
#' @return A data frame with validated soil texture fraction columns
#'   (`sand_percent`, `silt_percent`, `clay_percent`). If exactly two of the
#'   fractions are present, the third is created and filled with `NA` for
#'   downstream completion.
#'
#' @keywords internal
validate_texture_fractions <- function(df) {
  # Abort if df is not a dataframe
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "x" = "Input must be a {.cls dataframe} with columns\
      {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}."
    ))
  }

  # Abort if sample_id is missing
  if (!"sample_id" %in% names(df)) {
    cli::cli_abort(c(
      "x" = "Column {.field sample_id} must be present in your data."
    ))
  }

  # If at least two fraction columns are provided, create the third. Otherwise,
  # return the df and inform the user that there is insufficient data to
  # validate and classify texture.
  fraction_cols <- c("sand_percent", "silt_percent", "clay_percent")
  present_fractions <- fraction_cols[fraction_cols %in% names(df)]
  missing_fraction <- setdiff(fraction_cols, present_fractions)

  if (length(present_fractions) < 2) {
    cli::cli_inform(
      c(
        "i" = "Not enough soil fraction data to validate or classify texture.",
        "*" = "Provide at least two of {.field sand_percent},\
        {.field silt_percent}, and {.field clay_percent} columns\
        to enable texture validation and classification."
      )
    )
    return(df)
  }

  if (length(present_fractions) == 2 && length(missing_fraction) == 1) {
    df[[missing_fraction]] <- NA_real_

    cli::cli_inform(
      c(
        "i" = "Exactly two soil fraction columns detected.",
        "*" = "Created missing column {.field {missing_fraction}}\
        to allow texture validation and classification."
      )
    )
  }

  # Standardize inputs and calculate number of missing fractions
  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(fraction_cols), round),
      missing_n = rowSums(
        is.na(cbind(sand_percent, silt_percent, clay_percent))
      )
    )

  # Identify samples with warnings/errors --------------------------------------

  # Determine if texture is provided (if so, do not warn)
  has_texture <- "texture" %in% names(df)
  texture_provided <- if (has_texture) {
    !is.na(df$texture)
  } else {
    rep(FALSE, nrow(df))
  }

  # Warn: Samples with fewer than two provided fractions & texture is not
  # provided (skip classification)
  insufficient_ids <- df$sample_id[
    df$missing_n >= 2 & !texture_provided
  ]

  # Warn: Samples with 1 missing fraction will be computed
  compute_ids <- df$sample_id[df$missing_n == 1]

  # Error: Samples with fractions not between 0 and 100
  out_of_range_ids <- df$sample_id[
    apply(
      df[fraction_cols],
      1,
      function(x) any(!is.na(x) & (x < 0 | x > 100))
    )
  ]

  # Error: Samples with complete fractions and sum outside tolerance 100 +/- 1
  invalid_sum_ids <- df$sample_id[
    df$missing_n == 0 &
      (df$sand_percent + df$silt_percent + df$clay_percent < 99 |
        df$sand_percent + df$silt_percent + df$clay_percent > 101)
  ]

  # Build error bullets --------------------------------------------------------

  # Fraction not between 0 and 100
  error_out_of_range <- if (length(out_of_range_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(out_of_range_ids))}{.strong Sample{?s}\
      {soils_cli_vec(out_of_range_ids)}} must have all fraction\
      values between 0 and 100."
    )
  } else {
    NULL
  }

  # Sum does not equal 100 (+/- 1)
  error_invalid_sum <- if (length(invalid_sum_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(invalid_sum_ids))}{.strong Sample{?s}\
      {soils_cli_vec(invalid_sum_ids)}} must have fractions that\
      sum to 100 (+/- 1)."
    )
  } else {
    NULL
  }

  # Build warning bullets ------------------------------------------------------

  # Fewer than two fractions provided (skip classification)
  warn_insufficient <- if (length(insufficient_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(insufficient_ids))}{.strong Sample{?s}\
      {soils_cli_vec(insufficient_ids)}\
      {cli::qty(length(insufficient_ids))}}{?has/have}\
      fewer than two fractions and will be skipped during texture\
      classification."
    )
  } else {
    NULL
  }

  # Missing fraction computed as 100 minus the other two
  warn_compute <- if (length(compute_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(compute_ids))}{.strong Sample{?s}\
      {soils_cli_vec(compute_ids)} {cli::qty(length(compute_ids))}}{?is/are}\
      missing one fraction and will be calculated as 100 minus the other two."
    )
  } else {
    NULL
  }

  # Emit error and/or warning messages -----------------------------------------

  error_present <- any(
    !is.null(error_out_of_range) |
      !is.null(error_invalid_sum)
  )

  warning_present <- any(
    !is.null(warn_insufficient) |
      !is.null(warn_compute)
  )

  # Error and warning
  if (isTRUE(error_present) && isTRUE(warning_present)) {
    cli::cli_abort(
      c(
        "x" = "{.strong Soil texture validation failed.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent},\
        {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with errors to correct:",
        "*" = error_out_of_range,
        "*" = error_invalid_sum,
        "",
        "i" = "Samples with assumptions:",
        "*" = warn_insufficient,
        "*" = warn_compute
      ),
      call = NULL
    )
  }

  # Error only
  if (isTRUE(error_present) && isFALSE(warning_present)) {
    cli::cli_abort(
      c(
        "x" = "{.strong Soil texture validation failed.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent},\
        {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with errors to correct:",
        "*" = error_out_of_range,
        "*" = error_invalid_sum
      ),
      call = NULL
    )
  }

  # Warning only
  if (isFALSE(error_present) && isTRUE(warning_present)) {
    cli::cli_warn(
      c(
        "!" = "{.strong Soil texture validation completed with assumptions.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent},\
        {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with assumptions:",
        "*" = warn_insufficient,
        "*" = warn_compute
      ),
      call = NULL
    )
  }

  # Drop helper column before returning validated data
  df <- df |>
    dplyr::select(-missing_n)
  return(df)
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
classify_texture <- function(df) {
  # Validate first
  df <- validate_texture_fractions(df)

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
