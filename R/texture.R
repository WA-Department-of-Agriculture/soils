#' Validate soil texture fractions
#'
#' Internal helper that validates the presence of soil particle-size fractions
#' (sand, silt, and clay). Rounds fractions prior to validation. Errors if two
#' or more fractions are missing for any sample and warns if exactly one
#' fraction is missing.
#'
#' @param df A data frame containing \code{sample_id}, \code{sand_percent},
#'   \code{silt_percent}, and \code{clay_percent}.
#'
#' @return A data frame with a helper column \code{missing_n} indicating the
#'   number of missing fractions per sample.
#'
#' @keywords internal
validate_texture_fractions <- function(df) {
  # Abort if df is not a dataframe
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "x" = "Input must be a {.cls dataframe} with columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}."
    ))
  }

  # Abort if sample_id, sand, silt, and clay are not present
  if (
    !all(
      c("sample_id", "sand_percent", "silt_percent", "clay_percent") %in%
        names(df)
    )
  ) {
    cli::cli_abort(
      c(
        "x" = "Columns {.field sample_id}, {.field sand_percent}, {.field clay_percent}, and {.field silt_percent} must be present in your data."
      )
    )
  }

  # Standardize inputs and calculate number of missing fractions
  df <- df |>
    dplyr::mutate(
      dplyr::across(c("sand_percent", "silt_percent", "clay_percent"), round),
      missing_n = rowSums(
        is.na(cbind(sand_percent, silt_percent, clay_percent))
      )
    )

  # Identify samples with warnings/errors --------------------------------------

  # Warn: Samples with 3 missing fractions assume texture was not measured unless
  # texture is provided
  has_texture <- "texture" %in% names(df)
  texture_provided <- if (has_texture) {
    !is.na(df$texture)
  } else {
    rep(FALSE, nrow(df))
  }

  # Warn: Samples with no provided fractions
  unmeasured_ids <- df$sample_id[
    df$missing_n == 3 & !texture_provided
  ]

  # Error: Samples with exactly 2 missing fractions
  insufficient_ids <- df$sample_id[df$missing_n == 2]

  # Warn: Samples with 1 missing fraction will be computed
  compute_ids <- df$sample_id[df$missing_n == 1]

  # Error: Samples with fractions not between 0 and 100
  out_of_range_ids <- df$sample_id[
    apply(
      df[c("sand_percent", "silt_percent", "clay_percent")],
      1,
      function(x) any(!is.na(x) & (x < 0 | x > 100))
    )
  ]

  # Error: Samples with complete fractions but sum is outside tolerance 100 +/- 1
  invalid_sum_ids <- df$sample_id[
    df$missing_n == 0 &
      (df$sand_percent + df$silt_percent + df$clay_percent < 99 |
        df$sand_percent + df$silt_percent + df$clay_percent > 101)
  ]

  # Build error bullets --------------------------------------------------------

  # Need at least 2/3 fractions
  error_insufficient <- if (length(insufficient_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(insufficient_ids))} {.strong Sample{?s} {soils_cli_vec(insufficient_ids)}} must have at least two fractions."
    )
  } else {
    NULL
  }

  # Fraction not between 0 and 100
  error_out_of_range <- if (length(out_of_range_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(out_of_range_ids))}{.strong Sample{?s} {soils_cli_vec(out_of_range_ids)}} must have all fraction values between 0 and 100."
    )
  } else {
    NULL
  }

  # Sum does not equal 100 (+/- 1)
  error_invalid_sum <- if (length(invalid_sum_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(invalid_sum_ids))}{.strong Sample{?s} {soils_cli_vec(invalid_sum_ids)}} must have fractions that sum to 100 (+/- 1)."
    )
  } else {
    NULL
  }

  # Build warning bullets ------------------------------------------------------

  # Missing fraction computed as 100 minus the other two
  warn_compute <- if (length(compute_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(compute_ids))}{.strong Sample{?s} {soils_cli_vec(compute_ids)} {cli::qty(length(compute_ids))}}{?is/are} missing one fraction and will be calculated as 100 minus the other two."
    )
  } else {
    NULL
  }

  # Missing all three fractions and assume texture not measured
  warn_unmeasured <- if (length(unmeasured_ids) > 0) {
    cli::format_inline(
      "{cli::qty(length(unmeasured_ids))}{.strong Sample{?s} {soils_cli_vec(unmeasured_ids)} {cli::qty(length(unmeasured_ids))}}{?is/are} missing all fractions and no texture was provided, so texture will remain blank."
    )
  } else {
    NULL
  }

  # Emit error and/or warning messages -----------------------------------------

  error_present <- any(
    !is.null(error_insufficient) |
      !is.null(error_out_of_range) |
      !is.null(error_invalid_sum)
  )

  warning_present <- any(
    !is.null(warn_compute) | !is.null(warn_unmeasured)
  )

  # Error and warning
  if (isTRUE(error_present) && isTRUE(warning_present)) {
    cli::cli_abort(
      c(
        "x" = "{.strong Soil texture validation failed.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with errors to correct:",
        "*" = error_insufficient,
        "*" = error_out_of_range,
        "*" = error_invalid_sum,
        "",
        "i" = "Samples with assumptions:",
        "*" = warn_compute,
        "*" = warn_unmeasured
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
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with errors to correct:",
        "*" = error_insufficient,
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
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Samples with assumptions:",
        "*" = warn_compute,
        "*" = warn_unmeasured
      ),
      call = NULL
    )
  }

  # Success with no error or warning
  if (isFALSE(error_present) && isFALSE(warning_present)) {
    cli::cli_alert_success(
      "Soil texture successfully validated."
    )
  }

  return(df)
}

#' Complete missing soil texture fractions
#'
#' Internal helper that computes a missing soil fraction (sand, silt, or clay)
#' when exactly one is missing, using \code{100 - (sum of the other two)}.
#'
#' @param df A data frame containing \code{sand_percent},
#'   \code{silt_percent}, and \code{clay_percent}.
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
#' @param df A data frame containing completed \code{sand_percent},
#'   \code{silt_percent}, and \code{clay_percent}.
#'
#' @return A data frame with an added \code{texture} column.
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
      dplyr::mutate(texture = NA_character_, .before = "sand_percent")
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
#' @details \code{classify_texture()} applies the following validation rules and
#'   assumptions:
#'
#' \itemize{
#'   \item Each sample must contain values for at least two of
#'   \code{sand_percent}, \code{silt_percent}, and \code{clay_percent}.
#'   Samples with fewer than two provided fractions must be corrected before
#'   texture classification can proceed.
#'
#'   \item When exactly one fraction is missing, it is calculated as
#'   \code{100 - (sum of the other two)}.
#'
#'   \item All fraction values must fall within the range 0–100. Samples with all
#'   three fractions must sum to 100 with a &plusmn;1 tolerance (allowable range
#'   99-101).
#'
#'   \item Samples with all three fractions missing are assumed to not have
#'   texture measured. These samples are retained and returned without
#'   fraction-based classification.
#'
#'   \item If a \code{texture} column is provided, non-missing texture values
#'   are preserved and are not overwritten by fraction-based classification.
#'   This allows users to supply texture classes from external sources (e.g.,
#'   NRCS gSSURGO) when particle-size fractions are unavailable.
#' }
#'
#' @param df A data frame containing the columns \code{sample_id},
#'   \code{sand_percent}, \code{silt_percent}, and \code{clay_percent}. An
#'   optional \code{texture} column can also be provided: if the sand/silt/clay
#'   fractions are present, \code{texture} will be overwritten by the classified
#'   value; if the fractions are missing, existing \code{texture} values are
#'   preserved.
#'
#' @return A data frame with a \code{texture} column containing USDA soil
#'   texture classes. Soil fractions are rounded to whole numbers.
#'
#' @source Thresholds for texture classification are based on the [USDA NRCS Soil
#'   Texture Calculator](https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator).
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
#' # Error when a sample has insufficient data
#' # One sample is missing two soil fractions
#'
#' df <- data.frame(
#'   sample_id = c(1, 2, 3),
#'   sand_percent = c(40, NA, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, NA, 30)
#' )
#'
#' try(classify_texture(df))
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
#' # Warn when soil texture was not measured
#' # Samples with all fractions missing return a blank texture class
#'
#'   df <- data.frame(
#'    sample_id = c(1, 2, 3),
#'    sand_percent = c(NA, NA, 30),
#'    silt_percent = c(NA, NA, 30),
#'    clay_percent = c(NA, NA, 40)
#'  )
#'
#'  classify_texture(df)
#'
#' @export
classify_texture <- function(df) {
  df |>
    validate_texture_fractions() |>
    complete_texture_fractions() |>
    assign_texture_class() |>
    dplyr::select(-missing_n)
}
