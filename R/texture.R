#' Validate soil texture fractions
#'
#' Internal helper that validates the presence of soil particle-size fractions
#' (sand, silt, and clay). Rounds fractions prior to validation. Errors if two
#' or more fractions are missing in any row and warns if exactly one fraction is
#' missing.
#'
#' @param df A data frame containing \code{sand_percent}, \code{silt_percent},
#'   and \code{clay_percent}.
#'
#' @return A data frame with a helper column \code{missing_n} indicating the
#'   number of missing fractions per row.
#'
#' @keywords internal
validate_texture_fractions <- function(df) {
  # Abort if df is not a dataframe
  if (!is.data.frame(df)) {
    cli::cli_abort(c(
      "x" = "Input must be a {.cls dataframe} with columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}."
    ))
  }

  # Abort if sand, silt, and clay are not present
  if (!all(c("sand_percent", "silt_percent", "clay_percent") %in% names(df))) {
    cli::cli_abort(
      c(
        "x" = "Columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent} must be present in your data."
      )
    )
  }

  # Standardize inputs and calculate number of missing fractions
  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), round),
      missing_n = rowSums(
        is.na(cbind(sand_percent, silt_percent, clay_percent))
      )
    )

  # Identify row groups --------------------------------------------------------

  # Warn: Rows with 3 missing fractions assume texture was not measured
  unmeasured_rows <- which(df$missing_n == 3)

  # Error: Rows with exactly 2 missing fractions
  insufficient_rows <- which(df$missing_n == 2)

  # Warn: Rows with 1 missing fraction will be computed
  compute_rows <- which(df$missing_n == 1)

  # Error: Rows with fractions not between 0 and 100
  fraction_cols <- c("sand_percent", "silt_percent", "clay_percent")
  out_of_range <- df |>
    dplyr::mutate(
      row = dplyr::row_number(),
      dplyr::across(
        dplyr::all_of(fraction_cols),
        ~ !is.na(.) & (. < 0 | . > 100)
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      bad_cols = list(fraction_cols[dplyr::c_across(dplyr::all_of(
        fraction_cols
      ))])
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(lengths(bad_cols) > 0)

  # Error: Rows with complete fractions but sum does not equal 100
  invalid_sum_rows <- which(
    df$missing_n == 0 &
      (df$sand_percent + df$silt_percent + df$clay_percent) != 100
  )

  # Build error bullets --------------------------------------------------------

  # Need at least 2/3 fractions
  if (length(insufficient_rows) > 0) {
    error_insufficient_rows <- cli::format_inline(
      "{cli::qty(length(insufficient_rows))} {.strong Row{?s} {soils_cli_vec(insufficient_rows)}} must have at least two fractions."
    )
  } else {
    error_insufficient_rows <- NULL
  }

  # Fraction not between 0 and 100
  if (nrow(out_of_range) > 0) {
    error_out_of_range <- cli::format_inline(
      "{cli::qty(nrow(out_of_range))}{.strong Row{?s} {soils_cli_vec(out_of_range$row)}} must have all fraction values between 0 and 100."
    )
  } else {
    error_out_of_range <- NULL
  }

  # Sum does not equal 100
  if (length(invalid_sum_rows) > 0) {
    error_invalid_sum_rows <- cli::format_inline(
      "{cli::qty(length(invalid_sum_rows))}{.strong Row{?s} {soils_cli_vec(invalid_sum_rows)}} must have fractions that sum to 100."
    )
  } else {
    error_invalid_sum_rows <- NULL
  }

  # Build warning bullets ------------------------------------------------------

  # Missing fraction computed as 100 minus the other two
  if (length(compute_rows) > 0) {
    warn_compute_rows <- cli::format_inline(
      "{cli::qty(length(compute_rows))}{.strong Row{?s} {soils_cli_vec(compute_rows)} {cli::qty(length(compute_rows))}}{?is/are} missing one fraction and will be calculated as 100 minus the other two."
    )
  } else {
    warn_compute_rows <- NULL
  }

  # Missing all three fractions and assume texture not measured
  if (length(unmeasured_rows) > 0) {
    warn_unmeasured_rows <- cli::format_inline(
      "{cli::qty(length(unmeasured_rows))}{.strong Row{?s} {soils_cli_vec(unmeasured_rows)} {cli::qty(length(unmeasured_rows))}}{?is/are} missing all fractions and will not have texture classified."
    )
  } else {
    warn_unmeasured_rows <- NULL
  }

  # Emit error and/or warning messages -----------------------------------------

  error_present <- any(
    !is.null(error_insufficient_rows) | !is.null(error_invalid_sum_rows)
  )

  warning_present <- any(
    !is.null(warn_compute_rows) | !is.null(warn_unmeasured_rows)
  )

  # Error and warning
  if (isTRUE(error_present) & isTRUE(warning_present)) {
    cli::cli_abort(
      c(
        "x" = "{.strong Soil texture validation failed.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Rows with errors to correct:",
        "*" = error_insufficient_rows,
        "*" = error_out_of_range,
        "*" = error_invalid_sum_rows,
        "",
        "i" = "Rows with assumptions to review:",
        "*" = warn_compute_rows,
        "*" = warn_unmeasured_rows
      ),
      call = NULL
    )
  }

  # Error only
  if (isTRUE(error_present) & isFALSE(warning_present)) {
    cli::cli_abort(
      c(
        "x" = "{.strong Soil texture validation failed.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Rows with errors to correct:",
        "*" = error_insufficient_rows,
        "*" = error_out_of_range,
        "*" = error_invalid_sum_rows
      ),
      call = NULL
    )
  }

  # Warning only
  if (isFALSE(error_present) & isTRUE(warning_present)) {
    cli::cli_warn(
      c(
        "!" = "{.strong Soil texture validation completed with assumptions.}",
        "",
        "Soil fractions are provided in the columns {.field sand_percent}, {.field clay_percent}, and {.field silt_percent}.",
        "",
        "i" = "Rows with assumptions to review:",
        "*" = warn_compute_rows,
        "*" = warn_unmeasured_rows
      ),
      call = NULL
    )
  }

  # Success with no error or warning
  if (isFALSE(error_present) & isFALSE(warning_present)) {
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
#' assumptions:
#'
#' \itemize{
#'   \item Each row must contain values for at least two of
#'   \code{sand_percent}, \code{silt_percent}, and \code{clay_percent}.
#'   Rows with fewer than two provided fractions must be corrected before
#'   texture classification can proceed.
#'
#'   \item When exactly one fraction is missing, it is calculated as
#'   \code{100 - (sum of the other two)}.
#'
#'   \item All fraction values must fall within the range 0–100, and fully
#'   specified rows must sum to exactly 100.
#'
#'   \item Rows with all three fractions missing are assumed to represent
#'   samples where soil texture was not measured. These rows are retained
#'   and returned with a missing texture class.
#' }

#'
#' @param df A data frame containing the columns \code{sand_percent},
#'   \code{silt_percent}, and \code{clay_percent}.
#'
#' @return A data frame with a \code{texture} column containing USDA soil
#'   texture classes. Soil fractions are rounded to whole numbers.
#'
#' @source Thresholds for texture classification are based on the USDA NRCS Soil
#'   Texture Calculator
#'   <https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator>.
#'
#' @examples
#'
#' # Three samples classified without error
#'
#' df <- data.frame(
#'   sand_percent = c(20, 45, 75),
#'   silt_percent = c(65, 35, 15),
#'   clay_percent = c(15, 20, 10)
#' )
#'
#' classify_texture(df)
#'
#' # Error when a sample has insufficient data
#' # One row is missing two soil fractions
#'
#' df <- data.frame(
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
#'   sand_percent = c(40, 0, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, 110, 30)
#' )
#'
#' try(classify_texture(df))
#'
#' # Error when fractions do not sum to 100
#'
#' df <- data.frame(
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
#'   sand_percent = c(NA, 60, 25),
#'   silt_percent = c(45, 10, 40),
#'   clay_percent = c(50, 30, 35)
#' )
#'
#' classify_texture(df)
#'
#' # Warn when soil texture was not measured
#' # Rows with all fractions missing return a blank texture class
#'
#'   df <- data.frame(
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
