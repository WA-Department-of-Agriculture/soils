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
  df <- df |>
    dplyr::mutate(
      dplyr::across(dplyr::where(is.numeric), round),
      missing_n = rowSums(
        is.na(cbind(sand_percent, silt_percent, clay_percent))
      )
    )

  # Error if not enough data to classify (missing 2 fractions)
  one_fraction_rows <- which(df$missing_n == 2)

  if (length(one_fraction_rows) > 0) {
    cli::cli_abort(
      c(
        "x" = "There is not enough soil fraction data to classify texture.",
        "i" = "{.strong {length(one_fraction_rows)}} row{?s} with insufficient data (row ind{?ex/ices}: {one_fraction_rows}).",
        "i" = "Provide at least two of {.field sand_percent}, {.field silt_percent}, and {.field clay_percent} for each row."
      )
    )
  }

  # Warn if one missing fraction (will be computed)
  two_fraction_rows <- which(df$missing_n == 1)
  if (length(two_fraction_rows) > 0) {
    cli::cli_warn(
      c(
        "One soil fraction is missing and will be calculated as 100 minus the other two.",
        "i" = "{.strong {length(two_fraction_rows)}} row{?s} with a calculated fraction (row ind{?ex/ices}: {two_fraction_rows})."
      )
    )
  }

  # Warn if one missing all fractions (cannot classify texture)
  # Do not error in case texture was not measured
  zero_fraction_rows <- which(df$missing_n == 3)
  if (length(zero_fraction_rows) > 0) {
    cli::cli_warn(
      c(
        "i" = "{length(zero_fraction_rows)} row{?s} {?is/are} missing all three fractions (row ind{?ex/ices}: {zero_fraction_rows}) and will not be assigned a soil texture class."
      )
    )
  }

  # Only validate sums for fully specified rows.
  # Rows with exactly one missing fraction are completed downstream.
  not_100_rows <- which(
    df$missing_n == 0 &
      (df$sand_percent + df$silt_percent + df$clay_percent) != 100
  )

  if (length(not_100_rows) > 0) {
    cli::cli_abort(
      c(
        "x" = "{.field sand_percent}, {.field silt_percent}, and {.field clay_percent} do not sum to 100.",
        "i" = "{.strong {length(not_100_rows)}} row{?s} with invalid sum{?s} (row ind{?ex/ices}: {not_100_rows})."
      )
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

#' Classify USDA soil texture
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
classify_texture <- function(df) {
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

#' Complete soil fractions and assign texture class
#'
#' Validates and completes soil particle-size fractions (sand, silt, and clay)
#' and assigns a USDA soil texture class. When exactly one fraction is missing,
#' it is computed as 100 minus the other two. The function errors if
#' insufficient information is provided.
#'
#' @param df A data frame containing \code{sand_percent}, \code{silt_percent},
#'   and \code{clay_percent}.
#'
#' @return A data frame with a new \code{texture} column and soil particle-size
#'   fractions rounded to whole numbers.
#'
#' @source Thresholds for texture classification are from the USDA NRCS Soil
#'   Texture Calculator found at
#'   <https://www.nrcs.usda.gov/resources/education-and-teaching-materials/soil-texture-calculator>.
#'
#' @examples
#'
#' # Example 1: Three samples classified without error
#' # All soil fractions are provided and sum to 100
#'
#' df <- data.frame(
#'   sand_percent = c(20, 45, 75),
#'   silt_percent = c(65, 35, 15),
#'   clay_percent = c(15, 20, 10)
#' )
#'
#' complete_texture(df)
#'
#' # Example 2: Three samples with one missing fraction (computed with a warning)
#' # The missing fraction is calculated as 100 minus the other two
#'
#' df <- data.frame(
#'   sand_percent = c(NA, 60, 25),
#'   silt_percent = c(50, 10, 40),
#'   clay_percent = c(50, 40, NA)
#' )
#'
#' complete_texture(df)
#'
#' # Example 3: Error when a sample has insufficient data
#' # One row is missing two soil fractions and cannot be classified
#'
#' df <- data.frame(
#'   sand_percent = c(40, NA, 65),
#'   silt_percent = c(40, 55, 5),
#'   clay_percent = c(20, NA, 30)
#' )
#'
#' try(complete_texture(df))
#'
#' # Example 4: Warn when soil texture was not measured
#' # Samples with all fractions missing are assumed to have no texture data.
#' # These samples trigger a warning and are not assigned a texture class.
#'
#'   df <- data.frame(
#'    sand_percent = c(NA, NA, 30),
#'    silt_percent = c(NA, NA, 30),
#'    clay_percent = c(NA, NA, 40)
#'  )
#'
#'  complete_texture(df)
#'
#' @export
complete_texture <- function(df) {
  required_cols <- c("sand_percent", "silt_percent", "clay_percent")

  has_required <- all(required_cols %in% names(df))

  if (!has_required) {
    cli::cli_warn(
      c(
        "Soil texture classification skipped.",
        "x" = "Required soil fraction columns were not found.",
        "i" = "Expected columns are {.field sand_percent}, {.field silt_percent}, and {.field clay_percent}.",
        "> Returning data unchanged."
      )
    )
    return(df)
  } else {
    df |>
      validate_texture_fractions() |>
      complete_texture_fractions() |>
      classify_texture() |>
      dplyr::select(-missing_n)
  }
}
