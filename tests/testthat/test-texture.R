# Validation: require a dataframe ----------------------------------------------

test_that("validate_texture_fractions errors if input is not a data frame", {
  expect_error(
    validate_texture_fractions(list(a = 1)),
    "Input must be a <dataframe>"
  )
})


# Validation: required columns -------------------------------------------------

test_that("validate_texture_fractions errors if required columns are missing", {
  df <- data.frame(
    sand_percent = 40,
    silt_percent = 40
  )

  expect_error(
    validate_texture_fractions(df),
    "Columns .* must be present"
  )
})


# Validation: missing fraction rules -------------------------------------------

test_that("errors when two fractions are missing in a row", {
  df <- data.frame(
    sand_percent = c(40, NA),
    silt_percent = c(40, NA),
    clay_percent = c(20, 30)
  )

  expect_error(
    validate_texture_fractions(df),
    "must have at least two fractions"
  )
})


test_that("warns when exactly one fraction is missing", {
  df <- data.frame(
    sand_percent = c(NA, 40),
    silt_percent = c(50, 40),
    clay_percent = c(50, 20)
  )

  expect_warning(
    validate_texture_fractions(df),
    "missing one fraction"
  )
})

test_that("warns when all fractions are missing and texture is not provided", {
  df <- data.frame(
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA
  )

  expect_warning(
    validate_texture_fractions(df),
    "missing all fractions"
  )
})

test_that("does not warn when all fractions are missing but texture is provided", {
  df <- data.frame(
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA,
    texture = "Loam"
  )

  expect_no_warning(
    validate_texture_fractions(df)
  )
})

# Validation: fraction range ---------------------------------------------------

test_that("errors when any fraction is outside 0–100", {
  df <- data.frame(
    sand_percent = c(40, 110),
    silt_percent = c(40, 10),
    clay_percent = c(20, 10)
  )

  expect_error(
    validate_texture_fractions(df),
    "between 0 and 100"
  )
})


# Validation: sum to 100 (±1 tolerance) ----------------------------------------

test_that("errors when complete fractions fall outside the 99–101 tolerance", {
  df <- data.frame(
    sand_percent = c(40, 30),
    silt_percent = c(40, 30),
    clay_percent = c(30, 42) # sums: 110 and 102
  )

  expect_error(
    validate_texture_fractions(df),
    "sum to 100"
  )
})

# Validation: combined errors and warnings -------------------------------------

test_that("validation reports all error and warning types together", {
  df <- data.frame(
    # Row 1: missing two fractions → error
    sand_percent = c(NA, 110, 40, NA, NA),

    # Row 2: out of range value → error
    silt_percent = c(NA, 10, 40, 30, NA),

    # Row 3: sums to != 100 → error
    clay_percent = c(30, 10, 30, 40, NA)
  )

  # Row breakdown:
  # 1 → two missing fractions (error)
  # 2 → sand > 100 (error)
  # 3 → sums to 110 (error)
  # 4 → exactly one missing fraction (warning)
  # 5 → all fractions missing (warning)

  expect_error(
    validate_texture_fractions(df),
    regexp = paste(
      "validation failed", # overall failure
      "must have at least two fractions", # insufficient data
      "between 0 and 100", # out of range
      "sum to 100", # invalid sum
      "missing one fraction", # computed fraction
      "missing all fractions", # unmeasured texture
      sep = ".*"
    )
  )
})


# Completion: compute missing fraction -----------------------------------------

test_that("complete_texture_fractions computes the missing fraction correctly", {
  df <- data.frame(
    sand_percent = NA,
    silt_percent = 30,
    clay_percent = 20
  )

  out <- df |>
    complete_texture_fractions()

  expect_equal(out$sand_percent, 50)
})


# Classification: USDA texture classes -----------------------------------------

test_that("assign_texture_class assigns expected USDA texture classes", {
  df <- data.frame(
    sand_percent = c(85, 40, 20),
    silt_percent = c(10, 40, 65),
    clay_percent = c(5, 20, 15)
  )

  out <- assign_texture_class(df)

  expect_equal(
    out$texture,
    c("Loamy Sand", "Loam", "Silt Loam")
  )
})


test_that("rows with unmeasured texture return NA texture", {
  df <- data.frame(
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA
  )

  expect_warning(
    out <- classify_texture(df),
    "missing all fractions"
  )

  expect_true(is.na(out$texture))
})


# End-to-end: classify_texture -------------------------------------------------

test_that("classify_texture completes, validates, and classifies correctly", {
  df <- data.frame(
    sand_percent = c(NA, 60),
    silt_percent = c(45, 10),
    clay_percent = c(50, 30)
  )

  expect_warning(
    out <- classify_texture(df),
    "missing one fraction"
  )

  expect_equal(out$sand_percent[1], 5)
  expect_false(is.na(out$texture[2]))
})


# CLI messaging: singular vs plural --------------------------------------------

test_that("CLI messages correctly pluralize 'Row' vs 'Rows'", {
  df_single <- data.frame(
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = 30
  )

  expect_error(
    validate_texture_fractions(df_single),
    "Row 1 must have at least two fractions"
  )

  df_plural <- data.frame(
    sand_percent = c(NA, NA),
    silt_percent = c(NA, NA),
    clay_percent = c(30, 40)
  )

  expect_error(
    validate_texture_fractions(df_plural),
    "Rows 1 and 2 must have at least two fractions"
  )
})


test_that("CLI messages correctly pluralize 'is' vs 'are'", {
  df_single <- data.frame(
    sand_percent = NA,
    silt_percent = 40,
    clay_percent = 60
  )

  expect_warning(
    validate_texture_fractions(df_single),
    "Row 1 is missing one fraction"
  )

  df_plural <- data.frame(
    sand_percent = c(NA, NA),
    silt_percent = c(40, 30),
    clay_percent = c(60, 70)
  )

  expect_warning(
    validate_texture_fractions(df_plural),
    "Rows 1 and 2 are missing one fraction"
  )
})
