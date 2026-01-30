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
    "required column: sample_id"
  )
})

# Validation: two fraction columns provided ------------------------------------

test_that("creates the third fraction column when exactly two are provided", {
  df <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40
  )

  expect_warning(
    out <- validate_texture_fractions(df),
    "missing one fraction"
  )

  expect_equal(out$clay_percent, NA_real_)
  expect_true(is.na(out$clay_percent))
})

# Validation: missing fraction rules -------------------------------------------

test_that("warns when exactly two fractions are missing in a row", {
  df <- data.frame(
    sample_id = c(1, 2),
    sand_percent = c(40, NA),
    silt_percent = c(40, NA),
    clay_percent = c(20, 30)
  )

  expect_warning(
    validate_texture_fractions(df),
    "fewer than two fractions"
  )
})

test_that("warns when exactly one fraction is missing", {
  df <- data.frame(
    sample_id = c(1, 2),
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
    sample_id = c(1, 2),
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA
  )

  expect_warning(
    validate_texture_fractions(df),
    "fewer than two fractions"
  )
})

test_that("does not warn when all fractions are missing but texture is provided", {
  df <- data.frame(
    sample_id = c(1, 2),
    texture = "Loam",
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA
  )

  expect_no_warning(
    validate_texture_fractions(df)
  )
})

# Validation: fraction range ---------------------------------------------------

test_that("errors when any fraction is outside 0–100", {
  df <- data.frame(
    sample_id = c(1, 2),
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
    sample_id = c(1, 2),
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
    sample_id = c(1, 2, 3, 4, 5),
    # Sample 1: missing two fractions → error
    sand_percent = c(NA, 110, 40, NA, NA),

    # Sample 2: out of range value → error
    silt_percent = c(NA, 10, 40, 30, NA),

    # Sample 3: sums to != 100 → error
    clay_percent = c(30, 10, 30, 40, NA)
  )

  # Sample breakdown:
  # 1 → two missing fractions (warning)
  # 2 → sand > 100 (error)
  # 3 → sums to 110 (error)
  # 4 → exactly one missing fraction (warning)
  # 5 → all fractions missing (warning)

  expect_error(
    validate_texture_fractions(df),
    regexp = paste(
      "validation failed", # overall failure
      "between 0 and 100", # out of range
      "sum to 100", # invalid sum
      "fewer than two fractions", # insufficient data
      "missing one fraction", # computed fraction
      sep = ".*"
    )
  )
})

# Completion: compute missing fraction -----------------------------------------

test_that("complete_texture_fractions computes the missing fraction correctly", {
  df <- data.frame(
    sample_id = 1,
    sand_percent = NA,
    silt_percent = 30,
    clay_percent = 20
  )

  out <- df |>
    complete_texture_fractions()

  expect_equal(out$sand_percent, 50)
})

# Completion: computes new fraction --------------------------------------------

test_that("complete_texture_fractions computes the missing fraction after validation", {
  df <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40
  )

  # Catch the warning from exactly one missing fraction
  expect_warning(
    validated <- validate_texture_fractions(df),
    regexp = "missing one fraction"
  )

  # The missing column should exist but still NA until completed
  expect_equal(validated$clay_percent, NA_real_)

  # Compute the missing fraction
  completed <- complete_texture_fractions(validated)
  expect_equal(completed$clay_percent, 20)
})

# Classification: USDA texture classes -----------------------------------------

test_that("assign_texture_class assigns expected USDA texture classes", {
  df <- data.frame(
    sample_id = c(1, 2, 3),
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
    sample_id = 1,
    texture = NA,
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = NA
  )

  expect_warning(
    out <- classify_texture(df),
    "skipped"
  )

  expect_true(is.na(out$texture))
})


# End-to-end: classify_texture -------------------------------------------------

test_that("classify_texture completes, validates, and classifies correctly", {
  df <- data.frame(
    sample_id = c(1, 2),
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

test_that("CLI messages correctly pluralize 'Sample' vs 'Samples'", {
  df_single <- data.frame(
    sample_id = 1,
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = 30
  )

  expect_warning(
    validate_texture_fractions(df_single),
    "Sample 1 has fewer than two fractions"
  )

  df_plural <- data.frame(
    sample_id = c(1, 2),
    sand_percent = c(NA, NA),
    silt_percent = c(NA, NA),
    clay_percent = c(30, 40)
  )

  expect_warning(
    validate_texture_fractions(df_plural),
    "Samples 1 and 2 have fewer than two fractions"
  )
})

test_that("CLI messages correctly pluralize 'is' vs 'are'", {
  df_single <- data.frame(
    sample_id = 1,
    sand_percent = NA,
    silt_percent = 40,
    clay_percent = 60
  )

  expect_warning(
    validate_texture_fractions(df_single),
    "Sample 1 is missing one fraction"
  )

  df_plural <- data.frame(
    sample_id = c(1, 2),
    sand_percent = c(NA, NA),
    silt_percent = c(40, 30),
    clay_percent = c(60, 70)
  )

  expect_warning(
    validate_texture_fractions(df_plural),
    "Samples 1 and 2 are missing one fraction"
  )
})

# Sync dictionary with new texture columns -------------------------------------

test_that("returns unchanged dictionary if no new columns", {
  data <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40,
    clay_percent = 20
  )

  dictionary <- data.frame(
    measurement_group = rep("Mediciones físicas", 3),
    column_name = c("sand_percent", "silt_percent", "clay_percent"),
    abbr = c("Arena", "Limo", "Arcilla"),
    unit = c("%", "%", "%"),
    stringsAsFactors = FALSE
  )

  out <- sync_dictionary_texture(data, dictionary, "Spanish")

  expect_equal(out, dictionary)
})

test_that("adds texture column if missing", {
  data <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40,
    clay_percent = 20,
    texture = "Loam"
  )

  dictionary <- data.frame(
    measurement_group = rep("Mediciones físicas", 3),
    column_name = c("sand_percent", "silt_percent", "clay_percent"),
    abbr = c("Arena", "Limo", "Arcilla"),
    unit = c("%", "%", "%"),
    stringsAsFactors = FALSE
  )

  out <- sync_dictionary_texture(data, dictionary, language = "Spanish")

  expect_true("texture" %in% out$column_name)
  expect_equal(out$column_name[1], "texture")
  expect_equal(out$abbr[1], "Textura")
})

test_that("adds missing fractions to dictionary", {
  data <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40,
    clay_percent = 20
  )

  # Dictionary missing silt_percent and clay_percent
  dictionary <- data.frame(
    measurement_group = "Mediciones físicas",
    column_name = "sand_percent",
    abbr = "Arena",
    unit = "%",
    stringsAsFactors = FALSE
  )

  out <- sync_dictionary_texture(data, dictionary, language = "Spanish")

  expect_true(all(
    c("sand_percent", "silt_percent", "clay_percent") %in% out$column_name
  ))
  expect_equal(out$column_name[1], "sand_percent") # original first
  expect_equal(out$measurement_group[1], "Mediciones físicas")
})

test_that("adds texture/fractions when physical group is missing", {
  data <- data.frame(
    sample_id = 1,
    texture = "Loam",
    sand_percent = 40,
    silt_percent = 40,
    clay_percent = 20
  )

  # Dictionary has only a chemical group
  dictionary <- data.frame(
    measurement_group = "Mediciones químicas",
    column_name = "pH",
    abbr = "pH",
    unit = "",
    stringsAsFactors = FALSE
  )

  out <- sync_dictionary_texture(data, dictionary, language = "Spanish")

  # Physical group rows added
  expect_true(all(
    c("texture", "sand_percent", "silt_percent", "clay_percent") %in%
      out$column_name
  ))

  # Original chemical row still exists
  expect_true("pH" %in% out$column_name)

  # Texture and fractions appear at the top of the dictionary
  phys_rows <- out[out$measurement_group == "Mediciones físicas", ]
  expect_equal(
    phys_rows$column_name,
    c("texture", "sand_percent", "silt_percent", "clay_percent")
  )
})

test_that("respects language argument for abbreviations", {
  data <- data.frame(
    sample_id = 1,
    sand_percent = 40,
    silt_percent = 40,
    clay_percent = 20,
    texture = "Loam"
  )

  dictionary_es <- data.frame(
    measurement_group = "Mediciones físicas",
    column_name = c("sand_percent"),
    abbr = c("Arena"),
    unit = c("%"),
    stringsAsFactors = FALSE
  )

  dictionary_en <- data.frame(
    measurement_group = "Physical",
    column_name = c("sand_percent"),
    abbr = c("Sand"),
    unit = c("%"),
    stringsAsFactors = FALSE
  )

  out_es <- sync_dictionary_texture(data, dictionary_es, language = "Spanish")
  expect_equal(out_es$abbr[1], "Textura")
  expect_equal(out_es$abbr[2:4], c("Arena", "Limo", "Arcilla"))

  out_en <- sync_dictionary_texture(data, dictionary_en, language = "English")
  expect_equal(out_en$abbr[1], "Texture")
  expect_equal(out_en$abbr[2:4], c("Sand", "Silt", "Clay"))
})
