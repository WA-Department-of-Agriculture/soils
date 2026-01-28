# calculate_mode() -------------------------------------------------------------

test_that("calculate_mode returns the most frequent value", {
  vec <- c("A", "B", "B", "C", "B", NA)
  expect_equal(calculate_mode(vec), "B") # "B" appears most frequently
})

# pull_unique() ----------------------------------------------------------------

test_that("pull_unique returns unique values of a column", {
  df <- data.frame(crop = c("wheat", "corn", "wheat"))
  # Order doesn't matter; expect the two unique crops
  expect_equal(sort(pull_unique(df, crop)), sort(c("wheat", "corn")))
})

# is_column_empty() ------------------------------------------------------------

test_that("is_column_empty detects empty columns", {
  col1 <- c(NA, "", NA)
  col2 <- c(1, NA)
  expect_true(is_column_empty(col1)) # All missing or empty
  expect_false(is_column_empty(col2)) # At least one non-missing value
})

# has_complete_row() -----------------------------------------------------------

test_that("has_complete_row detects rows with complete columns", {
  df <- data.frame(a = c(1, NA), b = c(NA, 2))
  expect_false(has_complete_row(df, c("a", "b"))) # No row has both columns complete
  expect_true(has_complete_row(df, "a")) # Column "a" has at least one non-NA
})

# summarize_by_var() -----------------------------------------------------------

test_that("summarize_by_var calculates averages and texture per group", {
  # Numeric-only example data for summarization
  df_numeric <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    measurement_group = c("Chemical", "Chemical", "Chemical"),
    abbr = c("ph", "ph", "ph"),
    value = c(6.5, 7.0, 6.8),
    texture = c("Silt loam", "Silt loam", "Clay loam"),
    stringsAsFactors = FALSE
  )

  # Producer samples: pick first sample
  producer_samples <- df_numeric[df_numeric$sample_id == "S1", ]

  summary <- summarize_by_var(df_numeric, producer_samples, sample_id)

  expect_true("Field or Average" %in% names(summary))
  expect_true("Texture" %in% names(summary))
})

test_that("summarize_by_var filters out groups with only 1 sample", {
  # Example long-format data
  results_long <- data.frame(
    sample_id = c("S1", "S2", "S3", "S4", "S5"),
    measurement_group = c(
      "Chemical",
      "Chemical",
      "Chemical",
      "Chemical",
      "Chemical"
    ),
    abbr = c("pH", "pH", "pH", "pH", "pH"),
    value = c(6.5, 7.0, 6.8, 5.5, 5.7),
    crop = c("Corn", "Corn", "Corn", "Corn", "Wheat"),
    texture = c("Loam", "Loam", "Loam", "Clay", "Clay")
  )

  # Producer samples (only including a subset)
  producer_samples <- data.frame(
    measurement_group = c("Chemical", "Chemical"),
    abbr = c("pH", "pH"),
    value = c(6.5, 5.5),
    crop = c("Corn", "Wheat")
  )

  # Summarize by crop
  summary <- summarize_by_var(results_long, producer_samples, crop)

  # Check that all groups with n = 1 are removed
  expect_false(any(grepl(
    "^Wheat Average \\n\\(1 Fields\\)",
    summary$`Field or Average`
  )))

  # Check that Corn group remains
  expect_true(any(grepl("^Corn Average", summary$`Field or Average`)))

  # Check that the value column is numeric and average is correct
  corn_summary <- summary |>
    dplyr::filter(grepl("^Corn Average", `Field or Average`))
  expect_equal(
    round(corn_summary$value, 2),
    round(mean(c(6.5, 7.0, 6.8, 5.5)), 4)
  )
})


# summarize_by_project() -------------------------------------------------------

test_that("summarize_by_project calculates project averages and texture mode", {
  # Numeric-only example data for summarization
  df_numeric <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    measurement_group = c("Chemical", "Chemical", "Chemical"),
    abbr = c("ph", "ph", "ph"),
    value = c(6.5, 7.0, 6.8),
    texture = c("Silt loam", "Silt loam", "Clay loam"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_by_project(df_numeric)

  expect_true("Field or Average" %in% names(summary))
  expect_true("Texture" %in% names(summary))
  expect_type(summary$value, "double")
})
