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
  # All missing or empty
  expect_true(is_column_empty(col1))
  # At least one non-missing value
  expect_false(is_column_empty(col2))
})

# has_complete_row() -----------------------------------------------------------

test_that("has_complete_row detects rows with complete columns", {
  df <- data.frame(a = c(1, NA), b = c(NA, 2))
  # No row has both columns complete
  expect_false(has_complete_row(df, c("a", "b")))
  # Column "a" has at least one non-NA
  expect_true(has_complete_row(df, "a"))
})

# summarize_by_var() -----------------------------------------------------------

test_that("summarize_by_var() aborts when required columns are missing", {
  # Minimal base R data frames
  results_long <- data.frame(
    sample_id = 1:3,
    measurement_group = c("A", "A", "B"),
    abbr = c("pH", "OM", "N"),
    value = c(5.5, 3.2, 1.1),
    crop = c("Apple", "Apple", "Apple"),
    stringsAsFactors = FALSE
  )

  producer_samples <- data.frame(
    measurement_group = c("A", "B"),
    abbr = c("pH", "N"),
    value = c(5.5, 1.1),
    crop = c("Apple", "Apple"),
    stringsAsFactors = FALSE
  )

  # Case 1: missing columns in producer_samples
  bad_producer <- producer_samples[, c("measurement_group", "value", "crop")]
  expect_error(
    summarize_by_var(results_long, bad_producer, crop),
    regexp = "`producer_samples` must have the required column: abbr"
  )

  # Case 2: missing columns in results_long
  bad_results <- results_long[, c("sample_id", "measurement_group", "crop")]
  expect_error(
    summarize_by_var(bad_results, producer_samples, crop),
    regexp = "`results_long` must have the required columns: abbr and value"
  )

  # Case 3: missing var in results_long
  expect_error(
    summarize_by_var(results_long, producer_samples, non_existent_var),
    regexp = "*Required because it was supplied to the `var` argument"
  )

  # Case 4: 3+ missing columns to test bullets
  bad_results3 <- results_long[, c("sample_id")]
  expect_error(
    summarize_by_var(bad_results3, bad_producer, sample_id),
    regexp = "`results_long` must have the required columns:"
  )
})

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

test_that("summarize_by_project() aborts when required columns are missing", {
  results_long <- data.frame(
    sample_id = 1:3,
    measurement_group = c("A", "A", "B"),
    value = c(5.5, 3.2, 1.1),
    crop = c("Apple", "Apple", "Apple"),
    stringsAsFactors = FALSE
  )

  expect_error(
    summarize_by_project(results_long),
    regexp = "`results_long` must have the required column: abbr"
  )
})

test_that("summarize_by_project calculates project averages and texture mode", {
  results_long <- data.frame(
    sample_id = c("S1", "S2", "S3"),
    measurement_group = c("Chemical", "Chemical", "Chemical"),
    abbr = c("ph", "ph", "ph"),
    value = c(6.5, 7.0, 6.8),
    texture = c("Silt loam", "Silt loam", "Clay loam"),
    stringsAsFactors = FALSE
  )

  summary <- summarize_by_project(results_long)

  expect_true("Field or Average" %in% names(summary))
  expect_true("Texture" %in% names(summary))
  expect_type(summary$value, "double")
})
