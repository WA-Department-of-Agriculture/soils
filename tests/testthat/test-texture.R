test_that("errors when one row has two or more missing soil fractions", {
  df <- data.frame(
    sand_percent = NA,
    silt_percent = NA,
    clay_percent = 20
  )

  expect_error(
    complete_texture(df),
    regexp = "row with insufficient data"
  )

  expect_error(
    complete_texture(df),
    regexp = "1)."
  )
})

test_that("errors when multiple rows have two or more missing soil fractions", {
  df <- data.frame(
    sand_percent = c(NA, 50, NA),
    silt_percent = c(NA, 40, 10),
    clay_percent = c(20, 10, NA)
  )

  expect_error(
    complete_texture(df),
    regexp = "rows with insufficient data"
  )

  expect_error(
    complete_texture(df),
    regexp = "1 and 3)"
  )
})

test_that("warns and computes missing fraction for single row", {
  df <- data.frame(
    sand_percent = c(NA, 40, 30),
    silt_percent = c(40, 40, 30),
    clay_percent = c(50, 20, 40)
  )

  expect_warning(
    out <- complete_texture(df),
    regexp = "One soil fraction is missing"
  )

  expect_warning(
    out <- complete_texture(df),
    regexp = "row.*(row: 1)"
  )

  expect_equal(out$sand_percent[1], 10)
})

test_that("warns and computes missing fraction for multiple rows", {
  df <- data.frame(
    sand_percent = c(NA, 40, 30),
    silt_percent = c(40, 40, 30),
    clay_percent = c(50, 20, NA)
  )

  expect_warning(
    out <- complete_texture(df),
    regexp = "One soil fraction is missing"
  )

  expect_warning(
    out <- complete_texture(df),
    regexp = "1 and 3)"
  )

  expect_equal(out$sand_percent[1], 10)
  expect_equal(out$clay_percent[3], 40)
})


test_that("errors when single row has invalid soil fraction sum", {
  df <- data.frame(
    sand_percent = c(40, 30),
    silt_percent = c(40, 30),
    clay_percent = c(20, 50) # row 2 sums to 110
  )

  expect_error(
    complete_texture(df),
    regexp = "invalid sum"
  )

  expect_error(
    complete_texture(df),
    regexp = "row.*(row 2)."
  )
})

test_that("errors when multiple rows have invalid soil fraction sums", {
  df <- data.frame(
    sand_percent = c(40, 30),
    silt_percent = c(40, 30),
    clay_percent = c(30, 50) # both rows invalid
  )

  expect_error(
    complete_texture(df),
    regexp = "invalid sums"
  )

  expect_error(
    complete_texture(df),
    regexp = "rows.*1.*2)"
  )
})


test_that("assigns Sandy Loam when fractions are complete", {
  df <- data.frame(
    sand_percent = 60,
    silt_percent = 30,
    clay_percent = 10
  )

  out <- complete_texture(df)

  expect_equal(out$texture, "Sandy Loam")
})

test_that("does not drop rows or reorder data", {
  df <- data.frame(
    sand_percent = c(60, 40),
    silt_percent = c(30, 40),
    clay_percent = c(10, 20)
  )

  out <- complete_texture(df)

  expect_equal(nrow(out), 2)
  expect_equal(out$sand_percent, df$sand_percent)
})

test_that("handles edge case near texture thresholds", {
  df <- data.frame(
    sand_percent = 52,
    silt_percent = 28,
    clay_percent = 20
  )

  out <- complete_texture(df)

  expect_true(out$texture == "Loam")
})

test_that("skips complete_texture() when fraction columns are missing", {
  df <- data.frame(
    sample_id = c("A", "B"),
    sand_percent = c(1, 2)
  )

  expect_warning(
    out <- complete_texture(df),
    regexp = "Soil texture classification skipped."
  )

  # Data frame should be unchanged
  expect_equal(out, df)
})

test_that("all combinations of sand, silt, clay are classified", {
  # Generate all combinations that sum to 100
  df <- expand.grid(
    sand_percent = 0:100,
    silt_percent = 0:100,
    clay_percent = 0:100
  ) |>
    dplyr::filter(sand_percent + silt_percent + clay_percent == 100)

  out <- complete_texture(df) |>
    dplyr::filter(is.na(texture)) |>
    nrow()

  # There should be no rows with missing texture
  expect_equal(out, 0)
})
