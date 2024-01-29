test_that("create_soils returns appropriate error", {
  expect_snapshot(
    error = TRUE,
    create_soils()
  )
})
