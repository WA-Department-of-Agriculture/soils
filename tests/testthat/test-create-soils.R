test_that("create_soils returns appropriate error", {
  expect_snapshot_error(
    create_soils()
  )
})
