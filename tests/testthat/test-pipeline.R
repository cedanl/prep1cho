# Tests for pipeline orchestration

test_that("run_pipeline works with synthetic data", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Load small sample for speed
  enrollments <- read.csv2(enrollments_path)[1:25, ]

  # Run full pipeline
  result <- suppressMessages(run_pipeline(
    enrollments,
    year = 2024,
    institution_brin = "21XX",
    download_rio = FALSE
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 25)
  expect_gt(ncol(result), ncol(enrollments))
})


test_that("run_pipeline can download RIO data", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  enrollments <- read.csv2(enrollments_path)[1:25, ]

  # Run pipeline without providing RIO (should download/get it)
  result <- suppressMessages(run_pipeline(
    enrollments,
    rio_data = NULL,  # Force it to get RIO data itself
    year = 2024,
    institution_brin = "21XX",
    download_rio = FALSE
  ))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 25)
})


test_that("run_pipeline validates input", {
  # Invalid input should error
  expect_error(
    suppressMessages(run_pipeline(NULL))
  )

  # Empty data frame should error
  empty_df <- data.frame()
  expect_error(
    suppressMessages(run_pipeline(empty_df))
  )
})


test_that("run_pipeline returns expected structure", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  enrollments <- read.csv2(enrollments_path)[1:10, ]

  result <- suppressMessages(run_pipeline(enrollments))

  # Check it's a data frame
  expect_s3_class(result, "data.frame")

  # Check it has both original and enriched columns
  expect_true(any(grepl("^INS_", colnames(result))))
  expect_true(any(grepl("^DEM_", colnames(result))))
  expect_true(any(grepl("^OPL_", colnames(result))))
})
