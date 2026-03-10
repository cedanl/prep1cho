# Tests for combine functions using synthetic data

test_that("combine_enrollments_rio works with real data", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Prepare enrollments (small sample)
  enrollments <- read.csv2(enrollments_path)[1:50, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))
  enrollments_prep <- enrollments_clean |>
    prepare_enrollments_mapping() |>
    prepare_enrollments_supplemental(year = 2024, institution_brin = "21XX")

  # Prepare RIO
  rio_data <- get_rio(force_download = FALSE)
  rio_prepared <- prepare_rio(rio_data, year = 2024, institution_brin = "21XX")

  # Test combine
  result <- combine_enrollments_rio(enrollments_prep, rio_prepared$rio_per_jaar)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)

  # Check that columns were added (more than input)
  expect_gt(ncol(result), ncol(enrollments_prep))
})


test_that("combine_enrollments_calculations works", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Prepare data
  enrollments <- read.csv2(enrollments_path)[1:50, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))
  enrollments_prep <- enrollments_clean |>
    prepare_enrollments_mapping() |>
    prepare_enrollments_supplemental(year = 2024, institution_brin = "21XX")

  rio_data <- get_rio(force_download = FALSE)
  rio_prepared <- prepare_rio(rio_data, year = 2024, institution_brin = "21XX")

  enrollments_combined <- combine_enrollments_rio(enrollments_prep, rio_prepared$rio_per_jaar)

  # Test calculations
  result <- combine_enrollments_calculations(enrollments_combined)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
})


test_that("combine_enrollments_final works", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Full pipeline up to final step
  enrollments <- read.csv2(enrollments_path)[1:50, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))
  enrollments_prep <- enrollments_clean |>
    prepare_enrollments_mapping() |>
    prepare_enrollments_supplemental(year = 2024, institution_brin = "21XX")

  rio_data <- get_rio(force_download = FALSE)
  rio_prepared <- prepare_rio(rio_data, year = 2024, institution_brin = "21XX")

  enrollments_combined <- enrollments_prep |>
    combine_enrollments_rio(rio_prepared$rio_per_jaar) |>
    combine_enrollments_calculations()

  # Test final
  result <- combine_enrollments_final(enrollments_combined)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
})
