# Tests for prepare functions using synthetic data

test_that("prepare_enrollments_mapping works with real data", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Load small sample
  enrollments <- read.csv2(enrollments_path)[1:50, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))

  # Test mapping
  result <- prepare_enrollments_mapping(enrollments_clean)

  expect_s3_class(result, "data.frame")
  expect_gt(ncol(result), ncol(enrollments_clean))

  # Check that mapped columns exist
  expect_true("DEM_Geslacht_naam" %in% colnames(result))
  expect_true("INS_Opleidingsvorm_naam" %in% colnames(result))

  # Check that mappings produced values (not all NA)
  expect_lt(mean(is.na(result$DEM_Geslacht_naam)), 0.5)
})


test_that("prepare_enrollments_supplemental works with real data", {
  synth_data_path <- file.path("..", "..", "synth_data")
  skip_if_not(dir.exists(synth_data_path), "Synthetic data not available")

  enrollments_path <- file.path(synth_data_path, "EV299XX24_DEMO_decoded.csv")
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Load small sample
  enrollments <- read.csv2(enrollments_path)[1:50, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))
  enrollments_mapped <- prepare_enrollments_mapping(enrollments_clean)

  # Test supplemental
  result <- prepare_enrollments_supplemental(enrollments_mapped, year = 2024, institution_brin = "21XX")

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 50)
})


test_that("prepare_rio works with real RIO data", {
  # Get real RIO data
  rio_data <- get_rio(force_download = FALSE)

  expect_s3_class(rio_data, "data.frame")
  expect_gt(nrow(rio_data), 0)

  # Test prepare_rio
  result <- prepare_rio(rio_data, year = 2024, institution_brin = "21XX", create_synthetic = TRUE)

  expect_type(result, "list")
  expect_named(result, c("rio", "rio_per_jaar"))
  expect_s3_class(result$rio, "data.frame")
  expect_s3_class(result$rio_per_jaar, "data.frame")

  # Check date conversion
  expect_s3_class(result$rio$Datum_begin_opleiding, "Date")

  # Check status codes were assigned
  expect_true("Code_stand_record" %in% colnames(result$rio))
})
