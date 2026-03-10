# Integration tests for full pipeline

test_that("full pipeline runs without errors on synthetic data", {
  skip_if_not(dir.exists("synth_data"), "Synthetic data not available")

  # Load synthetic enrollment data
  enrollments_path <- "synth_data/EV299XX24_DEMO_decoded.csv"
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  enrollments <- read.csv2(enrollments_path)

  # Test each stage
  expect_error({
    # Stage 1: Get RIO
    rio_data <- get_rio(force_download = FALSE)

    # Stage 2: Audit
    enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))

    # Stage 3: Prepare RIO
    rio_prepared <- prepare_rio(rio_data, year = 2024, institution_brin = "21XX")

    # Stage 4: Prepare enrollments (just mapping, skip supplemental for speed)
    enrollments_prep <- prepare_enrollments_mapping(enrollments_clean)
  }, NA)  # Expect no errors
})


test_that("mapping functions produce expected column counts", {
  skip_if_not(dir.exists("synth_data"), "Synthetic data not available")

  enrollments_path <- "synth_data/EV299XX24_DEMO_decoded.csv"
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  enrollments <- read.csv2(enrollments_path)
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))

  initial_cols <- ncol(enrollments_clean)

  enrollments_mapped <- prepare_enrollments_mapping(enrollments_clean)

  # Mapping should add columns (new mapped columns)
  expect_gt(ncol(enrollments_mapped), initial_cols)
})


test_that("mappings do not produce excessive NA values", {
  skip_if_not(dir.exists("synth_data"), "Synthetic data not available")

  enrollments_path <- "synth_data/EV299XX24_DEMO_decoded.csv"
  skip_if_not(file.exists(enrollments_path), "Synthetic enrollment data not found")

  # Test with small sample for speed
  enrollments <- read.csv2(enrollments_path)[1:100, ]
  enrollments_clean <- suppressMessages(suppressWarnings(audit_enrollments(enrollments)))
  enrollments_mapped <- prepare_enrollments_mapping(enrollments_clean)

  # Check for critical mapped columns that should NOT have high NA rates
  # (assuming the source data has these values)
  critical_columns <- c(
    "DEM_Geslacht_naam",
    "INS_Opleidingsvorm_naam"
  )

  for (col in critical_columns) {
    if (col %in% colnames(enrollments_mapped)) {
      na_rate <- mean(is.na(enrollments_mapped[[col]]))

      # If source column had data, mapped column should not be >90% NA
      # (some NA is OK if source had NA, but >90% suggests mapping failure)
      expect_lt(
        na_rate,
        0.9,
        label = paste0(col, " has too many NAs (", round(na_rate * 100, 1), "%)"),
        info = "Mapping may have failed - check that source values match mapping table"
      )
    }
  }
})
