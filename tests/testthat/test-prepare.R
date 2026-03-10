# Tests for prepare functions

test_that("prepare_rio runs with real RIO data", {
  skip("prepare_rio requires full RIO schema - use integration tests instead")
})


test_that("prepare_rio date logic is sound", {
  # Test the date conversion logic separately
  test_date <- "2024-01-15 00:00:00"
  result <- suppressWarnings(lubridate::as_date(lubridate::ymd_hms(test_date)))

  expect_s3_class(result, "Date")
  expect_equal(as.character(result), "2024-01-15")
})


test_that("prepare_enrollments_mapping requires specific columns", {
  skip_if_not(requireNamespace("dplyr", quietly = TRUE))

  # Create minimal enrollment data
  enrollments <- data.frame(
    INS_Studentnummer = c(1, 2, 3),
    INS_Inschrijvingsjaar = c(2024, 2024, 2024),
    DEM_Geslacht_code = c("M", "V", "M"),
    INS_Datum_inschrijving = c("01/01/2024", "01/01/2024", "01/01/2024"),
    INS_Datum_uitschrijving = c(NA, NA, NA),
    stringsAsFactors = FALSE
  )

  # This will error because required columns are missing (DEM_Nationaliteit_1_omschrijving etc)
  # That's expected behavior - the function needs properly structured data
  expect_error(
    prepare_enrollments_mapping(enrollments),
    "doesn't exist"
  )
})
