# Tests for prepare functions

test_that("prepare_rio works with minimal RIO data", {
  rio <- data.frame(
    Croho_onderdeel = c("12345", "12346"),
    Naam_opleiding = c("Test Opleiding 1", "Test Opleiding 2"),
    Datum_begin_opleiding = c("2020-01-01 00:00:00", "2021-01-01 00:00:00"),
    Datum_einde_opleiding = c(NA, NA),
    Datum_einde_instroom = c(NA, NA),
    Niveau = c("B", "M")
  )

  result <- prepare_rio(rio, year = 2024, institution_brin = "21XX", create_synthetic = FALSE)

  expect_type(result, "list")
  expect_named(result, c("rio", "rio_per_jaar"))
  expect_s3_class(result$rio$Datum_begin_opleiding, "Date")
})


test_that("prepare_rio assigns status codes correctly", {
  future_date <- as.character(Sys.Date() + 365)
  past_date <- as.character(Sys.Date() - 365)

  rio <- data.frame(
    Croho_onderdeel = c("12345", "12346", "12347"),
    Naam_opleiding = c("Future", "Past", "Current"),
    Datum_begin_opleiding = c(
      paste0(future_date, " 00:00:00"),
      "2010-01-01 00:00:00",
      "2020-01-01 00:00:00"
    ),
    Datum_einde_opleiding = c(
      NA,
      paste0(past_date, " 00:00:00"),
      NA
    ),
    Datum_einde_instroom = c(NA, NA, NA),
    Niveau = c("B", "M", "B")
  )

  result <- prepare_rio(rio, year = 2024, institution_brin = "21XX", create_synthetic = FALSE)

  expect_true("Code_stand_record" %in% colnames(result$rio))
  expect_equal(result$rio$Code_stand_record[1], "TOEKOMST")
  expect_equal(result$rio$Code_stand_record[2], "HISTORISCH")
  expect_equal(result$rio$Code_stand_record[3], "ACTUEEL")
})


test_that("prepare_enrollments_mapping handles missing columns gracefully", {
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

  # This should not error, even with missing columns
  # The function will just skip mappings for columns that don't exist
  expect_error(
    prepare_enrollments_mapping(enrollments),
    NA  # No error expected
  )
})
