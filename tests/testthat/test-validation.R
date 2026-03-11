# Tests for validation functions

test_that("validate_enrollments_input rejects non-data frames", {
  expect_error(
    validate_enrollments_input(NULL),
    "moet een data frame zijn"
  )

  expect_error(
    validate_enrollments_input(c(1, 2, 3)),
    "moet een data frame zijn"
  )
})


test_that("validate_enrollments_input rejects empty data", {
  expect_error(
    validate_enrollments_input(data.frame()),
    "leeg.*0 rijen"
  )

  empty_df <- data.frame(matrix(ncol = 0, nrow = 10))
  expect_error(
    validate_enrollments_input(empty_df),
    "geen kolommen"
  )
})


test_that("validate_enrollments_input checks required columns", {
  incomplete_data <- data.frame(
    INS_Studentnummer = c(1, 2, 3),
    INS_Inschrijvingsjaar = c(2024, 2024, 2024)
    # Missing DEM_Geslacht_code
  )

  expect_error(
    validate_enrollments_input(incomplete_data),
    "Ongeldige inschrijvingsdata"
  )
})


test_that("validate_enrollments_input checks year ranges", {
  bad_year_data <- data.frame(
    INS_Studentnummer = c(1, 2),
    INS_Inschrijvingsjaar = c(1850, 2024),  # 1850 is unrealistic
    DEM_Geslacht_code = c("M", "V")
  )

  expect_error(
    validate_enrollments_input(bad_year_data),
    "Ongeldige inschrijvingsjaren"
  )
})


test_that("validate_enrollments_input passes valid data", {
  valid_data <- data.frame(
    INS_Studentnummer = c(1, 2, 3),
    INS_Inschrijvingsjaar = c(2022, 2023, 2024),
    DEM_Geslacht_code = c("M", "V", "M")
  )

  expect_error(
    validate_enrollments_input(valid_data),
    NA
  )
})


test_that("validate_enrollments_input rejects all-NA student numbers", {
  bad_data <- data.frame(
    INS_Studentnummer = c(NA, NA, NA),
    INS_Inschrijvingsjaar = c(2024, 2024, 2024),
    DEM_Geslacht_code = c("M", "V", "M")
  )

  expect_error(
    validate_enrollments_input(bad_data),
    "alleen NA"
  )
})


test_that("validate_rio_input rejects non-data frames", {
  expect_error(
    validate_rio_input(NULL),
    "moet een data frame zijn"
  )
})


test_that("validate_rio_input rejects empty data", {
  expect_error(
    validate_rio_input(data.frame()),
    "leeg.*0 rijen"
  )
})


test_that("validate_rio_input checks required columns", {
  incomplete_rio <- data.frame(
    OPL_Code_in_jaar = c("12345")
    # Missing OPL_Opleidingsnaam_CROHO
  )

  expect_error(
    validate_rio_input(incomplete_rio),
    "Ongeldige RIO data"
  )
})


test_that("validate_rio_input passes valid data", {
  valid_rio <- data.frame(
    OPL_Code_in_jaar = c("12345", "67890"),
    OPL_Opleidingsnaam_CROHO = c("Test 1", "Test 2")
  )

  expect_error(
    validate_rio_input(valid_rio),
    NA
  )
})


test_that("validate_data_types checks numeric columns", {
  test_data <- data.frame(
    year = c("2024", "2025"),  # Should be numeric
    name = c("A", "B")
  )

  expect_error(
    validate_data_types(test_data, list(year = "numeric")),
    "wrong type.*numeric"
  )
})


test_that("validate_data_types checks character columns", {
  test_data <- data.frame(
    year = c(2024, 2025),
    name = c(1, 2)  # Should be character
  )

  expect_error(
    validate_data_types(test_data, list(name = "character")),
    "wrong type.*character"
  )
})


test_that("validate_data_types checks Date columns", {
  test_data <- data.frame(
    date = c("2024-01-01", "2024-01-02")  # Should be Date
  )

  expect_error(
    validate_data_types(test_data, list(date = "Date")),
    "wrong type.*Date"
  )
})


test_that("validate_data_types passes correct types", {
  test_data <- data.frame(
    year = c(2024, 2025),
    name = c("A", "B"),
    date = as.Date(c("2024-01-01", "2024-01-02"))
  )

  expect_error(
    validate_data_types(test_data, list(
      year = "numeric",
      name = "character",
      date = "Date"
    )),
    NA
  )
})


test_that("validate_data_types skips missing columns", {
  test_data <- data.frame(x = 1:3)

  expect_error(
    validate_data_types(test_data, list(
      x = "numeric",
      y = "character"  # y doesn't exist, should be skipped
    )),
    NA
  )
})
