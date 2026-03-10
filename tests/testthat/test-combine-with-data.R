# Tests for combine functions using synthetic data

test_that("combine_enrollments_rio selects and aggregates RIO columns correctly", {
  # Create minimal test data
  rio_test <- data.frame(
    OPL_Code_in_jaar = c("12345", "12345", "67890"),
    OPL_Opleidingsnaam_CROHO_actueel = c("Test 1", "Test 1", "Test 2"),
    OPL_Nominale_studieduur = c(4, 4, 3),
    OPL_Bekostiging = c("B", "B", "H"),
    OPL_Bekostigingsduur = c(4, 4, 3),
    extra_col = c("x", "y", "z")  # Should be dropped
  )

  enrollments_test <- data.frame(
    INS_Studentnummer = c(1, 2),
    OPL_Code_in_jaar = c("12345", "67890")
  )

  result <- suppressMessages(combine_enrollments_rio(enrollments_test, rio_test))

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)

  # Check that RIO columns were added
  expect_true("OPL_Opleidingsnaam_CROHO_actueel" %in% colnames(result))
  expect_true("OPL_Nominale_studieduur" %in% colnames(result))

  # Check that extra columns were not added
  expect_false("extra_col" %in% colnames(result))

  # Check aggregation worked (duplicates removed)
  expect_equal(result$OPL_Nominale_studieduur[1], 4)
})


test_that("combine_enrollments_rio handles missing OPL_Code", {
  rio_test <- data.frame(
    OPL_Code_in_jaar = c("12345"),
    OPL_Opleidingsnaam_CROHO_actueel = c("Test"),
    OPL_Nominale_studieduur = c(4),
    OPL_Bekostiging = c("B"),
    OPL_Bekostigingsduur = c(4)
  )

  enrollments_test <- data.frame(
    INS_Studentnummer = c(1, 2),
    OPL_Code_in_jaar = c("12345", "99999")  # 99999 not in RIO
  )

  result <- suppressMessages(combine_enrollments_rio(enrollments_test, rio_test))

  expect_equal(nrow(result), 2)
  # Row with unknown code should have NA for RIO fields
  expect_true(is.na(result$OPL_Nominale_studieduur[2]))
})


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


test_that("combine_enrollments_calculations creates outflow types correctly", {
  # Test data with different scenarios
  test_data <- data.frame(
    INS_Studentnummer = c(1, 2, 3, 4),
    OPL_code_historisch = c("A", "B", "C", "D"),
    INS_Uitval = c(TRUE, FALSE, FALSE, FALSE),
    INS_Aantal_inschrijvingen = c(2, 3, 4, 3),
    INS_Datum_tekening_diploma = c(NA, "2024-01-01", "2024-01-01", "2024-01-01"),
    INS_Aantal_inschrijvingen_tot_diploma = c(NA, 3, 2, 4),
    OPL_Nominale_studieduur = c(3, 3, 3, 3),
    INS_Studiejaar = c(2, 3, 4, 3),
    INS_Inschrijvingsjaar = c(2022, 2021, 2020, 2021),
    INS_Inschrijvingsjaar_EOI = c(2022, 2021, 2020, 2021),
    INS_Opleidingsfase_actueel_naam = c("B", "B", "B", "B"),
    INS_Inschrijvingsjaar_max = c(2023, 2023, 2023, 2023),
    OPL_Code_in_jaar = c("1", "2", "3", "4")
  )

  result <- combine_enrollments_calculations(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 4)

  # Check that SUC_Type_uitstroom was created
  expect_true("SUC_Type_uitstroom" %in% colnames(result))
  expect_true("SUC_Type_uitstroom_studiejaar" %in% colnames(result))

  # Check dropout case
  expect_match(result$SUC_Type_uitstroom[1], "Dropout")

  # Check nominal diploma case
  expect_equal(result$SUC_Type_uitstroom[2], "Nominal")

  # Check faster than nominal
  expect_match(result$SUC_Type_uitstroom[3], "Nominal - 1")

  # Check slower than nominal
  expect_match(result$SUC_Type_uitstroom[4], "Nominal \\+ 1")
})


test_that("combine_enrollments_calculations detects switches", {
  # Test data with a switch scenario
  test_data <- data.frame(
    INS_Studentnummer = c(1, 1),
    OPL_code_historisch = c("A", "B"),
    INS_Uitval = c(TRUE, FALSE),
    INS_Aantal_inschrijvingen = c(1, 2),
    INS_Datum_tekening_diploma = c(NA, "2024-01-01"),
    INS_Aantal_inschrijvingen_tot_diploma = c(NA, 3),
    OPL_Nominale_studieduur = c(3, 3),
    INS_Studiejaar = c(1, 2),
    INS_Inschrijvingsjaar = c(2022, 2023),
    INS_Inschrijvingsjaar_EOI = c(2022, 2023),
    INS_Opleidingsfase_actueel_naam = c("B", "B"),
    INS_Inschrijvingsjaar_max = c(2022, 2023),
    OPL_Code_in_jaar = c("1", "2")
  )

  result <- combine_enrollments_calculations(test_data)

  expect_s3_class(result, "data.frame")

  # Check that switch columns were created
  expect_true("SUC_Type_uitstroom_incl_switch" %in% colnames(result))
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


test_that("combine_enrollments_final converts data types correctly", {
  test_data <- data.frame(
    INS_Inschrijvingsjaar = c("2022", "2023", "2024"),
    INS_Studentnummer = c(1, 2, 3),
    other_col = c("a", "b", "c")
  )

  result <- combine_enrollments_final(test_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)

  # Check that INS_Inschrijvingsjaar is integer
  expect_type(result$INS_Inschrijvingsjaar, "integer")
  expect_equal(result$INS_Inschrijvingsjaar, c(2022, 2023, 2024))
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
