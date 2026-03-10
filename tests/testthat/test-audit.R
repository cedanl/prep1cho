# Tests for audit functions

test_that("audit_enrollments returns data frame", {
  test_data <- data.frame(
    persoonsgebonden_nummer = c(1, 2, 3),
    inschrijvingsjaar = c(2024, 2024, 2024),
    geslacht = c("M", "V", "M")
  )

  # Mock the documentation file
  doc_path <- system.file("metadata/assertions/Documentatie_ev.csv", package = "prep1cho")

  if (doc_path != "") {
    result <- suppressMessages(audit_enrollments(test_data))
    expect_s3_class(result, "data.frame")
    expect_equal(nrow(result), 3)
  } else {
    skip("Documentation file not available")
  }
})


test_that("audit_enrollments warns about empty columns", {
  test_data <- data.frame(
    persoonsgebonden_nummer = c(1, 2, 3),
    empty_col = c(NA, NA, NA)
  )

  doc_path <- system.file("metadata/assertions/Documentatie_ev.csv", package = "prep1cho")

  if (doc_path != "") {
    expect_warning(
      suppressMessages(audit_enrollments(test_data)),
      "empty"
    )
  } else {
    skip("Documentation file not available")
  }
})
