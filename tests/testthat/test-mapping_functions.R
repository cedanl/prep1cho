# Tests for mapping functions (utils.R)

test_that("mapping_translate works with basic mapping", {
  test_data <- data.frame(
    id = 1:3,
    code = c("M", "V", "O")
  )

  mapping <- data.frame(
    from = c("M", "V", "O"),
    to = c("Man", "Vrouw", "Onbekend")
  )

  result <- mapping_translate(test_data, "code", "label", mapping_table_input = mapping)

  expect_equal(result$label, c("Man", "Vrouw", "Onbekend"))
  expect_true("code" %in% colnames(result))  # Original column kept by default
  expect_equal(nrow(result), 3)
})


test_that("mapping_translate handles missing values correctly", {
  test_data <- data.frame(
    code = c("M", "V", "X")  # X not in mapping
  )

  mapping <- data.frame(
    from = c("M", "V"),
    to = c("Man", "Vrouw")
  )

  result <- mapping_translate(test_data, "code", "label", mapping_table_input = mapping)

  expect_equal(result$label, c("Man", "Vrouw", NA))
})


test_that("mapping_translate can remove original column", {
  test_data <- data.frame(code = c("M", "V"))
  mapping <- data.frame(from = c("M", "V"), to = c("Man", "Vrouw"))

  result <- mapping_translate(test_data, "code", "label",
                              mapping_table_input = mapping,
                              KeepOriginal = FALSE)

  expect_false("code" %in% colnames(result))
  expect_true("label" %in% colnames(result))
})


test_that("mapping_translate errors when mapping table missing columns", {
  test_data <- data.frame(code = c("M"))
  bad_mapping <- data.frame(wrong = c("M"), columns = c("Man"))

  expect_error(
    mapping_translate(test_data, "code", "label", mapping_table_input = bad_mapping),
    "from.*to"
  )
})


test_that("mapping_translate errors when new column already exists", {
  test_data <- data.frame(code = c("M"), label = c("existing"))
  mapping <- data.frame(from = c("M"), to = c("Man"))

  expect_error(
    mapping_translate(test_data, "code", "label", mapping_table_input = mapping),
    "already exists"
  )
})


test_that("mapping_category works with numeric data", {
  test_data <- data.frame(
    age = c(17, 18, 20, 25, 30)
  )

  mapping <- data.frame(
    lower = c(0, 18, 25),
    upper = c(18, 25, 99),
    category = c("Onder 18", "18-24", "25+")
  )

  result <- mapping_category(test_data, "age", "age_cat", mapping_table_input = mapping)

  expect_equal(result$age_cat, c("Onder 18", "18-24", "18-24", "25+", "25+"))
})


test_that("mapping_category handles edge values correctly", {
  test_data <- data.frame(age = c(0, 18, 25))

  mapping <- data.frame(
    lower = c(0, 18, 25),
    upper = c(18, 25, 99),
    category = c("0-17", "18-24", "25+")
  )

  result <- mapping_category(test_data, "age", "age_cat", mapping_table_input = mapping)

  # cut with right=FALSE means [lower, upper), so 0 is in first category, 18 in second, 25 in third
  expect_equal(result$age_cat, c("0-17", "18-24", "25+"))
})


test_that("mapping_category errors when mapping table missing columns", {
  test_data <- data.frame(age = c(20))
  bad_mapping <- data.frame(min = c(0), max = c(100), cat = c("All"))

  expect_error(
    mapping_category(test_data, "age", "age_cat", mapping_table_input = bad_mapping),
    "lower.*upper.*category"
  )
})


test_that("translate_colnames_documentation renames columns correctly", {
  test_data <- data.frame(
    export_col_1 = c(1, 2),
    export_col_2 = c(3, 4)
  )

  doc_table <- data.frame(
    Veldnaam = c("internal_col_1", "internal_col_2"),
    Veldnaam_export = c("export_col_1", "export_col_2")
  )

  result <- translate_colnames_documentation(test_data, doc_table, drop_na = FALSE)

  expect_equal(colnames(result), c("internal_col_1", "internal_col_2"))
})


test_that("translate_colnames_documentation drops unmapped columns", {
  test_data <- data.frame(
    export_col_1 = c(1, 2),
    unknown_col = c(3, 4)
  )

  doc_table <- data.frame(
    Veldnaam = c("internal_col_1"),
    Veldnaam_export = c("export_col_1")
  )

  result <- translate_colnames_documentation(test_data, doc_table, drop_na = TRUE)

  expect_equal(ncol(result), 1)
  expect_equal(colnames(result), "internal_col_1")
})
