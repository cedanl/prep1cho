# Tests for I/O functions

test_that("read_file works with CSV format", {
  # Create temp CSV file
  temp_csv <- tempfile(fileext = ".csv")

  test_data <- data.frame(
    naam = c("Jan", "Piet"),
    leeftijd = c(25, 30),
    score = c(7.5, 8.2)
  )

  # Write European CSV format (semicolon, comma decimal)
  write.table(
    test_data,
    temp_csv,
    sep = ";",
    dec = ",",
    row.names = FALSE,
    quote = FALSE
  )

  # Read with read_file
  result <- read_file(temp_csv)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 3)
  expect_equal(result$naam, c("Jan", "Piet"))

  unlink(temp_csv)
})


test_that("read_file works with FST format", {
  skip_if_not_installed("fst")

  temp_fst <- tempfile(fileext = ".fst")

  test_data <- data.frame(
    x = 1:5,
    y = letters[1:5]
  )

  fst::write_fst(test_data, temp_fst)

  result <- read_file(temp_fst)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 5)
  expect_equal(result$x, 1:5)

  unlink(temp_fst)
})


test_that("read_file works with RDS format", {
  temp_rds <- tempfile(fileext = ".rds")

  test_data <- data.frame(
    a = 1:3,
    b = c("x", "y", "z")
  )

  saveRDS(test_data, temp_rds)

  result <- read_file(temp_rds)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(result$a, 1:3)

  unlink(temp_rds)
})


test_that("read_file errors on unsupported format", {
  temp_txt <- tempfile(fileext = ".txt")
  writeLines("test", temp_txt)

  expect_error(
    read_file(temp_txt),
    "Unsupported file extension"
  )

  unlink(temp_txt)
})


test_that("write_file works with CSV format", {
  temp_dir <- tempdir()
  temp_base <- file.path(temp_dir, "test_output")

  test_data <- data.frame(
    col1 = c(1.5, 2.3),
    col2 = c("a", "b")
  )

  write_file(test_data, temp_base, formats = "csv")

  expect_true(file.exists(paste0(temp_base, ".csv")))

  # Read back and verify
  result <- read.csv2(paste0(temp_base, ".csv"))
  expect_equal(nrow(result), 2)

  unlink(paste0(temp_base, ".csv"))
})


test_that("write_file works with FST format", {
  skip_if_not_installed("fst")

  temp_dir <- tempdir()
  temp_base <- file.path(temp_dir, "test_output_fst")

  test_data <- data.frame(
    x = 1:10,
    y = runif(10)
  )

  write_file(test_data, temp_base, formats = "fst")

  expect_true(file.exists(paste0(temp_base, ".fst")))

  # Read back and verify
  result <- fst::read_fst(paste0(temp_base, ".fst"))
  expect_equal(nrow(result), 10)

  unlink(paste0(temp_base, ".fst"))
})


test_that("write_file works with RDS format", {
  temp_dir <- tempdir()
  temp_base <- file.path(temp_dir, "test_output_rds")

  test_data <- data.frame(
    a = letters[1:5],
    b = 1:5
  )

  write_file(test_data, temp_base, formats = "rds")

  expect_true(file.exists(paste0(temp_base, ".rds")))

  # Read back and verify
  result <- readRDS(paste0(temp_base, ".rds"))
  expect_equal(nrow(result), 5)

  unlink(paste0(temp_base, ".rds"))
})


test_that("write_file works with multiple formats", {
  skip_if_not_installed("fst")

  temp_dir <- tempdir()
  temp_base <- file.path(temp_dir, "test_multi")

  test_data <- data.frame(x = 1:3)

  write_file(test_data, temp_base, formats = c("csv", "fst", "rds"))

  expect_true(file.exists(paste0(temp_base, ".csv")))
  expect_true(file.exists(paste0(temp_base, ".fst")))
  expect_true(file.exists(paste0(temp_base, ".rds")))

  unlink(paste0(temp_base, ".csv"))
  unlink(paste0(temp_base, ".fst"))
  unlink(paste0(temp_base, ".rds"))
})


test_that("write_file warns on unknown format", {
  temp_dir <- tempdir()
  temp_base <- file.path(temp_dir, "test_warn")

  test_data <- data.frame(x = 1)

  expect_warning(
    write_file(test_data, temp_base, formats = "xyz"),
    "Unknown format"
  )
})


test_that("get_data_path constructs correct paths", {
  path <- get_data_path("folder", "filename", "fst")

  expect_type(path, "character")
  expect_match(path, "data/folder/filename\\.fst")
})


test_that("get_data_path uses default extension", {
  path <- get_data_path("test", "myfile")

  expect_match(path, "\\.fst$")
})
