# Tests for utility functions

test_that("academic_year calculates correctly for September boundary", {
  expect_equal(academic_year(as.Date("2024-09-01")), 2024)
  expect_equal(academic_year(as.Date("2024-08-31")), 2023)
  expect_equal(academic_year(as.Date("2024-12-31")), 2024)
  expect_equal(academic_year(as.Date("2024-01-01")), 2023)
})


test_that("academic_year handles NA values", {
  expect_true(is.na(academic_year(as.Date(NA))))
  expect_true(is.na(academic_year(c(NA, NA))))
})


test_that("academic_year handles character dates", {
  expect_equal(academic_year("2024-09-01"), 2024)
  expect_equal(academic_year("2024-08-31"), 2023)
})


test_that("minnum handles numeric vectors correctly", {
  expect_equal(minnum(c(1, 2, 3)), 1)
  expect_equal(minnum(c(-5, -2, 3)), -5)
  expect_equal(minnum(c(1.5, 2.3, 0.1)), 0)  # Uses round_values which floors negatives
})


test_that("minnum handles NA values", {
  expect_true(is.na(minnum(c(NA, NA))))
  expect_equal(minnum(c(1, NA, 3)), 1)
})


test_that("minnum handles non-numeric input", {
  expect_true(is.na(minnum(c("a", "b"))))
  expect_true(is.na(minnum(NULL)))
})


test_that("maxnum handles numeric vectors correctly", {
  expect_equal(maxnum(c(1, 2, 3)), 3)
  expect_equal(maxnum(c(-5, -2, 3)), 3)
})


test_that("maxnum handles NA values", {
  expect_true(is.na(maxnum(c(NA, NA))))
  expect_equal(maxnum(c(1, NA, 3)), 3)
})


test_that("round_values rounds correctly", {
  expect_equal(round_values(1.5), 2)
  expect_equal(round_values(-1.5), -2)
  expect_equal(round_values(0.1), 1)
  expect_equal(round_values(-0.1), -1)
})


test_that("first_class returns first class", {
  df <- data.frame(x = 1)
  expect_equal(first_class(df), "data.frame")

  vec <- c(1, 2, 3)
  expect_equal(first_class(vec), "numeric")
})
