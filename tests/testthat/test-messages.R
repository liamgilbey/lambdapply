# ==============================================================================
# Testing capabilities of messages

test_that("Error messaging works", {
  test_message <- paste(clisymbols::symbol$cross, "Test error message", sep = " ")
  expect_equal(test_message, .error_message("Test error message"))
})

test_that("Warning messaging works", {
  test_message <- paste(clisymbols::symbol$circle_filled, "Test warning message", sep = " ")
  expect_equal(test_message, .warning_message("Test warning message"))
})
