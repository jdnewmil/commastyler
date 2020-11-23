# test_roundtrip.R
library(styler)
context("roundtrip works")

test_that("can_verify_roundtrip works", {
  expect_true( commastyler:::can_verify_roundtrip( commastyler_style( scope = "line_breaks" ) ) )
  expect_true( commastyler:::can_verify_roundtrip( commastyler_style( scope = "spaces" ) ) )
  expect_true( commastyler:::can_verify_roundtrip( commastyler_style( scope = "indention" ) ) )
  expect_false( commastyler:::can_verify_roundtrip( commastyler_style( scope = "tokens" ) ) )
})

test_that("correct styling does not give an error", {
  expect_error( commastyler:::verify_roundtrip( "1+1", "1 + 1" ), NA )
})

test_that("corrupt styling does give an error", {
  expect_error( commastyler:::verify_roundtrip("1-1", "1 + 1"), "bug")
})
