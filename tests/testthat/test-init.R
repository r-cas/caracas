test_that("init", {
  ver <- sympy_version()
  expect_true(ver >= "1.4")
})

