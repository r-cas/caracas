test_that("init", {
  ver <- sympy_version()
  expect_true(ver >= "1.4")
})

test_that("smoke", {
  sympy <- get_sympy()
  ans <- sympy$solve("x**2-1", "x")
  expect_equal(length(ans), 2L)
})
