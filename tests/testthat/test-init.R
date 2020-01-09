skip_if_no_sympy <- function() {
  if (!have_sympy()) {
    skip("sympy not available for testing")
  }
}

test_that("init", {
  skip_if_no_sympy()
  
  ver <- sympy_version()
  expect_true(ver >= "1.4")
})

test_that("smoke", {
  skip_if_no_sympy()
  
  sympy <- get_sympy()
  ans <- sympy$solve("x**2-1", "x")
  expect_equal(length(ans), 2L)
})
