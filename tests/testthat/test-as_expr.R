test_that("as_expr", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  
  # sqrt()
  expect_equal(eval(as_expr(sqrt(3*x)), list(x = 1)), sqrt(3*1))
})

# https://github.com/r-cas/caracas/issues/47
test_that("complex", {
  skip_if_no_sympy()
  
  
  expect_equal(as.character(as_sym("1-I")), "1 - 1i")
  e1 <- as_expr(as_sym("1-I"))
  expect_equal(as.character(e1), "1-1i")
  
  
  expect_equal(as.character(as_sym("I-1")), "-1 + 1i")
  e2 <- as_expr(as_sym("I-1"))
  expect_equal(as.character(e2), "-1+1i")
})

