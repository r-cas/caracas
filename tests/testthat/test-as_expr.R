test_that("as_expr", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  
  # sqrt()
  expect_equal(eval(as_expr(sqrt(3*x)), list(x = 1)), sqrt(3*1))
})
