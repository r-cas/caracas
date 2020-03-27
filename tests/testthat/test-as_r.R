test_that("as_r", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  
  # sqrt()
  expect_equal(eval(as_r(sqrt(3*x)), list(x = 1)), sqrt(3*1))
})
