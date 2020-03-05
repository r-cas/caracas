context("Ops and math")

test_that("Ops", {
  x <- symbol('x')
  
  res <- -x
  expect_s3_class(res, 'caracas_symbol')
  expect_equal(as.character(res), '-x')
  
  expect_equal(as.character(2*x), '2*x')
  expect_equal(as.character((1+1)*x), '2*x')
  expect_equal(as.character(3*x^2), '3*x^2')
  
  y <- symbol('y')
  expect_equal(as.character(3*x^2+y^(2+2)), '3*x^2 + y^4')
})

test_that("Math", {
  x <- symbol('x')
  
  res <- -cos(3*x)
  expect_s3_class(res, 'caracas_symbol')
  expect_equal(as.character(res), '-cos(3*x)')
  
  y <- symbol('y')
  expect_equal(as.character(3*sin(x^2)+sqrt(y^(2+2))), 
               "sqrt(y^4) + 3*sin(x^2)")
})
