context("Ops and math")

test_that("Ops", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  
  res <- -x
  expect_s3_class(res, 'caracas_symbol')
  expect_equal(as.character(res), '-x')
  
  expect_equal(as.character(2*x), '2*x')
  expect_equal(as.character((1+1)*x), '2*x')
  expect_equal(as.character(3*x^2), '3*x^2')
  
  y <- symbol('y')
  expect_equal(as.character(3*x^2+y^(2+2)), '3*x^2 + y^4')
  
  
  # #68: https://github.com/r-cas/caracas/issues/68
  def_sym(beta)
  x <- as_sym(paste0("x_", seq_len(2)))
  expect_equal(as.character(x), "Matrix([[x_1], [x_2]])")
  expect_equal(as.character(beta^x[1]), "beta^x_1")
  expect_equal(as.character(beta^x[2]), "beta^x_2")
  expect_equal(as.character(beta^x), "Matrix([[beta^x_1], [beta^x_2]])")
})

test_that("Math", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  
  res <- -cos(3*x)
  expect_s3_class(res, 'caracas_symbol')
  expect_equal(as.character(res), '-cos(3*x)')
  
  y <- symbol('y')
  expect_equal(as.character(3*sin(x^2)+sqrt(y^(2+2))), 
               "sqrt(y^4) + 3*sin(x^2)")
})

test_that("matrix_ele_power", {
  skip_if_no_sympy()
  
  y <- symbol('y')
  v <- as_sym(paste0("v_", seq_len(2)))
  
  matrix_ele_power(v, y)
  matrix_ele_power(y, v)
  
  
  # #68: https://github.com/r-cas/caracas/issues/68
  def_sym(beta)
  x <- as_sym(paste0("x_", seq_len(2)))
  expect_equal(as.character(x), "Matrix([[x_1], [x_2]])")
  expect_equal(as.character(beta^x[1]), "beta^x_1")
  expect_equal(as.character(beta^x[2]), "beta^x_2")
  expect_equal(as.character(beta^x), "Matrix([[beta^x_1], [beta^x_2]])")
})

