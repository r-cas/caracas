context("symbol")

test_that("init", {
  skip_if_no_sympy()
  
  expect_error(symbol(''))
  expect_error(symbol('2*x'))
  x <- symbol('x')
  expect_s3_class(x, 'caracas_symbol')
  
  expect_true(!is.null(x$pyobj))
  expect_s3_class(x$pyobj, 'python.builtin.object')
  expect_s3_class(x$pyobj, 'sympy.core.symbol.Symbol')
})

test_that("variable names", {
  skip_if_no_sympy()
  
  expect_s3_class(symbol('x0'), 'caracas_symbol')
  expect_error(symbol('0x'))
  expect_s3_class(symbol('xa_0'), 'caracas_symbol')
  expect_s3_class(symbol('yY_x'), 'caracas_symbol')
})

test_that("eval_to_symbol", {
  skip_if_no_sympy()
  
  expect_error(eval_to_symbol(''))
  expect_error(eval_to_symbol('2*q'))
  x <- symbol('x')
  expr <- eval_to_symbol('2*x')
  expect_s3_class(expr, 'caracas_symbol')
  expect_equal(as.character(expr), "2*x")
  
  expect_equal(as.character(eval_to_symbol("1/3")), "1/3")
  x <- symbol('x')
  expect_equal(as.character(eval_to_symbol("x*1/3")), "x/3")
  
  x1 <- symbol('x1')
  expect_equal(as.character(eval_to_symbol("x1/3")), "x1/3")
})

test_that("subs", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  e <- 2*x^2
  e2 <- subs(e, "x", "y^2")
  expect_equal(as.character(e2), "2*y^4")
})

