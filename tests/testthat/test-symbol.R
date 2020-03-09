context("symbol")

test_that("init", {
  expect_error(symbol(''))
  expect_error(symbol('2*x'))
  x <- symbol('x')
  expect_s3_class(x, 'caracas_symbol')
  
  expect_true(!is.null(x$pyobj))
  expect_s3_class(x$pyobj, 'python.builtin.object')
  expect_s3_class(x$pyobj, 'sympy.core.symbol.Symbol')
})

test_that("variable names", {
  expect_s3_class(symbol('x0'), 'caracas_symbol')
  expect_error(symbol('0x'))
  expect_s3_class(symbol('xa_0'), 'caracas_symbol')
  expect_s3_class(symbol('yY_x'), 'caracas_symbol')
})

test_that("eval_to_symbol", {
  expect_error(eval_to_symbol(''))
  expect_error(eval_to_symbol('2*w'))
  x <- symbol('x')
  expr <- eval_to_symbol('2*x')
  expect_s3_class(expr, 'caracas_symbol')
  expect_equal(as.character(expr), "2*x")
})
