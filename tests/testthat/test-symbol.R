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

