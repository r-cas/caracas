context("simplify")

test_that("simplify", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  
  expect_equal(as.character(z), "sin(x)^2 + cos(x)^2")
  expect_equal(as.character(simplify(z)), "1")
  
  y <- symbol('y')
  z <- cos(x)*cos(y) - sin(x)*sin(y)
  expect_equal(as.character(z), "-sin(x)*sin(y) + cos(x)*cos(y)")
  expect_equal(as.character(simplify(z)), "cos(x + y)")
})

test_that("expand", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  z <- (x-3)*(x+4)
  expect_equal(as.character(z), "(x - 3)*(x + 4)")
  expect_equal(as.character(expand(z)), "x^2 + x - 12")
})

test_that("expand_trig", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  y <- symbol('y')
  z <- cos(x + y)
  
  expect_equal(as.character(z), "cos(x + y)")
  expect_equal(as.character(expand_trig(z)), "-sin(x)*sin(y) + cos(x)*cos(y)")
})

test_that("expand_log", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  y <- symbol('y')
  z <- log(x*y)
  
  expect_equal(as.character(z), "log(x*y)")
  expect_equal(as.character(expand_log(z)), "log(x) + log(y)")
})

