context("calculus")

test_that("derivative", {
  x <- symbol("x")
  y <- symbol("y")
  f <- 3*x^2 + x*y^2
  
  expect_equal(as.character(dd(f, "x")), "6*x + y^2")
  expect_equal(as.character(dd(f, x)), "6*x + y^2")
  
  expect_equal(as.character(dd(f, c("x", "y"))), "[6*x + y^2, 2*x*y]")
  expect_equal(as.character(dd(f, c(x, y))), "[6*x + y^2, 2*x*y]")
  
  expect_equal(as.character(dd2(f, c("x", "y"))), "[[6, 2*y], [2*y, 2*x]]")
  expect_equal(as.character(dd2(f, c(x, y))), "[[6, 2*y], [2*y, 2*x]]")
})

test_that("limit", {
  x <- symbol("x")
  
  expect_equal(as.character(lim(sin(x)/x, "x", 0)), "1")
  expect_equal(as.character(lim(sin(x)/x, x, 0)), "1")
  expect_equal(as_r(lim(sin(x)/x, "x", 0)), 1)
  expect_equal(as_r(lim(sin(x)/x, x, 0)), 1)
  
  expect_equal(as.character(lim(1/x, "x", 0, dir = '+')), "Inf")
  expect_equal(as.character(lim(1/x, x, 0, dir = '+')), "Inf")
  expect_equal(as_r(lim(1/x, "x", 0, dir = '+')), Inf)
  expect_equal(as_r(lim(1/x, x, 0, dir = '+')), Inf)
  
  expect_equal(as.character(lim(1/x, "x", 0, dir = '-')), "-Inf")
  expect_equal(as.character(lim(1/x, x, 0, dir = '-')), "-Inf")
  expect_equal(as_r(lim(1/x, "x", 0, dir = '-')), -Inf)
  expect_equal(as_r(lim(1/x, x, 0, dir = '-')), -Inf)
  
  
  a <- symbol("a")
  expect_equal(as.character(a*lim(sin(x)/x, "x", 0)), "a")
  expect_equal(as.character(a*lim(sin(x)/x, x, 0)), "a")
})

test_that("sumf", {
  x <- symbol("x")
  s <- sumf(1/x, "x", 1, 10)
  
  expect_equal(as.character(s), "7381/2520")
  expect_equal(as_r(s), sum(1/(1:10)))
               
  n <- symbol("n")
  s <- simplify(sumf(x, x, 1, n))
  expect_equal(as.character(s), "n*(n + 1)/2")
})

test_that("prodf", {
  x <- symbol("x")
  p <- prodf(1/x, "x", 1, 10)
  
  expect_equal(as.character(p), "1/3628800")
  expect_equal(as_r(p), prod(1/(1:10)))
})

test_that("intf", {
  x <- symbol("x")
  
  # Limits
  expect_equal(as.character(intf(1/x, "x", 1, 10)), "log(10)")
  expect_equal(as.character(intf(1/x, x, 1, 10)), "log(10)")
  
  i1 <- intf(1/x, "x", 1, 10, doit = FALSE)
  expect_equal(as.character(i1), "Integral(1/x, (x, 1, 10))")
  expect_equal(tex(i1), "\\int\\limits_{1}^{10} \\frac{1}{x}\\, dx")
  expect_equal(as.character(doit(i1)), "log(10)")
  
  i1 <- intf(1/x, x, 1, 10, doit = FALSE)
  expect_equal(as.character(i1), "Integral(1/x, (x, 1, 10))")
  expect_equal(tex(i1), "\\int\\limits_{1}^{10} \\frac{1}{x}\\, dx")
  expect_equal(as.character(doit(i1)), "log(10)")

  
  ## No limits
  z <- symbol('z')
  expect_equal(as.character(intf(1/z, "z")), "log(z)")
  expect_equal(as.character(intf(1/z, z)), "log(z)")
  
  i1 <- intf(1/x, "x", doit = FALSE)
  expect_equal(as.character(i1), "Integral(1/x, x)")
  expect_equal(tex(i1), "\\int \\frac{1}{x}\\, dx")
  expect_equal(as.character(doit(i1)), "log(x)")
  
  i1 <- intf(1/x, x, doit = FALSE)
  expect_equal(as.character(i1), "Integral(1/x, x)")
  expect_equal(tex(i1), "\\int \\frac{1}{x}\\, dx")
  expect_equal(as.character(doit(i1)), "log(x)")
})

