context("calculus")

test_that("derivative", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  y <- symbol("y")
  f <- 3*x^2 + x*y^2
  
  expect_equal(as.character(dd(f, "x")), "6*x + y^2")
  expect_equal(as.character(dd(f, x)), "6*x + y^2")
  
  expect_equal(as.character(dd(f, c("x", "y"))), "[6*x + y^2, 2*x*y]")
  expect_equal(as.character(dd(f, c(x, y))), "[6*x + y^2, 2*x*y]")
  
  expect_equal(as.character(dd2(f, c("x", "y"))), "[[6, 2*y], [2*y, 2*x]]")
  expect_equal(as.character(dd2(f, c(x, y))), "[[6, 2*y], [2*y, 2*x]]")
  
  p <- as_symbol(paste0("p", 1:3))
  y <- as_symbol(paste0("y", 1:3))
  a <- as_symbol("a")
  l <- sum(y*log(p))
  L <- -l + a*(sum(p) - 1)
  gL <- dd(L, c(p, a)) 
  expect_equal(as.character(gL), "[a - y1/p1, a - y2/p2, a - y3/p3, p1 + p2 + p3 - 1]")
})

test_that("limit", {
  skip_if_no_sympy()
  
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
  skip_if_no_sympy()
  
  x <- symbol("x")
  s <- sumf(1/x, "x", 1, 10)
  
  expect_equal(as.character(s), "7381/2520")
  expect_equal(as_r(s), sum(1/(1:10)))
               
  n <- symbol("n")
  s <- simplify(sumf(x, x, 1, n))
  expect_equal(as.character(s), "n*(n + 1)/2")
})

test_that("prodf", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  p <- prodf(1/x, "x", 1, 10)
  
  expect_equal(as.character(p), "1/3628800")
  expect_equal(as_r(p), prod(1/(1:10)))
})

test_that("intf", {
  skip_if_no_sympy()
  
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

