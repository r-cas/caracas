context("calculus")

test_that("limit", {
  x <- symbol("x")
  lim(sin(x)/x, "x", 0)
  lim(1/x, "x", 0, dir = '+')
  lim(1/x, "x", 0, dir = '-')
})

test_that("derivative", {
  x <- symbol("x")
  y <- symbol("y")
  f <- 3*x^2 + x*y^2
  dd(f, "x")
  dd(f, x)
  dd(f, c("x", "y"))
  dd(f, c(x, y))
})
