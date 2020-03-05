context("simplify")

test_that("simplify", {
  x <- symbol('x')
  z <- cos(x)^2 + sin(x)^2
  z
  simplify(z)
  tex(z)
})

test_that("expand", {
  x <- symbol('x')
  z <- (x-3)*(x+4)
  z
  expand(z)
})

test_that("expand_trig", {
  
})
