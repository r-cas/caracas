context("assumptions")

# test_that("ls_sym", {
#   skip_if_no_sympy()
#   
#   n <- length(ls_sym())
#   zzzzzzzzzzzzzzzzz <- symbol("zzzzzzzzzzzzzzzzz")
#   expect_length(ls_sym(), n+1L)
# })

test_that("ask", {
  skip_if_no_sympy()
  
  y <- symbol("y")
  expect_equal(ask(y, "positive"), NA)
  
  B <- as_sym("[[y + 1, 1], [1, 1]]")
  expect_equal(ask(B, "hermitian"), NA)
  
  
  w <- symbol("w", positive = TRUE)
  D <- as_sym("[[w + 1, 1], [1, 1]]", declare_symbols = FALSE)
  expect_true(ask(w, "positive"))
  expect_true(ask(D, "hermitian"))
})



test_that("solve 1", {
  x <- symbol("x")
  sol <- solve_sys(x^2 + 1, x)
  expect_equal(length(sol), 2L)
})

test_that("solve 2", {
  x <- symbol("x", real = TRUE)
  sol <- solve_sys(x^2 + 1, x)
  expect_equal(length(sol), 0L)
})

test_that("solve 3", {
  x <- symbol("x", positive = TRUE)
  ask(x, 'positive')
  sol <- solve_sys(x^2 - 1, x)
  expect_equal(length(sol), 1L)
})
