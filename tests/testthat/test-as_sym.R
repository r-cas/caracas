context("as_sym()")

test_that("var detect", {
  skip_if_no_sympy()
  
  A <- matrix(c("w", 1, 1, "2*w", "w+2", 1), 3, 2)
  expect_error(as_sym(A, declare_symbols = FALSE))
  expect_s3_class(as_sym(A, declare_symbols = TRUE), "caracas_symbol")
})

test_that("smoke", {
  skip_if_no_sympy()
  
  A <- matrix(c("x", 1, 1, "2*x", "x+2", 1), 3, 2)
  B <- as_sym(A)
  expect_equal(dim(B), c(3L, 2L))
  expect_equal(nrow(B), 3L)
  expect_equal(ncol(B), 2L)
  
  expect_equal(as.character(log(B)), "Matrix([[log(x), log(2*x)], [0, log(x + 2)], [0, 0]])")
  
  expect_equal(eval(as_expr(log(B)), list(x = 42)), 
               log(matrix(c(42, 1, 1, 2*42, 42+2, 1), 3, 2)))
  
  expect_equal(as.character(2*B - sqrt(B)), "Matrix([[-sqrt(x) + 2*x, -sqrt(2)*sqrt(x) + 4*x], [1, 2*x - sqrt(x + 2) + 4], [1, 1]])")
  expect_equal(as_expr(2*B - B - B), matrix(0, 3, 2))
  
  expect_equal(as.character(t(B)), "Matrix([[x, 1, 1], [2*x, x + 2, 1]])")
  expect_equal(as.character(t(t(B))), as.character(B))
})

test_that("matrix", {
  skip_if_no_sympy()
  
  D <- as_sym("[[1, 4, 5], [-5, 8, 9]]")
  expect_equal(as.character(D), "Matrix([[1, 4, 5], [-5, 8, 9]])")
  expect_equal(as.character(t(D)), "Matrix([[1, -5], [4, 8], [5, 9]])")
  
  D <- as_sym("[[x, 1], [Inf, x^2]]")
  expect_equal(as.character(D), "Matrix([[x, 1], [Inf, x^2]])")
  
  expect_equal(as.character(eval_to_symbol(as.character(D))), as.character(D))
})

test_that("multiplication", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  A <- matrix(c("x", 1, 1, "2*x", "x+2", 1), 3, 2)
  B <- as_sym(A)

  expect_equal(as.character(B+1), 
               as.character(as_sym(matrix(paste0(A, "+1"), 3, 2))))
  expect_equal(as.character(B-2), 
               as.character(as_sym(matrix(paste0(A, "-2"), 3, 2))))
  
  expect_equal(as.character(3*B), "Matrix([[3*x, 6*x], [3, 3*x + 6], [3, 3]])")
  expect_equal(as.character(2*B - B - B), "Matrix([[0, 0], [0, 0], [0, 0]])")
  
  expect_equal(dim(B), c(3L, 2L))
  expect_equal(dim(t(B) %*% B), c(2L, 2L))
  expect_equal(dim(B %*% t(B)), c(3L, 3L))
  
  D <- unname(eval(as_expr(B), list(x = 42)))
  expect_equal(t(D) %*% D, eval(as_expr(t(B) %*% B), list(x = 42)))
  expect_equal(D %*% t(D), eval(as_expr(B %*% t(B)), list(x = 42)))
})



test_that("fraction", {
  skip_if_no_sympy()
  
  #https://docs.sympy.org/latest/gotchas.html#python-numbers-vs-sympy-numbers
  x <- as_sym("120000/7")
  y <- as_sym("20")
  
  expect_equal(as.character(x + y), "120140/7")
})


test_that("eigval", {
  skip_if_no_sympy()

  str <- "a - sqrt(3)*sqrt(b*c)"
  eig <- as_sym(str)
  expect_equal(as.character(eig), str)
})



test_that("expression", {
  skip_if_no_sympy()
  
  x <- expression(a - sqrt(3)*sqrt(b*c))
  y <- as_sym(x)
  expect_equal(as.character(y), "a - sqrt(3)*sqrt(b*c)")
})



test_that("matrix", {
  skip_if_no_sympy()
  
  A <- as_sym("[[9, 3*I], [-3*I, 5]]")
  expect_equal(as_character_matrix(A), 
               structure(c("9", "-3*1i", "3*1i", "5"), .Dim = c(2L, 2L)))
  expect_equal(as_expr(A), 
               structure(c(9+0i, 0-3i, 0+3i, 5+0i), .Dim = c(2L, 2L)))
})

