context("linalg")

test_that("Math", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  expect_equal(as.character(B), "Matrix([[a, 0, a], [0, a, 0], [0, a, 0]])")
  expect_equal(as.character(2*B), "Matrix([[2*a, 0, 2*a], [0, 2*a, 0], [0, 2*a, 0]])")
  expect_equal(as.character(B + B), "Matrix([[2*a, 0, 2*a], [0, 2*a, 0], [0, 2*a, 0]])")
  expect_equal(as.character(B - B), "Matrix([[0, 0, 0], [0, 0, 0], [0, 0, 0]])")
  expect_equal(as.character(B*B), "Matrix([[a^2, 0, a^2], [0, a^2, 0], [0, a^2, 0]])")
})

test_that("determinant", {
  skip_if_no_sympy()
  
  B <- as_symbol("[[x, 1], [2, x**2]]")
  
  expect_equal(as.character(determinant(B)), "x^3 - 2")
})

test_that("diag", {
  skip_if_no_sympy()
  
  B <- as_symbol("[[x, 1], [2, x**2]]")
  expect_equal(as.character(diag(B)), "Matrix([[x, x^2]])")
  
  A <- matrix(c("a", 4, 2, 1, "a", "a"), 2, 3)
  B <- as_symbol(A)
  expect_equal(as.character(diag(B)), "Matrix([[a, 1]])")
})



test_that("eigenvalues and eigenvectors", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  eval <- eigenval(B)
  eval_order <- order(unlist(lapply(eval, function(l) l$eigmult)))
  eval <- eval[eval_order]
  
  expect_equal(as.character(eval[[1L]]$eigval), "0")
  expect_equal(eval[[1L]]$eigmult, 1L)
  expect_equal(as.character(eval[[2L]]$eigval), "a")
  expect_equal(eval[[2L]]$eigmult, 2L)
  
  
  evec <- eigenvec(B)
  evec_order <- order(unlist(lapply(eval, function(l) l$eigmult)))
  evec <- evec[evec_order]
  
  expect_equal(as.character(evec[[1L]]$eigval), "0")
  expect_equal(evec[[1L]]$eigmult, 1L)
  expect_equal(as.character(evec[[1L]]$eigvec), "Matrix([[-1], [0], [1]])")
  expect_equal(as.character(evec[[2L]]$eigval), "a")
  expect_equal(evec[[2L]]$eigmult, 2L)
  expect_equal(as.character(evec[[2L]]$eigvec), "Matrix([[1], [0], [0]])")
})


test_that("as_character_matrix", {
  skip_if_no_sympy()
  
  x <- as_symbol(1)
  expect_equal(as_character_matrix(x), "1")
  
  
  b <- as_symbol(1:3)
  
  expect_equal(as_character_matrix(b), 
               structure(c("1", "2", "3"), .Dim = c(3L, 1L)))
  
  expect_equal(as_character_matrix(t(b)), 
               structure(c("1", "2", "3"), .Dim = c(1L, 3L)))
})


