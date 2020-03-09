context("linalg")

test_that("eigenvalues and eigenvectors", {
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  eval <- eigenvals(B)
  expect_equal(as.character(eval[[1L]]$eigval), "a")
  expect_equal(eval[[1L]]$eigmult, 2L)
  expect_equal(as.character(eval[[2L]]$eigval), "0")
  expect_equal(eval[[2L]]$eigmult, 1L)
  
  
  evec <- eigenvects(B)
  expect_equal(as.character(evec[[1L]]$eigval), "0")
  expect_equal(evec[[1L]]$eigmult, 1L)
  expect_equal(as.character(evec[[1L]]$eigvec), "Matrix([[-1], [0], [1]])")
  expect_equal(as.character(evec[[2L]]$eigval), "a")
  expect_equal(evec[[2L]]$eigmult, 2L)
  expect_equal(as.character(evec[[2L]]$eigvec), "Matrix([[1], [0], [0]])")
})
