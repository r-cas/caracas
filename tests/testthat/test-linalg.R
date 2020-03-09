context("linalg")

test_that("eigenvalues and eigenvectors", {
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  eval <- eigenvals(B)
  eval_order <- order(unlist(lapply(eval, function(l) l$eigmult)))
  eval <- eval[eval_order]
  
  expect_equal(as.character(eval[[1L]]$eigval), "0")
  expect_equal(eval[[1L]]$eigmult, 1L)
  expect_equal(as.character(eval[[2L]]$eigval), "a")
  expect_equal(eval[[2L]]$eigmult, 2L)
  
  
  evec <- eigenvects(B)
  evec_order <- order(unlist(lapply(eval, function(l) l$eigmult)))
  evec <- evec[evec_order]
  
  expect_equal(as.character(evec[[1L]]$eigval), "0")
  expect_equal(evec[[1L]]$eigmult, 1L)
  expect_equal(as.character(evec[[1L]]$eigvec), "Matrix([[-1], [0], [1]])")
  expect_equal(as.character(evec[[2L]]$eigval), "a")
  expect_equal(evec[[2L]]$eigmult, 2L)
  expect_equal(as.character(evec[[2L]]$eigvec), "Matrix([[1], [0], [0]])")
})
