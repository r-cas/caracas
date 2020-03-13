context("as_symbol()")

test_that("var detect", {
  A <- matrix(c("zzzzzzz", 1, 1, "2*zzzzzzz", "zzzzzzz+2", 1), 3, 2)
  expect_error(as_symbol(A, declare_variables = FALSE))
  expect_s3_class(as_symbol(A, declare_variables = TRUE), "caracas_symbol")
})

test_that("smoke", {
  A <- matrix(c("x", 1, 1, "2*x", "x+2", 1), 3, 2)
  B <- as_symbol(A)
  expect_equal(dim(B), c(3L, 2L))
  expect_equal(nrow(B), 3L)
  expect_equal(ncol(B), 2L)
  
  expect_equal(as.character(log(B)), "Matrix([[log(x), log(2*x)], [0, log(x + 2)], [0, 0]])")
  
  expect_equal(eval(as_r(log(B)), list(x = 42)), 
               log(matrix(c(42, 1, 1, 2*42, 42+2, 1), 3, 2)))
  
  expect_equal(as.character(2*B - sqrt(B)), "Matrix([[-sqrt(x) + 2*x, -sqrt(2)*sqrt(x) + 4*x], [1, 2*x - sqrt(x + 2) + 4], [1, 1]])")
  expect_equal(as_r(2*B - B - B), matrix(0, 3, 2))
  
  expect_equal(as.character(t(B)), "Matrix([[x, 1, 1], [2*x, x + 2, 1]])")
  expect_equal(as.character(t(t(B))), as.character(B))
})

test_that("multiplication", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  A <- matrix(c("x", 1, 1, "2*x", "x+2", 1), 3, 2)
  B <- as_symbol(A)

  expect_equal(as.character(B+1), 
               as.character(as_symbol(matrix(paste0(A, "+1"), 3, 2))))
  expect_equal(as.character(B-2), 
               as.character(as_symbol(matrix(paste0(A, "-2"), 3, 2))))
  
  expect_equal(as.character(3*B), "Matrix([[3*x, 6*x], [3, 3*x + 6], [3, 3]])")
  expect_equal(as.character(2*B - B - B), "Matrix([[0, 0], [0, 0], [0, 0]])")
  
  expect_error(B*B, regexp = paste0("Only \\+, \\- and %\\*% are valid ", 
                                    "for matrix-matrix/matrix-vector operations"))
  
  expect_equal(dim(B), c(3L, 2L))
  expect_equal(dim(t(B) %*% B), c(2L, 2L))
  expect_equal(dim(B %*% t(B)), c(3L, 3L))
  
  D <- unname(eval(as_r(B), list(x = 42)))
  expect_equal(t(D) %*% D, eval(as_r(t(B) %*% B), list(x = 42)))
  expect_equal(D %*% t(D), eval(as_r(B %*% t(B)), list(x = 42)))
})

