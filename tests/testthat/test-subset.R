context("subset")

test_that("get", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_sym(A)
  
  expect_equal(as.character(B[1:2, ]), as.character(as_sym(A[1:2, ])))
  expect_equal(as.character(B[1, ]), as.character(as_sym(A[1, ])))
  expect_equal(as.character(B[-1, ]), as.character(as_sym(A[-1, ])))
  expect_equal(as.character(B[, 1:2]), as.character(as_sym(A[, 1:2])))
  expect_equal(as.character(B[1, , drop = FALSE]), as.character(as_sym(A[1, , drop = FALSE])))
  
  b <- B[1, ]
  a <- A[1, ]
  expect_equal(as.character(b[3]), as.character(as_sym(a[3])))
  expect_equal(as.character(b[c(1, 3)]), as.character(as_sym(a[c(1, 3)])))
  expect_equal(as.character(b[-c(1, 3)]), as.character(as_sym(a[-c(1, 3)])))
  
  b <- B[, 1]
  a <- A[, 1]
  expect_equal(as.character(b[3]), as.character(as_sym(a[3])))
  expect_equal(as.character(b[c(1, 3)]), as.character(as_sym(a[c(1, 3)])))
  expect_equal(as.character(b[-c(1, 3)]), as.character(as_sym(a[-c(1, 3)])))
})

test_that("set", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_sym(A)
  
  Amod <- A
  Amod[2, 2] <- "x"
  Bmod <- B
  Bmod[2, 2] <- "x"
  expect_equal(as.character(Bmod), as.character(as_sym(Amod)))
  
  Amod <- A
  Amod[, 2] <- "x"
  Bmod <- B
  Bmod[, 2] <- "x"
  expect_equal(as.character(Bmod), as.character(as_sym(Amod)))
  
  b <- B[1, ]
  b[1] <- "x"
  a <- A[1, ]
  a[1] <- "x"
  expect_equal(as.character(b), as.character(as_sym(a)))
})

test_that("diag", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  x <- as_sym(A)
  diag(x) <- "b"
  expect_equal(as.character(x), 
               "Matrix([[b, 0, a], [0, b, 0], [0, a, b]])")
  
  
  Ve1 <- as_sym(matrix(
    c(1,   0,   0,   0,  0,
      0, "v2",  0,   0,  0,
      0,   0, "v2",  0,  0,
      0,   0,   0, "v2", 0,
      0,   0,   0,  0, "v2"), nrow=5, byrow = TRUE))
  Ve2 <- as_sym(diag(5))
  diag(Ve2) <- c("1", rep("v2", 4))
  expect_equal(as.character(Ve1), 
               as.character(Ve2))
  
})


test_that("diag subassignment", {
  skip_if_no_sympy()
  
  Aorg <- diag(5)
  B <- as_sym(Aorg)
  
  expect_equal(as.character(diag(B)), "Matrix([[1, 1, 1, 1, 1]])")
  
  diag(B)[2:4] <- 4
  expect_equal(as.character(diag(B)), "Matrix([[1, 4, 4, 4, 1]])")
  expect_equal(as.character(B), "Matrix([[1, 0, 0, 0, 0], [0, 4, 0, 0, 0], [0, 0, 4, 0, 0], [0, 0, 0, 4, 0], [0, 0, 0, 0, 1]])")
})
