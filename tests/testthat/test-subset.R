context("subset")

test_that("get", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  expect_equal(as.character(B[1:2, ]), as.character(as_symbol(A[1:2, ])))
  expect_equal(as.character(B[1, ]), as.character(as_symbol(A[1, ])))
  expect_equal(as.character(B[-1, ]), as.character(as_symbol(A[-1, ])))
  expect_equal(as.character(B[, 1:2]), as.character(as_symbol(A[, 1:2])))
  expect_equal(as.character(B[1, , drop = FALSE]), as.character(as_symbol(A[1, , drop = FALSE])))
  
  b <- B[1, ]
  a <- A[1, ]
  expect_equal(as.character(b[3]), as.character(as_symbol(a[3])))
  expect_equal(as.character(b[c(1, 3)]), as.character(as_symbol(a[c(1, 3)])))
  expect_equal(as.character(b[-c(1, 3)]), as.character(as_symbol(a[-c(1, 3)])))
  
  b <- B[, 1]
  a <- A[, 1]
  expect_equal(as.character(b[3]), as.character(as_symbol(a[3])))
  expect_equal(as.character(b[c(1, 3)]), as.character(as_symbol(a[c(1, 3)])))
  expect_equal(as.character(b[-c(1, 3)]), as.character(as_symbol(a[-c(1, 3)])))
})

test_that("set", {
  skip_if_no_sympy()
  
  A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
  B <- as_symbol(A)
  
  Amod <- A
  Amod[2, 2] <- "x"
  Bmod <- B
  Bmod[2, 2] <- "x"
  expect_equal(as.character(Bmod), as.character(as_symbol(Amod)))
  
  Amod <- A
  Amod[, 2] <- "x"
  Bmod <- B
  Bmod[, 2] <- "x"
  expect_equal(as.character(Bmod), as.character(as_symbol(Amod)))
  
  b <- B[1, ]
  b[1] <- "x"
  a <- A[1, ]
  a[1] <- "x"
  expect_equal(as.character(b), as.character(as_symbol(a)))
})
