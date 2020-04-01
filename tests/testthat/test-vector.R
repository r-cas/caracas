test_that("sum", {
  a <- c("x", "x^2")
  b <- as_symbol(a)
  expect_equal(as.character(sum(b)), "x^2 + x")
  
  p <- as_symbol(paste0("p", 1:3))
  y <- as_symbol(paste0("y", 1:3))
  a <- as_symbol("a")
  expect_equal(as.character(sum(a)), "a")
  l <- sum(y*log(p))
  expect_equal(as.character(l), "y1*log(p1) + y2*log(p2) + y3*log(p3)")
  
  A <- matrix(c("x", 0, 0, "2*x", "3*x**2", "-1"), 2, 3)
  B <- as_symbol(A)
  expect_equal(as.character(sum(B)), "3*x^2 + 3*x - 1")
})
