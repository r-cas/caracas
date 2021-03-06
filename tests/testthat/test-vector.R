test_that("sum", {
  skip_if_no_sympy()
  
  a <- c("x", "x^2")
  b <- as_sym(a)
  expect_equal(as.character(sum(b)), "x^2 + x")
  
  p <- as_sym(paste0("p", 1:3))
  y <- as_sym(paste0("y", 1:3))
  a <- as_sym("a")
  expect_equal(as.character(sum(a)), "a")
  l <- sum(y*log(p))
  expect_equal(as.character(l), "y1*log(p1) + y2*log(p2) + y3*log(p3)")
  
  A <- matrix(c("x", 0, 0, "2*x", "3*x**2", "-1"), 2, 3)
  B <- as_sym(A)
  expect_equal(as.character(sum(B)), "3*x^2 + 3*x - 1")
})

test_that("cbind/rbind", {
  skip_if_no_sympy()
  
  x <- symbol("x")
  
  expect_equal(as.character(cbind(x, x, x)), "Matrix([[x, x, x]])")
  expect_equal(as_character_matrix(cbind(x, x, x)), 
               structure(c("x", "x", "x"), .Dim = c(1L, 3L)))
  
  expect_equal(as.character(rbind(x, x, x)), "Matrix([[x], [x], [x]])")
  expect_equal(as_character_matrix(rbind(x, x, x)), 
               structure(c("x", "x", "x"), .Dim = c(3L, 1L)))
  
  
  
  a <- c("x", "x^2")
  b <- as_sym(a)
  
  expect_equal(unname(cbind(a, a, a)), 
               as_character_matrix(cbind(b, b, b)))
  expect_equal(as.character(cbind(b, 2*b, b-1)), 
               "Matrix([[x, 2*x, x - 1], [x^2, 2*x^2, x^2 - 1]])")
  expect_equal(as_character_matrix(cbind(b, 2*b, b-1)), 
               structure(c("x", "x^2", "2*x", "2*x^2", "x - 1", "x^2 - 1"), .Dim = 2:3))
  
  expect_equal(unname(rbind(a, a, a)), 
               as_character_matrix(rbind(t(b), t(b), t(b))))
  expect_equal(as.character(rbind(b, 2*b, b-1)), 
               "Matrix([[x], [x^2], [2*x], [2*x^2], [x - 1], [x^2 - 1]])")
  expect_equal(as.character(rbind(t(b), t(2*b), t(b-1))), 
               "Matrix([[x, x^2], [2*x, 2*x^2], [x - 1, x^2 - 1]])")
  expect_equal(as_character_matrix(rbind(t(b), t(2*b), t(b-1))), 
               structure(c("x", "2*x", "x - 1", "x^2", "2*x^2", "x^2 - 1"), .Dim = 3:2))
})
