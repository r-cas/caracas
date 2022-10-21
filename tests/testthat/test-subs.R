test_that("subs simple", {
  skip_if_no_sympy()
  
  x <- symbol('x')
  e <- 2*x^2
  expect_equal(as.character(e), "2*x^2")
  
  expect_equal(as.character(subs(e, "x", "2")), "8")
  expect_equal(as.character(subs(e, x, 2)), "8")
  expect_equal(as.character(subs(e, list(x = 2))), "8")
})

test_that("subs matrix", {
  skip_if_no_sympy()
  
   A <- matrix_sym(2, 2, "a")
   B <- matrix_sym(2, 2, "b")
   e <- A %*% A
   
   expect_equal(as.character(e), 
                "Matrix([[a11^2 + a12*a21, a11*a12 + a12*a22], [a11*a21 + a21*a22, a12*a21 + a22^2]])")
   
   
   expect_equal(as.character(subs(e, A, B)), 
                "Matrix([[b11^2 + b12*b21, b11*b12 + b12*b22], [b11*b21 + b21*b22, b12*b21 + b22^2]])")
})


test_that("subs list", {
  skip_if_no_sympy()
  
  def_sym(xx, yy, zz)
  
  sym <- cos(xx) + yy^2 + zz 
  nms <- list(xx = xx + yy, zz = yy^2 - xx) 
  s <- subs(sym, nms)
  expect_equal(as.character(s), "-xx + 2*yy^2 + cos(xx + yy)")
})


test_that("subs vectors", {
  skip_if_no_sympy()
  
  
  def_sym(xx, yy, zz)
  
  sym <- cos(xx) + yy^2 + zz 
  answer <- 
  
  ## Case atomics or vectors
  nms <- xx
  vls <- xx + cos(yy^2)
  expect_equal(as.character(subs(sym, nms, vls)), "yy^2 + zz + cos(xx + cos(yy^2))")
  
  ###
  
  answer <- "-xx + yy^2 + cos(yy^2) + cos(xx + yy)"
  
  nms <- c(xx, zz)
  vls <- c(xx + yy, cos(yy^2) - xx)
  expect_equal(as.character(subs(sym, nms, vls)), answer)
  
  nms <- c("xx", "zz")
  vls <- c(xx + yy, cos(yy^2) - xx)
  expect_equal(as.character(subs(sym, nms, vls)), answer)
  
  nms <- c(xx, zz)
  vls <- c("xx + yy", "cos(yy^2) - xx")
  expect_equal(as.character(subs(sym, nms, vls)), answer)
  
})



test_that("subs vectors2", {
  skip_if_no_sympy()
  
  
  p <- vector_sym(3, "p")
  y <- vector_sym(3, "y")
  sym <- sum(y * log(p))
  
  nms <- p
  vls <- 2*p
  
  s <- subs(sym, nms, vls)
  expect_equal(as.character(s), "y1*log(2*p1) + y2*log(2*p2) + y3*log(2*p3)")
  
  
  vls[1] = "2*R"
  vls[2] = "7"
  s <- subs(sym, nms, vls)
  expect_equal(as.character(s), "y1*log(2*R) + y2*log(7) + y3*log(2*p3)")
  
  
  nms <- list("p1" = "A", "p2" = y[1], "p3" = 2)
  s <- subs(sym, nms)
  expect_equal(as.character(s), "y1*log(A) + y2*log(y1) + y3*log(2)")
  
  
  
  sym <- matrix_sym(2, 2)
  nms <- sym
  vls <- matrix(c("A", 0, 1, "B"), nrow=2)
  s <- subs(2*sym, nms, vls)
  expect_equal(as.character(s), "Matrix([[2*A, 2], [0, 2*B]])")
})

