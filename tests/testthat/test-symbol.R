context("symbol")

test_that("init", {
  skip_if_no_sympy()
  
  expect_error(symbol(''))
  expect_error(symbol('2*x'))
  x <- symbol('x')
  expect_s3_class(x, 'caracas_symbol')
  
  expect_true(!is.null(x$pyobj))
  expect_s3_class(x$pyobj, 'python.builtin.object')
  expect_s3_class(x$pyobj, 'sympy.core.symbol.Symbol')
})

test_that("variable names", {
  skip_if_no_sympy()
  
  expect_s3_class(symbol('x0'), 'caracas_symbol')
  expect_error(symbol('0x'))
  expect_s3_class(symbol('xa_0'), 'caracas_symbol')
  expect_s3_class(symbol('yY_x'), 'caracas_symbol')
})

test_that("eval_to_symbol", {
  skip_if_no_sympy()
  
  ## expect_error(eval_to_symbol(''))
  ## expect_error(eval_to_symbol('2*q'))
  x <- symbol('x')
  expr <- eval_to_symbol('2*x')
  expect_s3_class(expr, 'caracas_symbol')
  expect_equal(as.character(expr), "2*x")
  
  expect_equal(as.character(eval_to_symbol("1/3")), "1/3")
  x <- symbol('x')
  expect_equal(as.character(eval_to_symbol("x*1/3")), "x/3")
  
  x1 <- symbol('x1')
  expect_equal(as.character(eval_to_symbol("x1/3")), "x1/3")
})


test_that("sympy_func", {
  skip_if_no_sympy()
  
  def_sym(x, a)
  p <- (x-a)^4
  q <- p %>% sympy_func("expand")
  expect_equal(as.character(q), "a^4 - 4*a^3*x + 6*a^2*x^2 - 4*a*x^3 + x^4")
  
  def_sym(x, y, z)
  expr <- x*y + x - 3 + 2*x^2 - z*x^2 + x^3
  qc <- expr %>% sympy_func("collect", x)
  expect_equal(as.character(qc), "x^3 + x^2*(2 - z) + x*(y + 1) - 3")
})

# test_that("matrix symbol 1x1", {  #   skip_if_no_sympy()  #  

#   W <- matrix_symbol("W") # 1 x 1 by default
#   expect_equal(dim(W), c(1L, 1L))
#   
#   J <- sympy_declare("J", "Identity(1)") 
#   expect_equal(dim(J), c(1L, 1L))
#   expect_equal(as.character(J$pyobj$as_explicit()), "Matrix([[1]])")
#   
#   O <- sympy_declare("O", "ZeroMatrix(1, 1)")
#   expect_equal(dim(O), c(1L, 1L))
#   expect_equal(as.character(O$pyobj$as_explicit()), "Matrix([[0]])")
#   
#   expect_equal(as.character(J %*% O), "0")
#   
#   L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>%
#     as_sym(declare_symbols = FALSE)
#   expect_equal(dim(L), c(2L, 2L))
#   expect_equal(as.character(L$pyobj), "Matrix([[I, 0], [-W, I]])")
#   expect_equal(as.character(L, replace_I = FALSE), "Matrix([[I, 0], [-W, I]])")
#   
#   Linv <- inv_2x2(L)
#   expect_equal(dim(Linv), c(2L, 2L))
#   expect_equal(as.character(Linv$pyobj), "Matrix([\n[I, 0],\n[W, I]])")
#   expect_equal(as.character(Linv, replace_I = FALSE), "Matrix([\n[I, 0],\n[W, I]])")
#   
#   # FIXME: TEST?
#   #A1 <- Linv %*% L
#   #A1
#   #A2 <- L %>% Linv
#   #A2
#   
#   A <- t(Linv) %*% Linv
#   expect_equal(dim(A), c(2L, 2L))
#   
#   O2 <- sympy_declare("O", "ZeroMatrix(2, 2)")
#   A <- O2 %*% t(Linv) %*% Linv
#   expect_equal(dim(A), c(2L, 2L))
#   expect_equal(as.character(A$pyobj), "0")
#   expect_equal(as.character(A), "0")
#   
#   
#   ############################
#   
#   W <- matrix_symbol("W", nrow = 2L, ncol = 2L) # 1 x 1 by default
#   expect_equal(dim(W), c(2L, 2L))
#   
#   J <- sympy_declare("J", "Identity(2)") 
#   expect_equal(dim(J), c(2L, 2L))
#   expect_equal(as.character(J$pyobj$as_explicit()), "Matrix([[1, 0], [0, 1]])")
#   
#   O <- sympy_declare("O", "ZeroMatrix(2, 2)")
#   expect_equal(dim(O), c(2L, 2L))
#   expect_equal(as.character(O$pyobj$as_explicit()), "Matrix([[0, 0], [0, 0]])")
#   
#   expect_equal(dim(J %*% O), c(2L, 2L))
#   expect_equal(as.character(J %*% O), "Matrix([[0, 0], [0, 0]])")
#   
#   
#   L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>%
#     as_sym(declare_symbols = FALSE)
#   expect_equal(dim(L), c(2L, 2L)) # FIXME?
#   expect_equal(as.character(L$pyobj), "Matrix([[I, 0], [-W, I]])")
#   expect_equal(as.character(L, replace_I = FALSE), "Matrix([[I, 0], [-W, I]])")
#   
#   Linv <- inv_2x2(L)
#   expect_equal(dim(Linv), c(4L, 4L))
#   expect_equal(as.character(Linv$pyobj), "Matrix([\n[I, 0],\n[W, I]])")
#   expect_equal(as.character(Linv, replace_I = FALSE), "Matrix([\n[I, 0],\n[W, I]])")
#   
#   A <- t(Linv) %*% Linv
#   expect_equal(dim(A), c(4L, 4L))
# })
# 



