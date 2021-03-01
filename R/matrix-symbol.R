## Create matrix symbol
## 
## @param x Name of matrix symbol
## @param nrow Number of rows
## @param ncol Number of columns
## 
## @examples 
## if (has_sympy()) {
##    A <- matrix_symbol("A", 3, 3)
##    A
##    dim(A)
##    t(A)
##    inv(A)
## }
## 
## @seealso [sympy_declare()]
## 
## @concept caracas_symbol
## 
## @export
#matrix_symbol <- function(x, nrow = 1L, ncol = 1L) {
#  ensure_sympy()
#  verify_variable_name(x)
#  
#  cmd <- paste0(x, " = MatrixSymbol('", x, "', ", nrow, ", ", ncol, ")")
#  s <- reticulate::py_run_string(cmd, convert = FALSE)
#  res <- s[[x]]
#  y <- construct_symbol_from_pyobj(res)
#  
#  return(y)
#}
#
#
#
#
## Declare symbol manually by Python syntax
## 
## @param x Name of symbol
## @param cmd Command (SymPy/Python syntax)
## 
## @examples 
## if (has_sympy()) {
##    x <- sympy_declare("x", "Symbol('x')")
##    x
##    
##    x <- sympy_declare("x", "Symbol('x', positive = True)")
##    x
##    ask(x, "positive")
##    
##    A <- matrix_symbol("A", 3, 3) # or: sympy_declare("A", "MatrixSymbol('A', 3, 3)")
##    A
##    dim(A)
##    t(A)
##    inv(A)
##    
##    A <- matrix_symbol("A", "MatrixSymbol('A', 3, 3)")
##    O <- sympy_declare("O", "ZeroMatrix(3, 3)")
##    A %*% O
## }
## 
## @seealso [matrix_symbol()]
## 
## @concept caracas_symbol
## 
## @export
#sympy_declare <- function(x, cmd) {
#  caracas:::ensure_sympy()
#  
#  python_cmd <- paste0(x, " = ", cmd)
#  s <- reticulate::py_run_string(python_cmd, convert = FALSE)
#  res <- s[[x]]
#  y <- caracas:::construct_symbol_from_pyobj(res)
#  
#  return(y)
#}
#
#
#
#
#
## Inverse of 2x2 matrix
## 
## Finds the inverse of
## ```
## [A  B]
## [C  D]
## ```
## based on block matrix inversion as explained at
## <https://en.wikipedia.org/wiki/Block_matrix#Block_matrix_inversion>.
## 
## @param x 2x2 matrix to be inverted
## 
## @examples 
## if (has_sympy()) {
##    W <- matrix_symbol("W") # 1 x 1 by default
##    J <- sympy_declare("J", "Identity(1)") # Instead of I to avoid confusion with imaginary unit
##    O <- sympy_declare("O", "ZeroMatrix(1, 1)")
##    L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>%
##      as_sym(declare_symbols = FALSE)
##    L
##    #inv(L): SymPy cannot figure it out...
##    Linv <- inv_2x2(L)
##    Linv
##    
##    t(Linv) %*% Linv
##    O2 <- sympy_declare("O", "ZeroMatrix(2, 2)")
##    A <- O2 %*% t(Linv) %*% Linv
##    A
##    dim(A)
## }
## 
## @return Columns of result. 
## The reason it is not a matrix is because SymPy has problems as 'I' both means ide
## 
## @concept caracas_symbol
## 
## @export
#inv_2x2 <- function(x) {
#  # https://en.wikipedia.org/wiki/Block_matrix#Block_matrix_inversion
#  
#  Woodbury
#  Sherman-Morrison
# 
#  ensure_sympy()
#  
#  d <- dim(x)
#  
#  if (is.null(d) || length(d) != 2L || d[1L] != 2L || d[2L] != 2L) {
#    stop("'x' must be a 2x2 matrix")
#  }
#  
#  Ap <- x$pyobj[0]
#  Aip <- Ap$inverse()
#  Bp <- x$pyobj[1]
#  Cp <- x$pyobj[2]
#  Dp <- x$pyobj[3]
#
#  s <- get_sympy()
#  
#  mult3 <- function(x, y, z) {
#    s$MatMul(x, y, z)
#  }
#
#  mult5 <- function(x1, x2, x3, x4, x5) {
#    s$MatMul(x1, x2, x3, x4, x5)
#  }
#  
#  add <- function(x, y) {
#    s$MatAdd(x, y)
#  }
#  
#  neg_mat <- function(z) {
#    reticulate::py_eval(paste0("-", as.character(z)))
#  }
#
#  CAiB <- mult3(Cp, Aip, Bp)
#  nCAiB <- neg_mat(CAiB)
#  DnCAiB <- add(Dp, nCAiB)
#  DnCAiBinv <- DnCAiB$inverse()
#  
#  e11 <- add(Aip, mult5(Aip, Bp, DnCAiBinv, Cp, Aip))
#  e12 <- neg_mat(mult3(Aip, Bp, DnCAiBinv))
#  e21 <- neg_mat(mult3(DnCAiBinv, Cp, Aip))
#  e22 <- DnCAiBinv
#  
#  e11 <- e11$doit()
#  e12 <- e12$doit()
#  e21 <- e21$doit()
#  e22 <- e22$doit()
#  
#  
#  elements <- list(e11, e12, e21, e22)
#  
#  # Resolve dims
#  elements_dims <- lapply(seq_along(elements), function(i) {
#    e <- elements[[i]]
#    
#    try({
#      return(list(e$rows, e$cols))
#    }, silent = TRUE)
#    
#    return(list(NULL, NULL))
#  })
#  
#  n_NULL_dim <- sum(unlist(lapply(elements_dims, function(l) is.null(unlist(l)))))
#  # FIXME: Maybe 2 if both in diagonal/off-diagonal?
#  if (n_NULL_dim > 1L) {
#    stop("Too many unknown dimensions")
#  }
#  
#  if (is.null(elements_dims[[1L]][[1L]])) {
#    # FIXME
#    stop("Not implemented")
#  }
#  
#  if (is.null(elements_dims[[2L]][[1L]])) {
#    # FIXME
#    elements_dims[[2L]] <- c(elements_dims[[1L]][1L], elements_dims[[3L]][2L])
#  }
#  
#  if (is.null(elements_dims[[3L]][[1L]])) {
#    # FIXME
#    stop("Not implemented")
#  }
#  
#  if (is.null(elements_dims[[4L]][[1L]])) {
#    # FIXME
#    stop("Not implemented")
#  }
#  
#  
#  
#  
#  elements_str <- lapply(seq_along(elements), function(i) {
#    e <- elements[[i]]
#    
#    if (inherits(e, "sympy.matrices.expressions.special.Identity")) {
#      if (as.character(e$rows) != as.character(e$cols)) {
#        stop("Unexpected")
#      }
#      
#      return(paste0("Identity(", as.character(e$rows), ")"))
#    }
#
#    else if (inherits(e, "sympy.core.numbers.Zero")) {
#      #return(paste0("ZeroMatrix(1, 1)"))
#      
#      nr <- elements_dims[[i]][[1L]]
#      nc <- elements_dims[[i]][[2L]]
#      return(paste0("ZeroMatrix(", nr, ", ", nc, ")"))
#    }
#    
#    else if (inherits(e, "sympy.matrices.expressions.special.ZeroMatrix")) {
#      return(paste0("ZeroMatrix(", as.character(e$rows), ",", 
#                    as.character(e$cols), ")"))
#    }
#    
#    return(as.character(e))
#  })
#  
#  y <- paste0("BlockMatrix([[",
#              elements_str[[1L]], ", ",
#              elements_str[[2L]],
#              "], [",
#              elements_str[[3L]], ", ",
#              elements_str[[4L]], "]])")
#  z <- reticulate::py_eval(y, convert = FALSE)
#  w <- construct_symbol_from_pyobj(z)
#  return(w)
#  
##   e11 <- construct_symbol_from_pyobj(e11)
##   e12 <- construct_symbol_from_pyobj(e12)
##   e21 <- construct_symbol_from_pyobj(e21)
##   e22 <- construct_symbol_from_pyobj(e22)
##   
##   # Columns
##   return(list(
##     list(e11, e21),
##     list(e12, e22))
##   )
#}

