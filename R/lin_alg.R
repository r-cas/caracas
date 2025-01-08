
stopifnot_matrix <- function(x) {
  # MatrixSymbol
  ## try({
  ##   res <- x$pyobj$is_MatrixExpr
    
  ##   if (is.logical(res) && isTRUE(res)) {
  ##     return(invisible(NULL))
  ##   }
    
  ##   if (reticulate::py_to_r(res)) {
  ##     return(invisible(NULL))
  ##   }
  ## }, silent = TRUE)
  
  if (!symbol_is_matrix(x)) {
      stop("'x' must be a matrix")
  }
}

#' Check if object is a caracas matrix
#'
#' Check if object is a caracas matrix
#'
#' @param x An object
#'
#' @examples
#' 
#' if (has_sympy() && sympy_version() >= "1.6") {
#'   x <- vector_sym(4)
#'   symbol_is_matrix(x)   ## TRUE
#'   x2 <- as.character(x) ## "Matrix([[v1], [v2], [v3], [v4]])"
#'   symbol_is_matrix(x2)  ## TRUE
#'   x3 <- as_character_matrix(x) ## R matrix
#'   symbol_is_matrix(x3)  ## FALSE 
#' }
#' 
#' @concept linalg
#' @export
symbol_is_matrix <- function(x) {

    if (length(x) != 1L) {
        cat("length not 1\n")
        return(FALSE)
    }

    ## FIXME (SH) Need this? Can go into loop
    ## MatrixSymbol
    ## try({
    ##     res <- x$pyobj$is_MatrixExpr
        
    ##     if (is.logical(res) && isTRUE(res)) {
    ##         return(TRUE)
    ##     }
        
    ##     if (reticulate::py_to_r(res)) {
    ##         return(TRUE)
    ##     }
    ## }, silent = TRUE)


    xstr <- as.character(x)
    
    if (grepl("^Matrix\\(\\[", xstr)) {
        return(TRUE)
    }
    
    return(FALSE)
}


scalar_to_matrix <- function(scalar, dims) {
  matrix(rep(scalar, prod(dims)), nrow = dims[1L], ncol = dims[2L])
}


# On purpose: Does not ensure sympy nor check that x is a matrix
number_rows <- function(x) {
  rows <- x$rows
  
  if (inherits(rows, "python.builtin.int")) {
    rows <- reticulate::py_to_r(rows)
  }
  
  if (inherits(rows, "sympy.core.numbers.Integer")) {
    # FIXME: For MatrixSymbols; avoid as.character()?
    rows <- as.integer(as.character(rows))
  }
  
  return(rows)
}

# On purpose: Does not ensure sympy nor check that x is a matrix
number_cols <- function(x) {
  cols <- x$cols
  
  if (inherits(cols, "python.builtin.int")) {
    cols <- reticulate::py_to_r(cols)
  }
  
  if (inherits(cols, "sympy.core.numbers.Integer")) {
    # FIXME: For MatrixSymbols; avoid as.character()?
    cols <- as.integer(as.character(cols))
  }
  
  return(cols)
}

#' Get matrix as character matrix
#' 
#' @param x caracas symbol
#' 
#' @concept linalg
#' @examples
#' if (has_sympy()) {
#'   s  <- as_sym("[[r1, r2, r3], [u1, u2, u3]]")
#'   s2 <- apply(as_character_matrix(s), 2, function(x) (paste("1/(", x, ")")))
#'   as_sym(s2)
#' }
#' 
#' @export
as_character_matrix <- function(x) {
    ensure_sympy()
    stopifnot_symbol(x)
    
    y <- as_expr_worker(x, as_character = TRUE)
    return(eval(parse(text = y)))
}


#' Dimensions of a caracas symbol
#' 
#' @param x caracas symbol
#' 
#' @concept linalg
#' 
#' @export
dim.caracas_symbol <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)

  ## print(symbol_is_matrix(x))
  # FIXME: This contract is used in dim<- setter
  if (!symbol_is_matrix(x)) { 
    return(NULL) 
  }

  rows <- number_rows(x$pyobj)
  cols <- number_cols(x$pyobj)

  return(c(rows, cols))
}









#' Dimensions of a caracas symbol
#' 
#' @param x caracas symbol
#' @param value new dimension
#' 
#' @examples
#' if (has_sympy()) {
#'   v <- vector_sym(4)
#'   v
#'   dim(v)
#'   dim(v) <- c(2, 2)
#'   v
#'   m <- matrix_sym(2, 2)
#'   dim(m)
#'   dim(m) <- c(4, 1)
#'   m
#' }
#' 
#' @concept linalg
#' 
#' @export
`dim<-.caracas_symbol` <- function(x, value) {
  ensure_sympy()
  stopifnot_symbol(x)
  
  if (is.null(dim(x))) {
    stop("x does not have a dimension to change")
  }
  
  if (prod(dim(x)) != prod(value)) {
    stop("Wrong number of elements")
  }
  
  # FIXME: Currently works because dim(x) is only non-NULL for matrices
  m1 <- as_character_matrix(x)
  dim(m1) <- value
  m1 <- as_sym(m1)
  m1
}

#' Transpose of matrix
#'
#' @param x If `caracas_symbol` treat as such, else
#' call [base::t()].
#'
#' @concept linalg
#' @export
t.caracas_symbol <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  xT <- x$pyobj$T
  
  return(construct_symbol_from_pyobj(xT))
}


#' Elementwise reciprocal matrix
#'
#' @param x Object `x`
#' @param numerator The numerator in the result.
#'
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   s <- as_sym("[[r1, r2, r3], [u1, u2, u3]]")
#'   reciprocal_matrix(s, numerator = 7)
#' }
#' 
#' @export
reciprocal_matrix <- function(x, numerator=1) {
    ensure_sympy()
    stopifnot_matrix(x)
    
    numerator <- as.character(numerator)
    
    if (grepl("^-?[0-9]+$", numerator)) {
        ## S(): Sympify
        ## Means that this will be e.g. (S(1))/(1) = 1 instead of 1/1 = 1.0 (numeric)
        numerator <- paste0("S(", numerator, ")")
    }
    
    rx <- apply(as_character_matrix(x), 2, function(xx) {
        paste0("(", numerator, ")", "/(", xx, ")")
    })
    
    dim(rx) <- dim(x)
    
    return(as_sym(rx))
}


#' Matrix power
#'
#' @param x A `caracas_symbol`, a matrix.
#' @param pow Power to raise matrix `x` to
#' 
#' @examples
#' if (has_sympy() && sympy_version() >= "1.6") {
#'   M <- matrix_(c("1", "a", "a", 1), 2, 2)
#'   M
#'   mat_pow(M, 1/2)
#' }
#' 
#' @concept linalg
#' @export
mat_pow <- function(x, pow = "1") {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  #pow_str <- deparse1(substitute(pow)) # to avoid 1/2 gets converted to 0.5
  pow_str <- paste0(deparse(substitute(pow))) # to avoid 1/2 gets converted to 0.5

  if (!("pow" %in% names(x$pyobj))) {
    stop("pow() not supported by SymPy version")
  }
  
  x_pow <- x$pyobj$pow(pow_str)
  
  return(construct_symbol_from_pyobj(x_pow))
}


## ' Matrix diagonal
## '
## ' @param x Object `x`
## ' @param \dots Passed on
## ' 
## ' @concept linalg
## ' 
## ' @export


## #' Matrix diagonal
## #' 
## #' @param x Object `x`
## #' @param \dots Not used
## #' 
## #' @concept linalg
## #' 
## #' @export
## diag.caracas_symbol <- function(x, ...) {
##   ensure_sympy()
##   stopifnot_symbol(x)
##   stopifnot_matrix(x)  
  
##   y <- eval_to_symbol(x$pyobj$diagonal())
  
##   return(y)
## }

#' Matrix diagonal
#' 
#' @param x Object `x`
## #' @param \dots Not used
#' 
#' @concept linalg
#' @importFrom Matrix diag
#' @method diag caracas_symbol
#' @export
setMethod(
  "diag",
  signature(x = "caracas_symbol"),
  function(x) {
      ensure_sympy()
      stopifnot_symbol(x)
      stopifnot_matrix(x)  
      y <- eval_to_symbol(x$pyobj$diagonal())
      return(y)
  }
)


## diag <- function(x, ...) {
##   UseMethod("diag")
## }

## #' @export
## diag.default <- function(x, ...) {
##   return(base::diag(x, ...))
## }


#' Replace matrix diagonal
#'
#' @param x Object `x`
#' @param value Replacement value
#' 
#' @name diag-set
#' 
#' @concept linalg
#' 
#' @export
`diag<-` <- function(x, value) {
  UseMethod("diag<-")
}

#' @export
`diag<-.default` <- function(x, value) {
  return(base::`diag<-`(x, value))
}

#' Replace diagonal
#' 
#' @param x A `caracas_symbol`.
#' @param value Replacement value
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_sym(A)
#'   B
#'   diag(B)
#'   diag(B) <- "b"
#'   B
#'   diag(B)
#' }
#' 
#' @concept vectors
#' 
#' @export
`diag<-.caracas_symbol` <- function(x, value) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  xmat <- convert_to_r_mat(x)
  
  if (inherits(value, "caracas_symbol")) {
    value <- c(as_character_matrix(value))
  }
  
  diag(xmat) <- value
  
  y <- as_sym(xmat)
  return(y)
}

#' Construct diagonal matrix from vector
#' 
#' @param x Matrix with 1 row or 1 column that is the 
#' diagonal in a new diagonal matrix
#' 
#' @examples 
#' if (has_sympy()) {
#'   d <- as_sym(c("a", "b", "c"))
#'   D <- as_diag(d)
#'   D
#' }
#' 
#' @concept linalg
#' 
#' @export
as_diag <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  if (!(nrow(x) == 1L || ncol(x) == 1L)) {
    stop("Either supply a row vector or a column vector with at least one element")
  }
  
  if (nrow(x) > 1L) {
    x <- t(x)
  }
  
  n <- ncol(x) 
  D <- as_sym(diag(n))
  
  for (i in seq_len(n)) {
    D[i, i] <- as.character(x[1L, i])
  }
  
  return(D)
}


#' Stacks matrix to vector
#' 
#' @param x Matrix
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- as_sym(matrix(1:9, 3))
#'   as_vec(A)
#' }
#' 
#' @concept linalg
#' 
#' @export
as_vec <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  stopifnot(length(dim(x)) == 2)
  x2 <- do.call(rbind, lapply(seq_len(ncol(x)), function(j) x[, j]))
  
  return(x2)
}



#' Symbolic diagonal matrix
#'
#' @param x Character vector with diagonal
#' @param n Number of times `x` should be repeated
#' @param declare_symbols Passed on to `as_sym()` when constructing symbolic matrix
#' @param \dots Passed on to `rep(x, n, ...)`
#'
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   diag_(c(1,3,5))
#'   diag_(c("a", "b", "c"))
#'   diag_("a", 2)
#'   diag_(vector_sym(4))
#' }
#' 
#' @export
diag_ <- function(x, n = 1L, declare_symbols = TRUE, ...) {
  ensure_sympy()

  
  if(inherits(x, "caracas_symbol")) {
      x <- as_character_matrix(x)
  }

  x <- rep(x, n, ...)
  n <- length(x)
  
  A <- matrix("0", nrow = n, ncol = n)
  diag(A) <- x
  y <- as_sym(A, declare_symbols = declare_symbols)
  
  return(y)
}

#' Symbolic matrix
#'
#' @param \dots Passed on to [matrix()]
#' @param declare_symbols Passed on to `as_sym()` when constructing symbolic matrix
#'
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   matrix_(1:9, nrow = 3)
#'   matrix_("a", 2, 2)
#' }
#' 
#' @export
matrix_ <- function(..., declare_symbols = TRUE) {
  ensure_sympy()
  
  args <- list(...)
  
  #if (length(args) >= 1L && inherits(args[[1L]], "caracas_vector")) {
  if (length(args) >= 1L && inherits(args[[1L]], "caracas_symbol") && symbol_is_matrix(args[[1L]])) {
    args[[1L]] <- as_character(args[[1L]])
  }
  
  A <- do.call(matrix, args)
  
  #A <- matrix(...)
  y <- as_sym(A, declare_symbols = declare_symbols)
  
  return(y)
}


#' Generate generic vectors and matrices
#'
#' Generate generic vectors and matrices.
#'
#' @name generic-matrices
#' 
#' @param n Length of vector
#' @param entry The symbolic name of each entry.
#' 
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   vector_sym(4, "b")
#'   matrix_sym(3, 2, "a")
#'   matrix_sym_diag(4, "s")
#'   matrix_sym_symmetric(4, "s")
#' }
#'
#' @rdname generic-matrices
#' @export 
vector_sym <- function(n, entry = "v") { 
  ensure_sympy()
  
  as_sym(paste0(entry, seq_len(n)))
}

#' @rdname generic-matrices
#' @param nrow,ncol Number of rows and columns
#' 
#' @export 
matrix_sym <- function(nrow, ncol, entry = "v"){
  ensure_sympy()
  
  out <- outer(seq_len(nrow), seq_len(ncol), FUN = function(r, c) paste0(entry, r, c))
  out <- matrix_(out, nrow = nrow)
  out
}

#' @rdname generic-matrices
#' @export 
matrix_sym_diag <- function(nrow, entry = "v") {
  ensure_sympy()
  
  out <- diag_(paste0(entry, seq_len(nrow)))
  out 
}

#' @rdname generic-matrices
#' @export 
matrix_sym_symmetric <- function(nrow, entry = "v") { 
  ensure_sympy()
  
  out <- matrix(data = "", nrow = nrow, ncol = nrow)
  for (r in seq_len(nrow)) {
    for (c in seq_len(nrow)) {
      if (r > c) {
        out[r, c] <- paste0(entry, r, c)
      } else {
        out[r, c] <- paste0(entry, c, r)            
      }
    }            
  }
  out <- matrix_(out, nrow = nrow)
  out
}



#' Column space (range) of a symbolic matrix
#'
#' Column space (range) of a symbolic matrix
#'
#' @param x Symbolic matrix
#'
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   X1 <- matrix_(paste0("x_",c(1,1,1,1, 2,2,2,2, 3,4,3,4)), nrow = 4)
#'   X1
#'   colspan(X1)
#'   do_la(X1, "columnspace")
#'   rankMatrix_(X1)
#'   
#'   X2 <- matrix_(paste0("x_",c(1,1,1,1, 0,0,2,2, 3,4,3,4)), nrow = 4)
#'   X2
#'   colspan(X2)
#'   do_la(X2, "columnspace")
#'   rankMatrix_(X2)
#' }
#' 
#' @importFrom stats model.matrix
#' @export
colspan <- function(x) {
  ensure_sympy()
  
  #rf <- do_la(x, "rref")
  #x[, rf$pivot_vars]
  
  do.call(cbind, do_la(x, "columnspace"))
  
}

#' Rank of matrix
#'
#' Rank of symbolic or numeric matrix. 
#'
#' @param x Numeric or symbolic matrix.
#' @param tol Tolerence, only relevant for a numeric matrix.
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   X <- matrix_(paste0("x_",c(1,1,1,1,2,2,2,2,3,4,3,4)), nrow=4)
#'   X
#'   rankMatrix_(X)
#'   colspan(X)
#' }

## FIXME SH addition. Not sure this is good idea
#' @importFrom Matrix rankMatrix
#' @export
rankMatrix_ <- function(x, tol=NULL) {
  ensure_sympy()
  
  if (is.numeric(x)) {
      c(Matrix::rankMatrix(x, tol=tol))
  } else {
      do_la(x, "rank")
  }
}



#' Add prefix to each element of matrix
#' 
#' Add prefix to each element of matrix
#' 
#' @param x Numeric or symbolic matrix
#' @param prefix A character vector
#' @concept linalg
#' 
#' @examples
#' if (has_sympy()) {
#'   X <- matrix_sym(2, 3)
#'   X
#'   add_prefix(X, "e")
#'   
#'   X <- matrix(1:6, 3, 2)
#'   X
#'   add_prefix(X, "e")
#' }
#' 
#' @export
add_prefix <- function(x, prefix = "") {
  ensure_sympy()
  
  if (inherits(x, "caracas_symbol")) {
    x <- as_character_matrix(x)
  }
  
  w <- apply(x, 2, function(y) {
    paste0(prefix, y)
  })
  
  as_sym(w)
}




#' Form Row and Column Sums
#'
#' Form Row and Column Sums
#'
#' @param x Symbolic matrix
#'
#' @concept linalg
#' @name rowSums_colSums
#' 
#' @examples
#' if (has_sympy()) {
#'   X <- matrix_(paste0("x_",c(1,1,1,1,2,2,2,2,3,4,3,4)), nrow=4)
#'   rowSums_(X)
#'   colSums_(X)
#' }
#' 
#' @export
rowSums_ <- function(x) {
  ensure_sympy()
  stopifnot_matrix(x)
  
  y <- as_sym(rep(1, ncol(x)))
  x %*% y
}

#' @rdname rowSums_colSums
#' @export
colSums_ <- function(x) {
  ensure_sympy()
  stopifnot_matrix(x)
  
  y <- as_sym(rep(1, nrow(x)))
  t(y) %*% x
}



#' Special matrices: zeros_sym, ones_sym, eye_sym
#' @name special_matrices
#' @param nrow,ncol Number of rows and columns of output
#' @seealso [diag_()], [matrix_sym()], [vector_sym()]
#' @examples
#'
#' if (has_sympy()){
#'   zeros_sym(3, 4)
#'   ones_sym(3, 4)
#'   eye_sym(3, 4)
#' }
#' 
#' @export
#' @concept linalg
#' @rdname special_matrices
zeros_sym <- function(nrow, ncol) {
    if (missing(ncol)){
        ncol <- nrow
    }
    as_sym(matrix(0, nrow=nrow, ncol=ncol))
}

#' @export
#' @rdname special_matrices
ones_sym <- function(nrow, ncol) {
    if (missing(ncol)){
        ncol <- nrow
    }
  as_sym(matrix(1, nrow=nrow, ncol=ncol))
}

#' @export
#' @rdname special_matrices
eye_sym <- function(nrow, ncol) {
    if (missing(ncol)){
        ncol <- nrow
    }
  if (nrow==ncol)
    return(diag_(1, nrow))
  m <- min(nrow, ncol)
  out <- matrix(0, nrow=nrow, ncol=ncol)
  d <- diag(1, m)
  out[1:m, 1:m] <- d
  as_sym(out)
}

#' Difference matrix
#'
#' @param N Number of rows (and columns)
#' @param l Value / symbol below main diagonal
#' @param d Value / symbol on main diagonal
#'
#' @examples
#' if (has_sympy()){
#'   Dm <- diff_mat(4)
#'   Dm
#'   y <- vector_sym(4, "y")
#'   Dm %*% y
#' }
#' @concept linalg
#' 
#' @export
diff_mat <- function(N, l="-1", d=1) {
  L1 <- diag(d, N)
  L1[cbind(1 + (1:(N-1)), 1:(N-1))] <- l
  L1 <- as_sym(L1)
  L1
}



#' Matrix cross product
#'
#' @name matrix_cross_product
#' @param x,y caracas matrices
#' @concept linalg
#'
#' @examples
#' if (has_sympy()) {
#'   s  <- as_sym("[[r1, r2, r3], [u1, u2, u3]]")
#'   s2 <- apply(as_character_matrix(s), 2, function(x) (paste("1/(", x, ")")))
#'   as_sym(s2)
#' }

#' 
#' @export
#' @rdname matrix_cross_product
crossprod_ <- function(x, y=NULL) {
    ensure_sympy()
    stopifnot_matrix(x)
    
    if (is.null(y)) {
        t(x) %*% x        
    } else {
        stopifnot_matrix(y)       
        t(x) %*% y
    }
}



#' @export
#' @rdname matrix_cross_product
tcrossprod_ <- function(x, y=NULL) {
    ensure_sympy()
    stopifnot_matrix(x)    
    
    if (is.null(y)) {
        x %*% t(x) 
    } else {
        stopifnot_matrix(y)        
        x %*% t(y) 
  }
}

#' @importFrom Matrix crossprod
#' @method crossprod caracas_symbol
#' @export
#' @rdname matrix_cross_product
setMethod(
  "crossprod",
  signature(x = "caracas_symbol", y = "missing"),
  function(x, y) {
    crossprod_(x, y)
  }
)

#' @importFrom Matrix tcrossprod
#' @method tcrossprod caracas_symbol
#' @export
#' @rdname matrix_cross_product
setMethod(
  "tcrossprod",
  signature(x = "caracas_symbol", y = "missing"),
  function(x, y) {
    tcrossprod_(x, y)
  }
)










#' Get basis
#'
#' Get basis
#'
#' @param x caracas vector / matrix
#' @examples
#' if (has_sympy()) {
#'   x <- vector_sym(3)
#'   get_basis(x)
#' 
#'   W <- matrix(c("r_1", "r_1", "r_2", "r_2", "0", "0", "u_1", "u_2"), nrow=4)
#'   W <- as_sym(W)
#'   get_basis(W)
#' }
#' @concept linalg
#' @export
get_basis <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  
  zz <- as_character_matrix(x)
  ##unique symbols
  us <- setdiff(unique(as.character(zz)), "0")
  out <- lapply(seq_along(us),
                function(i) {
                  1*(us[i] == zz)           
                })
  names(out) <- us
  ## attr(out, "symbols") <- us
  out
}







# Still needed?
#symbol_is_list_of_lists_matrix <- function(x) {
#   if (grepl("^\\[\\[", as.character(x))) {
#     return(TRUE)
#   } 
#   
#   return(FALSE)
# }

# symbol_is_vector <- function(x) {
#   xstr <- as.character(x)
#   
#   if (grepl("^\\[", xstr)) {
#     return(TRUE)
#   }
#   
#   return(FALSE)
# }


# ensure_symbol_is_matrix <- function(x) {
#   if (!inherits(x, "caracas_symbol")) {
#     stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
#   }
#   
#   if (!symbol_is_matrix(x)) {
#     stop(paste0("'x' is not a matrix"))
#   }
# }




  # zz <- as_character_matrix(x)
  # 
  # uu <- apply(zz, 2, factor, simplify = FALSE)
  # 
  # mm <-
  #   lapply(seq_along(uu),
  #          function(i){
  #            vv <- uu[[i]]
  #            if (length(levels(vv)) == 1) {
  #              out <- matrix(rep(1, length(vv)))
  #            }
  #            else {
  #              out <- model.matrix(~ 0 + vv)
  #            }
  #            colnames(out) <- levels(uu[[i]])
  #            out
  #          })
  # 
  # x_mat <- do.call(cbind, mm)
  # zero <- which(colnames(x_mat) == "0")
  # if (length(zero) > 0)
  #     x_mat <- x_mat[, -zero]
  # x_mat
