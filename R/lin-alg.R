stopifnot_matrix <- function(x){
  # MatrixSymbol
  try({
    res <- x$pyobj$is_MatrixExpr
    
    if (is.logical(res) && isTRUE(res)) {
      return(invisible(NULL))
    }
    
    if (reticulate::py_to_r(res)) {
      return(invisible(NULL))
    }
  }, silent = TRUE)
  
  if (!symbol_is_matrix(x)) {
      stop("'x' must be a matrix")
  }
}

symbol_is_matrix <- function(x) {
  # MatrixSymbol
  try({
    res <- x$pyobj$is_MatrixExpr
    
    if (is.logical(res) && isTRUE(res)) {
      return(TRUE)
    }
    
    if (reticulate::py_to_r(res)) {
      return(TRUE)
    }
  }, silent = TRUE)
  
  xstr <- as.character(x)
  
  if (grepl("^Matrix\\(\\[", xstr)) {
    return(TRUE)
  }
  
  # FIXME: From der() and der2()
  # if (grepl("^\\[\\[", xstr)) {
  #   return(TRUE)
  # }
  
  return(FALSE)
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

scalar_to_matrix <- function(scalar, dims) {
  matrix(rep(scalar, prod(dims)), nrow = dims[1L], ncol = dims[2L])
}


# ensure_symbol_is_matrix <- function(x) {
#   if (!inherits(x, "caracas_symbol")) {
#     stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
#   }
#   
#   if (!symbol_is_matrix(x)) {
#     stop(paste0("'x' is not a matrix"))
#   }
# }

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
  
  if (!symbol_is_matrix(x)) { 
    return(NULL) 
  }

  rows <- number_rows(x$pyobj)
  cols <- number_cols(x$pyobj)

  return(c(rows, cols))
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
reciprocal_matrix <- function(x, numerator = 1){
  ensure_sympy()

  if (!symbol_is_matrix(x)) {
    stop("'x' must be sympy matrix\n")
  }
  
  numerator <- as.character(numerator)
  
  if (grepl("^-?[0-9]+$", numerator)) {
    # S(): Sympify
    # Means that this will be e.g. (S(1))/(1) = 1 instead of 1/1 = 1.0 (numeric)
    numerator <- paste0("S(", numerator, ")")
  }

  rx <- apply(as_character_matrix(x), 2, function(xx) {
    paste0("(", numerator, ")", "/(", xx, ")")
  })
  
  return(as_sym(rx))
}



#' Matrix diagonal
#'
#' @param x Object `x`
#' @param \dots Passed on
#' 
#' @concept linalg
#' 
#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

#' @export
diag.default <- function(x, ...) {
  return(base::diag(x, ...))
}


#' Matrix diagonal
#' 
#' @param x Object `x`
#' @param \dots Not used
#' 
#' @concept linalg
#' 
#' @export
diag.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)  
  
  y <- eval_to_symbol(x$pyobj$diagonal())
  
  return(y)
}


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
#'   vec(A)
#' }
#' 
#' @concept linalg
#' 
#' @export
vec <- function(x) {
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
#'   diag_sym(c("a", "b", "c"))
#'   diag_sym("a", 2)
#' }
#' 
#' @export
diag_sym <- function(x, n = 1L, declare_symbols = TRUE, ...){
  ensure_sympy()
  
  if (!is.character(x)) {
    stop("'x' must be a character")
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
#'   matrix_sym(1:9, nrow = 3)
#'   matrix_sym("a", 2, 2)
#' }
#' 
#' @export
matrix_sym <- function(..., declare_symbols = TRUE){
  ensure_sympy()
  
  A <- matrix(...)
  y <- as_sym(A, declare_symbols = declare_symbols)
  
  return(y)
}