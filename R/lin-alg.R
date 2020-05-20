symbol_is_matrix <- function(x) {
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

symbol_is_list_of_lists_matrix <- function(x) {
  if (grepl("^\\[\\[", as.character(x))) {
    return(TRUE)
  } 
  
  return(FALSE)
}

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
  
  return(rows)
}

# On purpose: Does not ensure sympy nor check that x is a matrix
number_cols <- function(x) {
  cols <- x$cols
  
  if (inherits(cols, "python.builtin.int")) {
    cols <- reticulate::py_to_r(cols)
  }
  
  return(cols)
}

#' Get matrix as character matrix
#' 
#' @param x caracas symbol
#' 
#' @concept linalg
#' 
#' @export
as_character_matrix <- function(x) {
  y <- as_r_worker(x, as_character = TRUE)
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
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (!symbol_is_matrix(x)) {
    return(NULL)
  }
  
  rows <- number_rows(x$pyobj)
  cols <- number_cols(x$pyobj)
  
  return(c(rows, cols))
}

#' Eigenvalues
#' 
#' @param x Matrix to find eigenvalues for
#' 
#' @examples 
#' if (have_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_symbol(A)
#'   eigen_val(B)
#'   eigen_vec(B)
#'   eigen(eval(as_r(B), list(a = 2)))
#' }
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
eigen_val <- function(x) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (symbol_is_list_of_lists_matrix(x)) {
    x <- as_symbol(as_character_matrix(x), 
                   declare_variables = FALSE)
  }
  
  if (!symbol_is_matrix(x)) {
    return(NULL)
  }
  
  vals <- x$pyobj$eigenvals()
  stopifnot(inherits(vals, "python.builtin.dict"))
  
  vals_lst <- reticulate::py_to_r(vals)
  
  eig_info <- vector("list", length(vals_lst))
  
  for (i in seq_along(vals_lst)) {
    eig_info[[i]] <- list(
      eigval = as_symbol(names(vals_lst)[i]),
      eigmult = as.integer(vals_lst[i])
    )
  }
  
  return(eig_info)
}

#' Eigenvectors and eigenvalues
#' 
#' @param x Matrix to find eigenvectors and eigenvalues for
#' 
#' @examples 
#' if (have_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_symbol(A)
#'   eigen_val(B)
#'   eigen_vec(B)
#'   eigen(eval(as_r(B), list(a = 2)))
#' }
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
eigen_vec <- function(x) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (symbol_is_list_of_lists_matrix(x)) {
    x <- as_symbol(as_character_matrix(x), 
                   declare_variables = FALSE)
  }
  
  if (!symbol_is_matrix(x)) {
    return(NULL)
  }
  
  vals <- x$pyobj$eigenvects()
  stopifnot(inherits(vals, "python.builtin.list"))
  
  vals_lst <- reticulate::py_to_r(vals)
  
  eig_info <- vector("list", length(vals_lst))
  
  for (i in seq_along(vals_lst)) {
    eigvalvec <- vals_lst[[i]]
    
    res <- list(
      eigval = construct_symbol_from_pyobj(eigvalvec[[1L]]),
      eigmult = as.integer(eigvalvec[[2L]]),
      eigvec = construct_symbol_from_pyobj(eigvalvec[[3L]][[1L]])
    )
    
    eig_info[[i]] <- res
  }

  return(eig_info)
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
  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xT <- x$pyobj$T
  return(construct_symbol_from_pyobj(xT))
}

#' Calculate the Determinant of a Matrix
#' 
#' Note that there is no argument for `logarithm` as with the generic
#' method.
#'
#' @param x A `caracas_symbol`
#' @param \dots Not used
#'
#' @concept linalg
#' @export
determinant.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xdet <- x$pyobj$det()
  
  return(construct_symbol_from_pyobj(xdet))
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
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (!symbol_is_matrix(x)) {
    stop(paste0("'x' is not a matrix"))
  }
  
  y <- eval_to_symbol(x$pyobj$diagonal())
  
  return(y)
}


#' Replace matrix diagonal
#'
#' @param x Object `x`
#' @param value Replacement value
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
#' if (have_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_symbol(A)
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
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xmat <- convert_to_r_mat(x)
  diag(xmat) <- value
  y <- as_symbol(xmat)
  return(y)
}