symbol_is_matrix <- function(x) {
  xstr <- as.character(x)
  
  if (grepl("^Matrix\\(\\[", xstr)) {
    return(TRUE)
  }
  
  if (grepl("^\\[\\[", xstr)) {
    return(TRUE)
  }
  
  return(FALSE)
}

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
#' A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#' B <- as_symbol(A)
#' eigenvals(B)
#' eigenvects(B)
#' eigen(eval(as_r(B), list(a = 2)))
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
eigenvals <- function(x) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
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
#'   eigenvals(B)
#'   eigenvects(B)
#'   eigen(eval(as_r(B), list(a = 2)))
#' }
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
eigenvects <- function(x) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
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

