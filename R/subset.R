
#' Extract or replace parts of an object
#' 
#' @param x A `caracas_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' @param \dots Not used
#' @param drop Simplify dimensions of resulting object
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_sym(A)
#'   B[1:2, ]
#'   B[, 2]
#'   B[2, , drop = FALSE]
#' }
#' 
#' @concept vectors
#' 
#' @export
`[.caracas_symbol` <- function(x, i, j, ..., drop = TRUE) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  ## if (!symbol_is_matrix(x)) {
  ##   stop("'x' must be a matrix")
  ## }
  
  # xmat <- convert_to_r_mat(x)
  # xmat_subset <- base::`[`(xmat, i, j, ..., drop = drop)
  # y <- as_sym(xmat_subset)
  # return(y)
  
  # A <- matrix_sym(4, 3)
  # A[2]
  # A[2, ]
  # A[, 2]
  # A[1:2, 2]
  # A[2, drop = FALSE]
  xmat <- convert_to_r_mat(x)
  
  
  
  n_args_without_drop <- nargs()
  arg_names <- names(as.list(match.call()))
  if ("drop" %in% arg_names) {
    n_args_without_drop <- n_args_without_drop - 1L
  }
  res <- NULL
  
  if (n_args_without_drop == 2L) {
    if (missing(i)) {
      res <- base::`[`(xmat, j = j, ..., drop = drop)
    } else if (missing(j)) {
      res <- base::`[`(xmat, i = i, ..., drop = drop)
    }
  } else if (n_args_without_drop == 3L) {
    res <- base::`[`(xmat, i, j, ..., drop = drop)
  }
  
  #xmat_subset <- base::`[`(xmat, i, j, ..., drop = drop)
  
  y <- as_sym(res)
  return(y)
}

#' Extract or replace parts of an object
#' 
#' @param x A `caracas_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' @param \dots Not used
#' @param value Replacement value
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_sym(A)
#'   B[, 2] <- "x"
#'   B[, 3] <- vector_sym(3)
#'   B
#' }
#' 
#' @concept vectors
#' 
#' @export
`[<-.caracas_symbol` <- function(x, i, j, ..., value) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }

  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xmat <- convert_to_r_mat(x)

  if (inherits(value, "caracas_symbol")) {
    if (symbol_is_matrix(value)) {
      value <- convert_to_r_mat(value)
    } else {
      value <- as.character(value)
    }
  }
  
  xmat_mod <- if (missing(j)) {
    base::`[<-`(xmat, i, ..., value = value)
  } else {
    base::`[<-`(xmat, i, j, ..., value = value)
  }

  y <- as_sym(xmat_mod)
  return(y)
}



convert_to_r_mat <- function(x) {
  ensure_sympy()
  
  xmat <- matrix(NA_character_, nrow = nrow(x), ncol = ncol(x))
  xlst <- x$pyobj$tolist()
  
  offset <- 0L # R lists do not change
  
  if (inherits(xlst, "python.builtin.list")) {
    offset <- 1L # A Python list has offset 0
  }
  
  for (i in seq_along(xlst)) {
    row_i <- xlst[[i - offset]] 
    
    for (j in seq_along(row_i)) {
      xmat[i, j] <- as.character(row_i[[j - offset]])
    }
  }
  
  return(xmat)
}

# convert_to_r_vec <- function(x) {
#   xmat <- vector(NA_character_, length = length(x))
#   xlst <- x$pyobj$tolist()
#   
#   for (i in seq_along(xlst)) {
#     row_i <- xlst[[i-1L]] # 0-based
#     
#     for (j in seq_along(row_i)) {
#       xmat[i, j] <- as.character(row_i[[j - 1L]]) # 0-based
#     }
#   }
#   
#   return(xmat)
# }
