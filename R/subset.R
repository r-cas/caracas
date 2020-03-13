convert_to_r_mat <- function(x) {
  xmat <- matrix(NA_character_, nrow = nrow(x), ncol = ncol(x))
  xlst <- x$pyobj$tolist()
  
  for (i in seq_along(xlst)) {
    row_i <- xlst[[i-1L]] # 0-based
    
    for (j in seq_along(row_i)) {
      xmat[i, j] <- as.character(row_i[[j - 1L]]) # 0-based
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

#' Extract or replace parts of an object
#' 
#' @param x A `caracas_symbol`.
#' @param i row indices specifying elements to extract or replace
#' @param j column indices specifying elements to extract or replace
#' @param \dots Not used
#' @param drop Simplify dimensions of resulting object
#' 
#' @examples 
#' if (have_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_symbol(A)
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
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  #if (!symbol_is_matrix(x) && !symbol_is_vector(x)) {
  #  stop("'x' must be a vector or matrix")
  #}
  
  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xmat <- convert_to_r_mat(x)
  xmat_subset <- base::`[`(xmat, i, j, ..., drop = drop)
  y <- as_symbol(xmat_subset)
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
#' if (have_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_symbol(A)
#'   B[, 2] <- "x"
#'   B
#' }
#' 
#' @concept vectors
#' 
#' @export
`[<-.caracas_symbol` <- function(x, i, j, ..., value) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }

  if (!symbol_is_matrix(x)) {
    stop("'x' must be a matrix")
  }
  
  xmat <- convert_to_r_mat(x)
  xmat_mod <- base::`[<-`(xmat, i, j, ..., value = value)
  y <- as_symbol(xmat_mod)
  return(y)
}