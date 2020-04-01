# sum
# prod
# length
# dim
# rbind
# cbind


#' Summation
#'
#' @param \dots Elements to sum
#' @param na.rm Not used
#'
#' @concept vectors
#'
#' @export
sum.caracas_symbol <- function(..., na.rm = FALSE) {
  expr <- list(...)
  
  if (length(expr) != 1L) {
    stop("To be implemented")
  }
  
  expr <- expr[[1L]]
  expr_dim <- dim(expr)
  
  if (is.null(expr_dim)) {
    return(expr)
  }
  
  if (!symbol_is_matrix(expr)) {
    stop("Unexpected")
  }
  
  ones1 <- as_symbol(matrix(1, nrow = 1L, ncol = expr_dim[1L]))
  z1 <- ones1 %*% expr
  
  ones2 <- as_symbol(matrix(1, nrow = expr_dim[2L], ncol = 1L))
  z2 <- z1 %*% ones2
  
  z <- remove_mat_prefix(z2)
  z <- gsub("^\\[\\[(.*)\\]\\]$", "\\1", z)
  z <- eval_to_symbol(z)
  
  return(z)
}


#' @export
cbind.caracas_symbol <- function(..., deparse.level = 1) {
  expr <- list(...)
  
  z <- lapply(expr, as_character_matrix)
  z <- do.call(cbind, z)
  z <- as_symbol(z)
  
  return(z)
}

#' @export
rbind.caracas_symbol <- function(..., deparse.level = 1) {
  expr <- list(...)
  
  z <- lapply(expr, as_character_matrix)
  z <- do.call(rbind, z)
  z <- as_symbol(z)
  
  return(z)
}
