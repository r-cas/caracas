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
#' @concept caracas_symbol
#'
#' @export
sum.caracas_symbol <- function(..., na.rm = FALSE) {
  expr <- list(...)
  
  if (length(expr) != 1L) {
    stop("To be implemented")
  }
  
  expr <- expr[[1L]]
  expr_dim <- dim(expr)
  
  ones <- as_symbol(matrix(1, nrow = expr_dim[2L], ncol = expr_dim[1L]))
  z <- ones %*% expr
  z <- remove_mat_prefix(z)
  z <- gsub("^\\[\\[(.*)\\]\\]$", "\\1", z)
  z <- eval_to_symbol(z)
  
  return(z)
}
