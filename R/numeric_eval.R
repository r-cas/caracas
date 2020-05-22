#' Numerical evaluation
#' 
#' @param x caracas object
#' @param digits Number of digits 
#' 
#' @examples 
#' if (have_sympy()) {
#'   n_2 <- as_symbol("2")
#'   n_pi <- as_symbol("pi", declare_variables = FALSE)
#'   x <- sqrt(n_2) * n_pi
#'   x
#'   N(x)
#'   N(x, 5)
#'   N(x, 50)
#'   as.character(N(x, 50))
#' }
#' 
#' @export
N <- function(x, digits = 15) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  xeval <- get_sympy()$N(x$pyobj, digits)
  
  y <- construct_symbol_from_pyobj(xeval)
  
  return(y)
}

