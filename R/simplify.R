#' Simplify expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
simplify <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  z <- get_sympy()$simplify(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}



#' Expand expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
expand <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  z <- get_sympy()$expand(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}


#' Expand a trigonometric expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
expand_trig <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  z <- get_sympy()$expand_trig(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}



#' Expand a logarithmic expression
#' 
#' Note that `force` as described at 
#' <https://docs.sympy.org/latest/tutorial/simplification.html#expand-log> is used 
#' meaning that some assumptions are taken.
#' 
#' @param x A `caracas_symbol`
#' 
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol('x')
#'   y <- symbol('y')
#'   z <- log(x*y)
#'   z
#'   expand_log(z)
#' }
#' 
#' @concept simplify
#' 
#' @export
expand_log <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  z <- get_sympy()$expand_log(x$pyobj, force = TRUE)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}

