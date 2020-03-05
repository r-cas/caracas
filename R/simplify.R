#' Simplify expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept caracas_symbol
#'
#' @export
simplify <- function(x) {
  UseMethod("simplify")
}

#' @export
simplify.caracas_symbol <- function(x) {
  ensure_sympy()
  
  z <- sympy$simplify(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}



#' Expand expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept caracas_symbol
#'
#' @export
expand <- function(x) {
  UseMethod("expand")
}

#' @export
expand.caracas_symbol <- function(x) {
  ensure_sympy()
  
  z <- sympy$expand(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}


#' Expand a trigonometric expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept caracas_symbol
#'
#' @export
expand_trig <- function(x) {
  UseMethod("expand_trig")
}

#' @export
expand_trig.caracas_symbol <- function(x) {
  ensure_sympy()
  
  z <- sympy$expand_trig(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}
