## FIXME:
## Concept is simplifcation
## Need seealso's..
## Can some entries be combined?
## See https://docs.sympy.org/latest/tutorials/intro-tutorial/simplification.html

#' Simplify expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
simplify <- function(x) {
  stopifnot_symbol(x)
  ensure_sympy()
  
  z <- get_sympy()$simplify(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  attributes(v) <- attributes(x) ## FIXME check this
  return(v)
}

#' Collects common powers of a term in an expression
#'
#' @param x,a A `caracas_symbol`
#' @examples 
#' if (has_sympy()){
#'   def_sym(x, y, z)
#'   expr = x*y + x - 3 + 2*x**2 - z*x**2 + x**3
#'   collect(expr, x)
#' }

#' @concept simplify
#'
#' @export
collect <- function(x, a) {
  stopifnot_symbol(x)
  ensure_sympy()
  sympy_func(x, "collect", a)
}

#' Put rational function into standard form
#' 
#' cancel() will take any rational function and put it into the standard canonical form
#'
#' @param x A `caracas_symbol`
#' @examples 
#' if (has_sympy()){
#'   def_sym(x, y, z)
#'   expr = cancel((x**2 + 2*x + 1)/(x**2 + x))
#'   cancel(expr)
#'   expr = (x*y**2 - 2*x*y*z + x*z**2 + y**2 - 2*y*z + z**2)/(x**2 - 1)
#'   cancel(expr)
#'   factor_(expr)
#' }

#' @concept simplify
#'
#' @export
cancel <- function(x) {
  stopifnot_symbol(x)
  ensure_sympy()
  sympy_func(x, "cancel")
}


#' Partial fraction decomposition on a rational function
#' 
#' apart() performs a partial fraction decomposition on a rational function
#'
#' @param x A `caracas_symbol`
#' @examples 
#' if (has_sympy()){
#'   def_sym(x)
#'   expr = (4*x**3 + 21*x**2 + 10*x + 12)/(x**4 + 5*x**3 + 5*x**2 + 4*x)
#'   apart(expr)
#' }
#' @concept simplify
#'
#' @export
apart <- function(x) {
  stopifnot_symbol(x)
  ensure_sympy()
  sympy_func(x, "apart")
}






#' Expand expression
#'
#' @param x A `caracas_symbol`
#' @param \dots Pass on to SymPy's expand, e.g. `force = TRUE`
#' 
#' @examples
#' if (has_sympy()) {
#'   def_sym(x)
#'   y <- log(exp(x))
#'   simplify(y)
#'   expand(simplify(y))
#'   expand(simplify(y), force = TRUE)
#'   expand_log(simplify(y))
#' }
#' 
#' @concept simplify
#'
#' @importFrom Matrix expand
#' @method expand caracas_symbol
#' @export
#' @rdname simplify
setMethod(
  "expand",
  signature(x = "caracas_symbol"),
  function(x, ...) {
      ensure_sympy()
      stopifnot_symbol(x)      
      z <- get_sympy()$expand(x$pyobj, ...)
      v <- construct_symbol_from_pyobj(z)
      return(v)
  }
)


## expand <- function(x, ...) {
##   stopifnot_symbol(x)
##   ensure_sympy()
  
##   z <- get_sympy()$expand(x$pyobj, ...)
##   v <- construct_symbol_from_pyobj(z)
##   return(v)
## }




## FIXME: There is also a factor_list() function
#' Expand expression
#' 
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#' @examples 
#' 
#' if (has_sympy()){
#'   def_sym(x, y, z)
#'   factor_(x**3 - x**2 + x - 1)
#'   factor_(x**2*z + 4*x*y*z + 4*y**2*z)
#' }
#'
#' @export
factor_ <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  sympy_func(x, "factor")
}



#' Expand a trigonometric expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
expand_trig <- function(x) {
    stopifnot_symbol(x)
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
#' if (has_sympy()) {
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
    stopifnot_symbol(x)      
    ensure_sympy()
    
    z <- get_sympy()$expand_log(x$pyobj, force = TRUE)
    v <- construct_symbol_from_pyobj(z)
    
    return(v)
}

#' Expand a function expression
#'
#' @param x A `caracas_symbol`
#'
#' @concept simplify
#'
#' @export
expand_func <- function(x) {
  stopifnot_symbol(x)
  ensure_sympy()
  
  z <- get_sympy()$expand_func(x$pyobj)
  v <- construct_symbol_from_pyobj(z)
  
  return(v)
}
