bound_to_str <- function(b) {
  if (is.character(b)) {
    return(b)
  }
  
  if (inherits(b, "caracas_symbol")) {
    return(as.character(b))
  }
  
  if (is.infinite(b)) {
    if (b > 0) {
      return("oo")
    } else {
      return("-oo")
    }
  }
  
  bnd <- deparse(eval(substitute(substitute(b)), parent.frame()))
  #bnd <- gsub("pi", "Pi", bnd, fixed = TRUE)
  
  return(bnd)
}


calc_verify_func <- function(f) {
  if (!inherits(f, "caracas_symbol")) {
    stop(paste0("'f' ", TXT_NOT_CARACAS_SYMBOL))
  }
}

#' Limit of a function
#'
#' @param f Function to take limit of
#' @param var Variable to take limit for (either string or `caracas_symbol`)
#' @param val Value for `var` to approach
#' @param dir Direction from where `var` should approach `val`: `'+'` or `'-'`
#' @param doit Evaluate the limit immediately (or later with [doit()])
#'
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   lim(sin(x)/x, "x", 0)
#'   lim(1/x, "x", 0, dir = '+')
#'   lim(1/x, "x", 0, dir = '-')
#' }
#' 
#' @concept calculus
#' 
#' @export
lim <- function(f, var, val, dir = NULL, doit = TRUE) {
  calc_verify_func(f)
  ensure_sympy()
  var <- as.character(var)
  verify_variable_name(var)
  
  val_str <- bound_to_str(val)
  
  y <- if (!is.null(dir) && length(dir) == 1L && dir %in% c('+', '-')) {
    if (doit) {
      sympy$limit(f$pyobj, var, val_str, dir)
    } else {
      sympy$Limit(f$pyobj, var, val_str, dir)
    }
  } else {
    if (doit) {
      sympy$limit(f$pyobj, var, val_str)
    } else {
      sympy$Limit(f$pyobj, var, val_str)
    }
  }
  
  z <- construct_symbol_from_pyobj(y)
  
  return(z)
}





#' Sum of a function
#'
#' @param f Function to take sum of
#' @param var Variable to take sum for (either string or `caracas_symbol`)
#' @param lower Lower limit
#' @param upper Upper limit
#' @param doit Evaluate the sum immediately (or later with [doit()])
#'
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   s <- sumf(1/x, "x", 1, 10)
#'   as_r(s)
#'   sum(1/(1:10))
#'   n <- symbol("n")
#'   simplify(sumf(x, x, 1, n))
#' }
#' 
#' @concept calculus
#' 
#' @export
sumf <- function(f, var, lower, upper, doit = TRUE) {
  calc_verify_func(f)
  ensure_sympy()
  var <- as.character(var)
  verify_variable_name(var)
  
  lwr_str <- bound_to_str(lower)
  upr_str <- bound_to_str(upper)
  
  y <- if (doit) {
    sympy$summation(f$pyobj, c(var, lwr_str, upr_str))
  } else {
    sympy$Sum(f$pyobj, c(var, lwr_str, upr_str))
  }
  
  z <- construct_symbol_from_pyobj(y)
  
  return(z)
}

#' Product of a function
#'
#' @param f Function to take product of
#' @param var Variable to take product for (either string or `caracas_symbol`)
#' @param lower Lower limit
#' @param upper Upper limit
#' @param doit Evaluate the product immediately (or later with [doit()])
#'
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   p <- prodf(1/x, "x", 1, 10)
#'   p
#'   as_r(p)
#'   prod(1/(1:10))
#'   n <- symbol("n")
#'   prodf(x, x, 1, n)
#' }
#' 
#' @concept calculus
#' 
#' @export
prodf <- function(f, var, lower, upper, doit = TRUE) {
  calc_verify_func(f)
  ensure_sympy()
  var <- as.character(var)
  verify_variable_name(var)
  
  lwr_str <- bound_to_str(lower)
  upr_str <- bound_to_str(upper)
  
  y <- if (doit) {
    sympy$product(f$pyobj, c(var, lwr_str, upr_str))
  } else {
    sympy$Product(f$pyobj, c(var, lwr_str, upr_str))
  }
  
  z <- construct_symbol_from_pyobj(y)
  
  return(z)
}

# TODO
int <- function(f) {
  
}




#' Symbolic differentiation of an expression
#'
#' @param expr A `caracas_symbol`
#' @param vars variables to take derivate with respect to
#'
#' @concept calculus
#'
#' @export
dd <- function(expr, vars) {
  ensure_sympy()
  
  py_vars <- if (is.character(vars)) {
    # vars a character vector
    lapply(vars, function(v) symbol(v)$pyobj)
  } else if (inherits(vars, "caracas_symbol")) {
    # vars actually just a single caracas_symbol
    vars
  } else {
    #vars a list of caracas_symbols
    lapply(vars, function(v) v$pyobj)
  }

  # vars should already be in expr, so
  # they are known as symbols on the Python side
  
  if (length(py_vars) == 1L) {
    z <- sympy$diff(expr$pyobj, py_vars[[1L]])
    v <- construct_symbol_from_pyobj(z)
    return(v)
  } else {
    z <- sympy$derive_by_array(expr$pyobj, py_vars)
    v <- construct_symbol_from_pyobj(z)
    return(v)
  }
}

#' Symbolic differentiation of second order of an expression
#'
#' @param expr A `caracas_symbol`
#' @param vars variables to take derivate with respect to
#'
#' @concept calculus
#'
#' @export
dd2 <- function(expr, vars) {
  ensure_sympy()
  
  d1 <- dd(expr, vars)
  d2 <- dd(d1, vars)
  
  return(d2)
}

