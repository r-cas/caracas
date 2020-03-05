bound_to_str <- function(b) {
  if (is.infinite(b)) {
    if (b > 0) {
      return("oo")
    } else {
      return("-oo")
    }
  }
  
  if (is.character(b)) {
    return(b)
  }
  
  bnd <- deparse(eval(substitute(substitute(b)), parent.frame()))
  #bnd <- gsub("pi", "Pi", bnd, fixed = TRUE)
  
  return(bnd)
}


#' Limits
#'
#' @param f Function to take limit of
#' @param var Variable to take limit for
#' @param val Value for `var` to approach
#' @param dir Direction from where `var` should approach `val`: `'+'` or `'-'`
#' @param doit Evaluate the limit immediately (or later with [doit()])
#' @param \dots Not used.
#'
#' @examples 
#' x <- symbol("x")
#' lim(sin(x)/x, "x", 0)
#' lim(1/x, "x", 0, dir = '+')
#' lim(1/x, "x", 0, dir = '-')
#' 
#' @concept caracas_symbol
#' 
#' @export
lim <- function(f, var, val, dir, doit = TRUE, ...) {
  if (!inherits(f, "caracas_symbol")) {
    stop("f must be a caracas_symbol")
  }
  
  ensure_sympy()
  
  dots <- list(...)
  
  val_str <- bound_to_str(val)
  
  y <- if (!is.null(dots$dir) && length(dots$dir) == 1L && dots$dir %in% c('+', '-')) {
    if (doit) {
      sympy$limit(f$pyobj, var, val_str, dots$dir)
    } else {
      sympy$Limit(f$pyobj, var, val_str, dots$dir)
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


# TODO
sumf <- function(f) {
  
}

# TODO
prodf <- function(f) {
  
}

# TODO
int <- function(f) {
  
}




#' Symbolic differentiation of an expression
#'
#' @param expr A `caracas_symbol`
#' @param vars variables to take derivate with respect to
#'
#' @concept caracas_symbol
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
#' @concept caracas_symbol
#'
#' @export
dd2 <- function(expr, vars) {
  ensure_sympy()
  
  d1 <- dd(expr, vars)
  d2 <- dd(d1, vars)
  
  return(d2)
}

