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
  
  bnd <- as.character(b)
  #bnd <- deparse(eval(substitute(substitute(b)), parent.frame()))
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
#'   limf(sin(x)/x, "x", 0)
#'   limf(1/x, "x", 0, dir = '+')
#'   limf(1/x, "x", 0, dir = '-')
#' }
#' 
#' @concept calculus
#' 
#' @export
limf <- function(f, var, val, dir = NULL, doit = TRUE) {
  calc_verify_func(f)
  ensure_sympy()
  var <- as.character(var)
  verify_variable_name(var)
  
  val_str <- bound_to_str(val)
  
  y <- if (!is.null(dir) && length(dir) == 1L && dir %in% c('+', '-')) {
    if (doit) {
      get_sympy()$limit(f$pyobj, var, val_str, dir)
    } else {
      get_sympy()$Limit(f$pyobj, var, val_str, dir)
    }
  } else {
    if (doit) {
      get_sympy()$limit(f$pyobj, var, val_str)
    } else {
      get_sympy()$Limit(f$pyobj, var, val_str)
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
#'   as_expr(s)
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
    get_sympy()$summation(f$pyobj, c(var, lwr_str, upr_str))
  } else {
    get_sympy()$Sum(f$pyobj, c(var, lwr_str, upr_str))
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
#'   as_expr(p)
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
    get_sympy()$product(f$pyobj, c(var, lwr_str, upr_str))
  } else {
    get_sympy()$Product(f$pyobj, c(var, lwr_str, upr_str))
  }
  
  z <- construct_symbol_from_pyobj(y)
  
  return(z)
}

#' Integrate a function
#' 
#' If no limits are provided, the 
#' indefinite integral is calculated. 
#' Otherwise, if both limits are provided, 
#' the definite integral is calculated.
#'
#' @param f Function to integrate
#' @param var Variable to integrate with respect to (either string or `caracas_symbol`)
#' @param lower Lower limit
#' @param upper Upper limit
#' @param doit Evaluate the integral immediately (or later with [doit()])
#'
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   
#'   intf(1/x, x, 1, 10)
#'   intf(1/x, x, 1, 10, doit = FALSE)
#'   intf(1/x, x)
#'   intf(1/x, x, doit = FALSE)
#'   intf(exp(-x^2/2), x, -Inf, Inf)
#'   intf(exp(-x^2/2), x, -Inf, Inf, doit = FALSE)
#' }
#' 
#' @concept calculus
#' 
#' @export
intf <- function(f, var, lower, upper, doit = TRUE) {
  calc_verify_func(f)
  ensure_sympy()
  var <- as.character(var)
  verify_variable_name(var)
  
  with_limits <- FALSE
  
  if (missing(lower) && missing(upper)) {
  } else if (!missing(lower) && !missing(upper)) {
    with_limits <- TRUE
  } else {
    stop("Either both lower and upper must be given or neither must be given")
  }
  
  if (with_limits) {
    lwr_str <- bound_to_str(lower)
    upr_str <- bound_to_str(upper)
    
    y <- if (doit) {
      get_sympy()$integrate(f$pyobj, c(var, lwr_str, upr_str))
    } else {
      get_sympy()$Integral(f$pyobj, c(var, lwr_str, upr_str))
    }
    
    z <- construct_symbol_from_pyobj(y)
    
    return(z)
  }
  
  # No limits:
  
  # Potentially replacing existing symbol:
  var_symb <- reticulate::py_eval(paste0("symbols('", var, "')"), convert = FALSE)
  
  y <- if (doit) {
    get_sympy()$integrate(f$pyobj, var_symb)
  } else {
    get_sympy()$Integral(f$pyobj, var_symb)
  }
  
  z <- construct_symbol_from_pyobj(y)
  
  return(z)
}

# expr: caracas symbol
# vars: array of symbols (potentially just 1)
der_worker <- function(expr, vars) {
  # vars <- vars_to_array(vars)
  ensure_sympy()
  stopifnot(inherits(vars, "caracas_symbol"))
  
  multi_var <- grepl("^\\[", as.character(vars))
  
  if (!multi_var) {
    z <- get_sympy()$diff(expr$pyobj, vars$pyobj)
    v <- construct_symbol_from_pyobj(z)
    return(v)
  }
  
  z <- get_sympy()$derive_by_array(expr$pyobj, vars$pyobj)
  v <- construct_symbol_from_pyobj(z)
  return(v)
}

# Convert vars to array; vars can be many different things...
vars_to_array <- function(vars) {
  # vars should already be in expr, so
  # they are known as symbols on the Python side
  
  vars_chr <- if (is.character(vars)) {
    # character vector
    vars
  } else if (inherits(vars, "caracas_symbol")) {
    # A single caracas_symbol
    as.vector(as_character_matrix(vars))
  } else if (inherits(vars, "list")) {
    # A list of caracas_symbols
    for (i in seq_along(vars)) {
      v <- vars[[i]]
      
      if (!inherits(v, "caracas_symbol")) {
        stop("Element in vars had wrong type (expected caracas_symbol)")
      }
    }
    
    unlist(lapply(vars, function(l) as.vector(as_character_matrix(l))))
  } else {
    stop("Unexpected vars type")
  }
  
  if (length(vars_chr) == 1L) {
    v <- eval_to_symbol(vars_chr)
    return(v)
  }
  
  # > 1 variable:
  v <- eval_to_symbol(paste0("[", paste0(vars_chr, collapse = ", "), "]"))
  return(v)
}

#' Symbolic differentiation of an expression
#'
#' @param expr A `caracas_symbol`
#' @param vars variables to take derivate with respect to
#'
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   y <- symbol("y")
#'   f <- 3*x^2 + x*y^2
#'   der(f, x)
#' }
#' 
#' @concept calculus
#'
#' @export
der <- function(expr, vars) {
  ensure_sympy()
  
  new_vars <- vars_to_array(vars)
  d <- der_worker(expr, new_vars)
  
  return(d)
}

#' Symbolic differentiation of second order of an expression
#'
#' @param expr A `caracas_symbol`
#' @param vars variables to take derivate with respect to
#' 
#' @examples 
#' if (have_sympy()) {
#'   x <- symbol("x")
#'   y <- symbol("y")
#'   f <- 3*x^2 + x*y^2
#'   der2(f, x)
#' }
#' 
#' @concept calculus
#'
#' @export
der2 <- function(expr, vars) {
  ensure_sympy()
  
  d1 <- der(expr, vars)
  d2 <- der(d1, vars)
  
  return(d2)
}

