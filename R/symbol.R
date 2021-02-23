TXT_NOT_CARACAS_SYMBOL <- paste0("must be a caracas_symbol, ", 
                                 "e.g. constructed by symbol() ", 
                                 "followed by elementary operations")

PATTERN_PYHTON_VARIABLE <- "[a-zA-Z]+[a-zA-Z0-9_]*"

verify_variable_name <- function(x) {
  if (length(x) != 1L) {
    stop("The name must have length 1")
  }
  
  pattern <- paste0("^", PATTERN_PYHTON_VARIABLE, "$")
  
  if (!grepl(pattern, x)) {
    stop(paste0("'", x, "' is not a valid variable name"))
  }
}

construct_symbol_from_pyobj <- function(pyobj) {
  y <- list(pyobj = pyobj)
  class(y) <- "caracas_symbol"
  return(y)
}

#' Create a symbol from a string
#'
#' @param x String to evaluate
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- symbol('x')
#'    (1+1)*x^2
#'    limf(sin(x)/x, "x", 0)
#' }
#' 
#' @return A `caracas_symbol`
#'
#' @concept lowlevel
#' @importFrom reticulate py_eval
#' @export
eval_to_symbol <- function(x) {
  ensure_sympy()
  
  # https://docs.sympy.org/latest/gotchas.html#python-numbers-vs-sympy-numbers
  # 1/3 should be caught
  # y1/3 should not be caught
  if (grepl("[0-9-.]+/[0-9-.]+", x, perl = TRUE)) {
    # Not there is a fraction that looks like '1/3'; 
    # we need to be sure that there are no characters in front of the 
    # number in the numerator
    
    if (grepl("[a-zA-Z]+[0-9-.]+/[0-9-.]+", x, perl = TRUE)) {
      # There was a character (e.g. 'x1/2'), do nothing
    } else {
      x <- gsub("([0-9-.]+)/([0-9-.]+)", "S(\\1)/S(\\2)", x, perl = TRUE)
    }
  }
  
  x <- r_strings_to_python(x)
  s <- reticulate::py_eval(x, convert = FALSE)
  y <- construct_symbol_from_pyobj(s)
  return(y)
}

#' Create a symbol
#' 
#' Find available assumptions at 
#' <https://docs.sympy.org/latest/modules/core.html#module-sympy.core.assumptions>.
#'
#' @param x Name to turn into symbol
#' @param \dots Assumptions like `positive = TRUE`
#'
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol("x")
#'   2*x
#'   
#'   x <- symbol("x", positive = TRUE)
#'   ask(x, "positive")
#' }
#' 
#' @return A `caracas_symbol`
#'
#' @seealso [as_sym()]
#' @concept caracas_symbol
#' @importFrom reticulate py_run_string
#' @export
symbol <- function(x, ...) {
  ensure_sympy()
  verify_variable_name(x)
  
  dots <- list(...)
  
  extra_cmd <- ""
  if (length(dots) > 0L) {
    arg_nm <- names(dots)
    
    arg_val <- rep("None", length(dots))
    arg_val[unlist(lapply(dots, function(l) is.logical(l) && isTRUE(l)))] <- "True"
    arg_val[unlist(lapply(dots, function(l) is.logical(l) && isFALSE(l)))] <- "False"
    
    extra_cmd <- paste0(arg_nm, " = ", arg_val, collapse = ", ")
  }
  
  cmd <- paste0(x, " = symbols('", x, "', ", extra_cmd, ")")
  
  # py_run_string instead of py_eval because we need to assign inside Python
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  
  y <- construct_symbol_from_pyobj(res)

  return(y)
}

is_atomic <- function(x) {
  xstr <- as.character(x)
  
  pattern <- paste0("^", PATTERN_PYHTON_VARIABLE, "$")
  
  return(grepl(pattern, x))
}

#' Perform calculations setup previously
#'
#' @param x A `caracas_symbol`
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- symbol('x')
#'    res <- limf(sin(x)/x, "x", 0, doit = FALSE)
#'    res 
#'    doit(res)
#' }
#'
#' @concept caracas_symbol
#'
#' @export
doit <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
  
  ensure_sympy()
  
  if (!is.null(x$pyobj) && !is.null(x$pyobj$doit)) {
    y <- construct_symbol_from_pyobj(x$pyobj$doit())
    return(y)
  }
  
  stop("Could not doit()")
}

try_doit <- function(x) {
  # if (!is.null(x$pyobj) && "doit" %in% names(x$pyobj)) {
  #   y <- construct_symbol_from_pyobj(x$pyobj$doit())
  #   return(y)
  # }
  
  try({
    y <- construct_symbol_from_pyobj(x$pyobj$doit())
    return(y)
  }, silent = TRUE)

  return(x)
}


#' @export
c.caracas_symbol <- function(...) {
  ensure_sympy()
  
  # FIXME: To Python vector?
  #        In that case, see der() too.
  x <- list(...)

  return(x)
}


#' Substitute symbol for value
#' 
#' @param s Expression
#' @param x Name of symbol (character)
#' @param v Value for `x`
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- symbol('x')
#'    e <- 2*x^2
#'    e
#'    subs(e, "x", "2")
#'    y <- as_sym("2")
#'    subs(e, "x", y)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
subs <- function(s, x, v) {
  
  sym <- as.character(x)
  
  val <- if (inherits(v, "caracas_symbol")) {
    v$pyobj
  } else {
    v
  }
  
  y <- construct_symbol_from_pyobj(s$pyobj$subs(sym, val))
  return(y)
}

#' Substitute symbol for of value given by a list
#' 
#' Useful for substituting solutions into expressions.
#' 
#' @param s Expression
#' @param x Named list of values
#' 
#' @examples 
#' if (has_sympy()) {
#'      p <- as_sym(paste0("p", 1:3))
#'      y <- as_sym(paste0("y", 1:3))
#'      a <- as_sym("a")
#'      l <- sum(y*log(p))
#'      L <- -l + a*(sum(p) - 1)
#'      g <- der(L, c(a, p))
#'      sols <- solve_sys(g, c(a, p))
#'      sol <- sols[[1L]]
#'      sol
#'      H <- der2(L, c(p, a))
#'      H
#'      H_sol <- subs_lst(H, sol)
#'      H_sol
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
subs_lst <- function(s, x) {
  new_s <- s
  
  for (i in seq_along(x)) {
    new_s <- subs(new_s, names(x)[i], x[[i]])
  }
  
  return(new_s)
}

