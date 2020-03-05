verify_variable_name <- function(x) {
  if (length(x) != 1L) {
    stop("The name must have length 1")
  }
  
  if (!grepl("^[a-zA-Z]+[a-zA-Z_]*$", x)) {
    stop(paste0("'", x, "' is not a valid variable name"))
  }
}

construct_symbol_from_pyobj <- function(pyobj) {
  y <- list(pyobj = pyobj)
  class(y) <- "caracas_symbol"
  return(y)
}

eval_to_symbol <- function(x) {
  s <- reticulate::py_eval(x, convert = FALSE)
  y <- construct_symbol_from_pyobj(s)
  return(y)
}

#' Create a symbol
#'
#' @param x Name to turn into symbol
#'
#' @return A `caracas_symbol`
#'
#' @concept caracas_symbol
#' @importFrom reticulate py_run_string
#' @export
symbol <- function(x) {
  ensure_sympy()
  verify_variable_name(x)
  
  cmd <- paste0(x, " = symbols('", x, "')")
  # py_run_string instead of py_eval because we need to assign inside Python
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  
  y <- construct_symbol_from_pyobj(res)

  return(y)
}


#' Perform calculations setup previously
#'
#' @param x A `caracas_symbol`
#'
#' @concept caracas_symbol
#'
#' @export
doit <- function(x) {
  UseMethod("doit")
}

try_doit <- function(x) {
  if (!is.null(x$pyobj) && !is.null(x$pyobj$doit)) {
    y <- construct_symbol_from_pyobj(x$pyobj$doit())
    return(y)
  }
  
  return(x)
}

#' @export
doit.caracas_symbol <- function(x) {
  ensure_sympy()
  
  if (!is.null(x$pyobj) && !is.null(x$pyobj$doit)) {
    y <- construct_symbol_from_pyobj(x$pyobj$doit())
    return(y)
  }
  
  stop("Coult not doit()")
}

#' @export
c.caracas_symbol <- function(...) {
  ensure_sympy()
  
  # FIXME: To Python vector?
  #        In that case, see dd() too.
  x <- list(...)

  return(x)
}


