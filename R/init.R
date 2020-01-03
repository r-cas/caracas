# global reference to sympy (will be initialized in .onLoad)
sympy <- NULL

ensure_sympy <- function() {
  if (is.null(sympy)) {
    stop("sympy was not available: please run caracas::install_sympy()")
  }
}

#' Get 'SymPy' version
#' 
#' @examples 
#' sympy_version()
#'
#' @importFrom reticulate import
#' @export
sympy_version <- function() {
  ensure_sympy()
  
  sympy_version <- base::numeric_version(sympy$`__version__`)
  
  return(sympy_version)
}


#' @importFrom reticulate import py_run_string
load_sympy <- function() {
  # use superassignment to update global reference to sympy
  sympy <<- reticulate::import("sympy", delay_load = TRUE)
  
  if (sympy_version() < "1.4") {
    sympy <<- NULL
    message("sympy must be version at least 1.4. ", 
            "Please install using 'caracas::install_sympy()'")
  }
  
  reticulate::py_run_string("from sympy import *")
  reticulate::py_run_string("from sympy.parsing.sympy_parser import parse_expr")
}

#' @importFrom reticulate py_module_available import
.onLoad <- function(libname, pkgname) {
  have_sympy <- reticulate::py_module_available("sympy")
  
  if (!have_sympy) {
    message("sympy not available: please run caracas::install_sympy()")
  } else {
    load_sympy()
  }
}

#' @importFrom reticulate py_install py_module_available
#' @export
install_sympy <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sympy", method = method, conda = conda)
  message("sympy was installed. ", 
          "Please load caracas again. And have fun!")
}

#' Access 'SymPy' directly
#' 
#' Get the 'SymPy' object.  
#' Note that it gives you extra responsibilities
#' when you choose to access the 'SymPy' object directly.
#'
#' @examples 
#' sympy <- get_sympy()
#' sympy$solve("x**2-1", "x")
#' 
#' @export
get_sympy <- function() {
  return(sympy)
}
