# devtools::check_win()

# global reference to sympy (will be initialized in .onLoad)
sympy <- NULL

ensure_sympy <- function() {
  if (is.null(sympy)) {
    stop("'SymPy' was not available")
  }
}

#' Check if 'SymPy' is available
#' 
#' @importFrom reticulate py_module_available
#' @export
have_sympy <- function() {
  # ~/.local/share/r-miniconda/envs/r-reticulate/lib/python3.6/site-packages
  return(reticulate::py_module_available("sympy"))
}

#' Get 'SymPy' version
#' 
#' @examples 
#' \dontrun{
#' if (have_sympy()) {
#'   sympy_version()
#' }
#' }
#'
#' @importFrom reticulate import
#' @export
sympy_version <- function() {
  ensure_sympy()
  
  sympy_version <- base::numeric_version(sympy$`__version__`)
  
  return(sympy_version)
}


#' @importFrom reticulate configure_environment import py_run_string
.onLoad <- function(libname, pkgname) {
  reticulate::configure_environment(pkgname)
  
  if (have_sympy()) {
    local_sympy <- reticulate::import("sympy", delay_load = TRUE)
    
    if (base::numeric_version(local_sympy$`__version__`) >= "1.4") {
      # All okay:
      
      sympy <<- local_sympy # update global reference
      
      #reticulate::py_run_string("from sympy import *")
      #reticulate::py_run_string("from sympy.parsing.sympy_parser import parse_expr")
    } 
    
    # else handled in .onAttach()
  }
}

.onAttach <- function(libname, pkgname) {
  if (!is.null(sympy)) {
    return() # SymPy loaded
  }
  
  if (!have_sympy()) {
    packageStartupMessage("'SymPy' not available")
    return()
  } 
  
  # else: wrong version:
  packageStartupMessage("'SymPy' version >= 1.4 not available")
}

#' Access 'SymPy' directly
#' 
#' Get the 'SymPy' object.  
#' Note that it gives you extra responsibilities
#' when you choose to access the 'SymPy' object directly.
#'
#' @examples 
#' \dontrun{
#' if (have_sympy()) {
#'   sympy <- get_sympy()
#'   sympy$solve("x**2-1", "x")
#' }
#' }
#' 
#' @export
get_sympy <- function() {
  return(sympy)
}

