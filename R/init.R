# devtools::check_win_devel()
# devtools::check_rhub() (rhub::check_for_cran())
# reticulate::conda_remove('r-reticulate')
# reticulate::py_module_available("sympy")

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
    packageStartupMessage("'SymPy' not available, please run this command:\ncaracas::install_sympy()")
    return()
  } 
  
  # else: wrong version:
  packageStartupMessage("Only 'SymPy' version < 1.4 available, and version >= 1.4 is needed.\n", 
                        "Please run this command:\ncaracas::install_sympy()")
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

#' Install 'SymPy'
#' 
#' Install the 'SymPy' Python package into a 
#' virtual environment or Conda environment.
#' 
#' @param method Installation method. 
#' By default, "auto" automatically finds a method that will work 
#' in the local environment. 
#' Change the default to force a specific installation method. 
#' Note that the "virtualenv" method is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda 
#' using the PATH and other conventional install locations).
#' 
#' @importFrom reticulate py_install py_module_available
#' @export
install_sympy <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sympy", method = method, conda = conda)
  message("Please check output above to verify that 'SymPy' was installed correctly. ", 
          "If so, please load 'caracas' again. And have fun!", 
          "\n\nIf for some reason it still does not work, try updating conda\n", 
          "with this R command:\nreticulate::miniconda_update()")
  
}
