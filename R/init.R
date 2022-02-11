
# devtools::check_win_devel()
# devtools::check_rhub() (rhub::check_for_cran())

# reticulate::py_module_available("sympy")
# reticulate::miniconda_update()


# reticulate::conda_remove('r-reticulate')
# reticulate::use_python('/usr/bin/python3')
# reticulate::conda_create('r-reticulate')
### reticulate::use_condaenv('r-reticulate', required = TRUE)
# install_sympy(method = "conda")

# global reference to sympy
pkg_globals <- new.env()
pkg_globals$internal_py <- NULL
pkg_globals$internal_sympy <- NULL
pkg_globals$internal_sympy_version <- NULL
pkg_globals$internal_globals_length <- NULL


define_printers <- function() {
  fl <- system.file("define_printers.py", package = "caracas")
  reticulate::py_run_file(fl)
  
  if (!grepl("UTF-8", Sys.getlocale())) {
    # Not UTF-8 system:
    options(caracas.print.prettyascii = TRUE)
  }
}


#' @importFrom reticulate import py_run_string py_module_available
silent_prepare_sympy <- function() {
  if (!is.null(pkg_globals$internal_sympy)) {
    return()
  }
  
  py_ver <- tryCatch({
    cfg <- reticulate::py_config()
    base::numeric_version(cfg$version)
  }, error = function(e) {
    base::numeric_version("1")
  })
  
  if (py_ver >= "3" && reticulate::py_module_available("sympy")) {
    local_sympy <- reticulate::import("sympy")
    
    if (base::numeric_version(local_sympy$`__version__`) >= "1.4") {
      # All okay:
      
      pkg_globals$internal_py <- reticulate::py # update global reference
      pkg_globals$internal_sympy <- local_sympy # update global reference
      
      reticulate::py_run_string("from sympy import *")
      
      pkg_globals$internal_sympy_version <- base::numeric_version(pkg_globals$internal_sympy$`__version__`)

      define_printers()
    } 
    
    # # Save number of existing symbols to later being able to discard those first n items
    # glb_syms <- reticulate::py_eval("globals()")
    # pkg_globals$internal_globals_length <- length(glb_syms)
    
    # else handled in .onAttach()
  }
}

ensure_sympy <- function() {
  silent_prepare_sympy()
  
  if (is.null(pkg_globals$internal_sympy)) {
    stop("Both Python3 and 'SymPy' >= 1.4 must be available.\n", 
         "Please verify Python version with 'reticulate::py_config()'.\n", 
         "Remember to configure reticulate (e.g. 'reticulate::use_condaenv(\"anaconda3\")') before loading caracas.\n",
         "To install SymPy, please run this command:\n", 
         "caracas::install_sympy()")
  }
}

#' Check if 'SymPy' is available
#' 
#' @return `TRUE` if 'SymPy' is available, else `FALSE`
#' 
#' @examples 
#' has_sympy()
#' 
#' @concept sympy
#' 
#' @export
has_sympy <- function() {
  silent_prepare_sympy()
  
  return(!is.null(pkg_globals$internal_sympy))
}

#' Get 'SymPy' version
#' 
#' @return The version of the 'SymPy' available
#' 
#' @examples 
#' if (has_sympy()) {
#'   sympy_version()
#' }
#' 
#' @concept sympy
#'
#' @importFrom reticulate import
#' @export
sympy_version <- function() {
  ensure_sympy()
  
  return(pkg_globals$internal_sympy_version)
}

#' Access 'py' object
#' 
#' Get the 'py' object.  
#' Note that it gives you extra responsibilities
#' when you choose to access the 'py' object directly.
#'
#' @return The 'py' object with direct access to the library.
#'
#' @examples 
#' if (has_sympy()) {
#'   py <- get_py()
#' }
#' 
#' @concept sympy
#' 
#' @export
get_py <- function() {
  ensure_sympy()
  
  return(pkg_globals$internal_py)
}

#' Access 'SymPy' directly
#' 
#' Get the 'SymPy' object.  
#' Note that it gives you extra responsibilities
#' when you choose to access the 'SymPy' object directly.
#'
#' @return The 'SymPy' object with direct access to the library.
#'
#' @examples 
#' if (has_sympy()) {
#'   sympy <- get_sympy()
#'   sympy$solve("x**2-1", "x")
#' }
#' 
#' @concept sympy
#' 
#' @export
get_sympy <- function() {
  ensure_sympy()
  
  return(pkg_globals$internal_sympy)
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
#' @return None
#' 
#' @concept sympy
#' 
#' @importFrom reticulate py_install
#' @export
install_sympy <- function(method = "auto", conda = "auto") {
  reticulate::py_install("sympy", method = method, conda = conda)
  message("Please check output above to verify that 'SymPy' was installed correctly. ", 
          "If so, please have fun!", 
          "\n\nIf for some reason it does not work, try updating conda\n", 
          "with this R command:\nreticulate::miniconda_update()")
  return(invisible(NULL))
}


