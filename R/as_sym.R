# FIXME:
# Functionality built into symbol() directly?

hlp_to_py_mat <- function(x) {
  z <- paste0("[", apply(x, 1, paste0, collapse = ", "), "]")
  z <- paste0("Matrix([", paste0(z, collapse = ", "), "])")
  
  return(z)
}

hlp_to_py_vec <- function(x) {
  # Column
  z <- paste0("Matrix([", paste0(x, collapse = ", "), "])")
  
  # Row:
  #z <- paste0(x, collapse = ", ")
  #z <- paste0("Matrix([[", paste0(z, collapse = ", "), "]])")
  
  return(z)
}

# Scalar
hlp_to_py_scalar <- function(x) {
  z <- as.character(x)
  
  return(z)
}

as_py_string <- function(x) {
  if (is.matrix(x)) {
    return(hlp_to_py_mat(x))
  }
  
  if (is.vector(x) && length(x) == 1L) {
    return(hlp_to_py_scalar(x))
  }
  
  return(hlp_to_py_vec(x))
}

#' Convert R object to caracas symbol
#' 
#' Variables are detected as a
#' character followed by a number of either: 
#' character, number or underscore.
#' 
#' Default is to declare used variables. Alternatively, the user 
#' must declare them first, e.g. by [symbol()].
#' 
#' Note that matrices can be defined by specifying a Python matrix, 
#' see below in examples.
#' 
#' @param x R object to convert to a symbol
#' @param declare_symbols declare detected symbols automatically
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol("x")
#'   A <- matrix(c("x", 0, 0, "2*x"), 2, 2)
#'   A
#'   B <- as_sym(A)
#'   B
#'   2 * B
#'   dim(B)
#'   sqrt(B)
#'   D <- as_sym("[[1, 4, 5], [-5, 8, 9]]")
#'   D
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
as_sym <- function(x, 
                   declare_symbols = TRUE) {
  ensure_sympy()

  if (is_sym(x)){ 
      return(x)
  }
  
  if (is.expression(x)) {
    x <- as.character(x)
  }
  
  varnames_exclude <- c("S", "sqrt", "log", "I", "exp", "sin", "cos", "Matrix", "Function")

  if (declare_symbols) {
    xele <- as.vector(x)
    m <- gregexpr(pattern = PATTERN_PYHTON_VARIABLE, 
                  text = xele)
    varnames <- regmatches(x = xele, m = m, invert = FALSE)
    varnames <- unique(unlist(varnames[unlist(lapply(varnames, length)) > 0]))
    varnames <- setdiff(varnames, varnames_exclude)
    #varnames
    
    for (varname in varnames) {
      cmd <- paste0(varname, " = symbols('", varname, "')")
      reticulate::py_run_string(cmd, convert = FALSE)
    }
  }

  # Defining a matrix by hand with '[[1], [2]]' syntax
  if (is.character(x) && length(x) == 1L && grepl("^\\[\\[", x)) {
      ## cat("Create matrix\n")
      x <- paste0("Matrix(", r_strings_to_python(x), ")")
      y <- eval_to_symbol(x)
      return(y)    
  } 
  
  # else 
  cmd <- as_py_string(x)
  y <- eval_to_symbol(cmd)
  return(y)
}



#' Is object a caracas symbol
#' @param x factor list
#' @export
is_sym <- function(x){
    inherits(x, "caracas_symbol")
}
