TXT_NOT_CARACAS_SYMBOL <- paste0("must be a caracas_symbol, ", 
                                 "e.g. constructed by symbol() ", 
                                 "followed by elementary operations")

PATTERN_PYHTON_VARIABLE <- "[a-zA-Z]+[a-zA-Z0-9_]*"

stopifnot_symbol <- function(x) {
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
}

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
#'    lim(sin(x)/x, "x", 0)
#' }
#' 
#' @return A `caracas_symbol`
#'
#' @concept lowlevel
#' @importFrom reticulate py_eval
#' @export
eval_to_symbol <- function(x) {
  ensure_sympy()
  
  # --------------------------------------------
  # 1/3 should be caught
  # --------------------------------------------
  # https://docs.sympy.org/latest/gotchas.html#python-numbers-vs-sympy-numbers
  # y1/3 should not be caught
  if (grepl("[0-9-.]+/[0-9-.]+", x, perl = TRUE)) {
    # Now there is a fraction that looks like '1/3'; 
    # we need to be sure that there are no characters in front of the 
    # number in the numerator
    
    if (grepl("[a-zA-Z_]+[0-9-.]+/[0-9-.]+", x, perl = TRUE)) {
      # There was a character (e.g. 'x1/2') 
      # or a subscript/underscore (e.g. 3*y_11/4)
      # do nothing
    } else {
      # S(): Sympify
      x <- gsub("([0-9-.]+)/([0-9-.]+)", "S(\\1)/S(\\2)", x, perl = TRUE)
    }
  }
  
  # --------------------------------------------
  # (1)/(3) should be caught  
  # --------------------------------------------
  # https://docs.sympy.org/latest/gotchas.html#python-numbers-vs-sympy-numbers
  if (grepl("\\([0-9-.]+\\) */ *\\([0-9-.]+\\)", x, perl = TRUE)) {
    # Now there is a fraction that looks like '(1)/(3)'; 
    # S(): Sympify
    # This gives S(1)/S(2), okay with no extra parentheses 
    x <- gsub("\\(([0-9-.]+)\\) */ *\\(([0-9-.]+)\\)", "S(\\1)/S(\\2)", x, perl = TRUE)
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
    arg_val[unlist(lapply(dots, function(l) isTRUE(l)))] <- "True"
    # isFALSE req. R >= 3.5, hence explicit:
    arg_val[unlist(lapply(dots, function(l) is.logical(l) && length(l) == 1L && !is.na(l) && !l))] <- "False"
    
    extra_cmd <- paste0(arg_nm, " = ", arg_val, collapse = ", ")
    extra_cmd <- paste0(", ", extra_cmd)
  }
  
  cmd <- paste0(x, " = symbols('", x, "'", extra_cmd, ")")
  
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
#' @examples 
#' if (has_sympy()) {
#'    x <- symbol('x')
#'    res <- lim(sin(x)/x, "x", 0, doit = FALSE)
#'    res 
#'    doit(res)
#' }
#'
#' @concept caracas_symbol
#'
#' @export
doit <- function(x) {
  stopifnot_symbol(x)
  
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







#' Remove inner-most dimension
#' 
#' @param x Array symbol to collapse dimension from
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym(paste0("x", 1:3))
#'   y <- as_sym("y")
#'   l <- list(x, y)
#'   l
#'   unbracket(l)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
unbracket <- function(x) {
  if (!inherits(x, "caracas_symbol") && is.list(x)) {
    z <- lapply(x, as_character)
    z <- do.call(rbind, z)
    z <- as_sym(z)
    #z <- to_vector(z)
    return(z)
  }
  
  z <- as.character(x)

  zz <- gsub("\\[([^]]+)\\]", "\\1", z)
  zz
  
  y <- eval_to_symbol(zz)
  return(y)
}

extract_elements <- function(x) {
  z <- as.character(x)
  
  zz <- gsub("[", "", z, fixed = TRUE)
  zz <- gsub("]", "", zz, fixed = TRUE)
  zz <- remove_mat_prefix(zz)
  
  return(zz)
}



## concatenate

#' @export
c.caracas_symbol <- function(...) {
  ensure_sympy()
  
  # v <- vector_sym(2)
  # c(v)
  # c(v, v)
  # A <- matrix_sym(3, 4)
  # c(A)
  # c(A, A)
  z <- list(...)
  z <- lapply(z, as_character)
  z <- unlist(z)
  z <- base::c(z)
  z <- as_sym(z)
  return(z)
  
  # # FIXME: To Python vector?
  # #        In that case, see der() too.
  # #x <- list(...)
  # x <- vectorfy(list(...))
  # 
  # # FIXME: Use? In that case ensure that all "[..., ...]" from elsewhere (e.g. der())
  # #        is also caught.
  # class(x) <- c("caracas_vector", class(x))
  # 
  # return(x)
}


#' Get numerator and denominator of a fraction
#' 
#' @param x Fraction
#'
#' @name fraction_parts
#' @examples 
#' if (has_sympy()) {
#'      x <- as_sym("a/b")
#'      frac <- fraction_parts(x)
#'      frac
#'      frac$numerator
#'      frac$denominator
#'  }
#' 
#' @concept caracas_symbol
#' 
#' @export
fraction_parts <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  
  frac <- x$pyobj$as_numer_denom()
  
  y <- list(
    numerator = construct_symbol_from_pyobj(frac[0]), # Python 0-indexed
    denominator = construct_symbol_from_pyobj(frac[1])
  )
  
  return(y)
}


#' @export
#' @rdname fraction_parts
numerator <- function(x) {
    return(fraction_parts(x)$numerator)
}

#' @export
#' @rdname fraction_parts
denominator <- function(x) {
    return(fraction_parts(x)$denominator)
}


#' Call a SymPy function directly on x
#'
#' Extend caracas by calling SymPy functions directly.
#' 
#' @param x Object to call `fun` on
#' @param fun Function to call
#' @param \dots Passed on to `fun`
#' 
#' @examples 
#' if (has_sympy()) {
#'   def_sym(x, a)
#'   p <- (x-a)^4
#'   p
#'   q <- p %>% sympy_func("expand")
#'   q
#'   q %>% sympy_func("factor")
#'   
#'   def_sym(x, y, z)
#'   expr <- x*y + x - 3 + 2*x^2 - z*x^2 + x^3
#'   expr
#'   expr %>% sympy_func("collect", x) 
#'   
#'   x <- symbol("x")
#'   y <- gamma(x+3)
#'   sympy_func(y, "expand_func")
#'   expand_func(y)
#' }
#'  
#' @concept caracas_symbol
#' 
#' @export
sympy_func <- function(x, fun, ...) {
  args <- list(...)
  
  args <- lapply(args, function(a) {
    if (inherits(a, "caracas_symbol")) {
      return(as.character(a))
    }
    
    return(a)
  })
  
  convert_res <- function(o) {
    o <- if (is.list(o)) {
      lapply(o, construct_symbol_from_pyobj)
    } else {
      construct_symbol_from_pyobj(o)
    }
    
    o
  }
  
  # See if x has fun method
  out <- tryCatch({
      p <- do.call(x$pyobj[[fun]], args)
      #res <- construct_symbol_from_pyobj(p)
      res <- convert_res(p)
      res
  }, error = function(cond) {
      
    # ...it did not, try from global namespace:    
    s <- get_sympy()
    args <- c(x$pyobj, args)
    p <- do.call(s[[fun]], args)
    
    #print(p)
    
    #res <- construct_symbol_from_pyobj(p)
    res <- convert_res(p)
    return(res)
  })
  
  return(out)
}


#' Get free symbol in expression
#' 
#' @param x Expression in which to get the free symbols in
#' 
#' @examples 
#' if (has_sympy()) {
#'   def_sym(a, b)
#'   x <- (a - b)^4
#'   free_symbols(x)
#' }
#'  
#' @concept caracas_symbol
#' 
#' @export
free_symbols <- function(x) {
  y <- x$pyobj$free_symbols
  z <- reticulate::py_eval(paste0("list(", as.character(y), ")"), convert = TRUE)
  z <- lapply(z, construct_symbol_from_pyobj)
  return(z)
}

#' All variables
#'
#' Return all variables in caracas symbol
#'
#' @param x caracas symbol
#'
#' @examples
#' if (has_sympy()){
#'   x <- vector_sym(5)
#'   all_vars(x)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
all_vars <- function(x) {
  all.vars(as_expr(x)) 
}


#' Coerce symbol to character
#'
#' Coerce symbol to character
#' @param x caracas symbol
#' 
#' @concept caracas_symbol
#'
#' @export
as_character <- function(x) {
    ensure_sympy()
    stopifnot_symbol(x)
    
    switch(symbol_class(x),
           "matrix" = {
               as_character_matrix(x)
           },         
           "vector" = {
               c(as_character_matrix(x))
           },
           "atomic" = {
               as.character(x)
           }
  )
}



