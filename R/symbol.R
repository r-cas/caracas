TXT_NOT_CARACAS_SYMBOL <- paste0("must be a caracas_symbol, ", 
                                 "e.g. constructed by symbol() ", 
                                 "followed by elementary operations")

PATTERN_PYHTON_VARIABLE <- "[a-zA-Z]+[a-zA-Z0-9_]*"

stopifnot_symbol <- function(x){
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
      # S(): Sympify
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




#' Creates matrix from array symbol
#' 
#' @param x Array symbol to convert to matrix
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol("x")
#'   y <- symbol("y")
#'   f <- 3*x^2 + x*y^2
#'   h <- der2(f, list(x, y))
#'   h
#'   dim(h)
#'   H <- matrify(h)
#'   H
#'   dim(H)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
matrify <- function(x) {
  z <- paste0("Matrix(", paste0(x, collapse = ", "), ")")
  y <- eval_to_symbol(z)
  return(y)
}

# Creates symbol vector from list of caracas symbols
vectorfy <- function(x) {
  z <- paste0(unlist(lapply(x, as.character)), collapse = ", ")
  z <- paste0("[", z, "]")
  y <- eval_to_symbol(z)
  return(y)
}


#' Remove inner-most dimension
#' 
#' @param x Array symbol to collapse dimension from
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym("[[[x1/(b2 + x1)], 
#'                  [x2/(b2 + x2)], 
#'                  [x3/(b2 + x3)]], 
#'                 [[-b1*x1/(b2 + x1)^2], 
#'                  [-b1*x2/(b2 + x2)^2], 
#'                  [-b1*x3/(b2 + x3)^2]]]")
#'   x
#'   unbracket(x)
#'   
#'   x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
#'   
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
unbracket <- function(x) {
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

#' Convert object to tuple
#' 
#' @param x Object
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
#'   tuplify(x)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
tuplify <- function(x) {
  zz <- extract_elements(x)
  zz <- paste0("(", zz, ")")
  
  y <- eval_to_symbol(zz)
  return(y)
}

#' Convert object to list of elements
#' 
#' @param x Object
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
#'   listify(x)
#'   
#'   xT <- t(x)
#'   listify(xT)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
listify <- function(x) {
  zz <- convert_to_r_mat(x)
  dim(zz) <- NULL
  zz <- as.list(zz)
  zz <- lapply(zz, as_sym, declare_symbols = FALSE)
  return(zz)
}

#' @export
c.caracas_symbol <- function(...) {
  ensure_sympy()
  
  # FIXME: To Python vector?
  #        In that case, see der() too.
  #x <- list(...)
  x <- vectorfy(list(...))
  
  # FIXME: Use? In that case ensure that all "[..., ...]" from elsewhere (e.g. der())
  #        is also caught.
  class(x) <- c("caracas_vector", class(x))

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
#' @seealso [subs_vec()], [subs_lst()]
#' 
#' @concept caracas_symbol
#' 
#' @export
subs <- function(s, x, v) {
  ensure_sympy()
  
  sym <- as.character(x)
  
  val <- if (inherits(v, "caracas_symbol")) {
    v$pyobj
  } else {
    v
  }
  
  y <- construct_symbol_from_pyobj(s$pyobj$subs(sym, val))
  return(y)
}



#' Substitute af vector of symbols for a vector of values
#' 
#' @param s Expression
#' @param x Names of symbol (vector)
#' @param v Values for `x` (vector)
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- as_sym(paste0('x', 1:3))
#'    e <- 2*x^2
#'    e
#'    subs_vec(e, x, 1:3)
#'    subs_vec(e, x, x^2)
#' }
#' 
#' @seealso [subs()], [subs_lst()]
#' @concept caracas_symbol
#' 
#' @export
subs_vec <- function(s, x, v) {
  ensure_sympy()

  if (is.character(x)){
      x <- as_sym(x)
  }
  if (is.character(v)){
      v <- as_sym(v)
  }
  
  stopifnot_matrix(x)
  
  vv <- v
  
  if (inherits(v, "caracas_symbol") && symbol_is_matrix(v)) {
    if (ncol(v) == 1L) {
      vv <- lapply(seq_len(nrow(v)), function(i) v[i, ])
    } else if (nrow(v) == 1L) {
      vv <- lapply(seq_len(ncol(v)), function(i) v[, i])
    } else {
      stop("When v is a caracas matrix, one dimension must be 1")
    }
  }
  
  if (nrow(x) != 1L && ncol(x) != 1L) {
    stop("x must have either 1 row or 1 column")
  }
  
  if (ncol(x) > 1L) {
    x <- t(x)
  }
  
  if (length(vv) != nrow(x)) {
    stop("Dimension mismatch")
  }
  
  ss <- s
  for (i in seq_along(vv)) {
    ss <- subs(ss, x[i, ], vv[[i]])
  }
  
  return(ss)
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
#' @seealso [subs()], [subs_vec()]
#' 
#' @concept caracas_symbol
#' 
#' @export
subs_lst <- function(s, x) {
  ensure_sympy()
  
  new_s <- s
  
  for (i in seq_along(x)) {
    new_s <- subs(new_s, names(x)[i], x[[i]])
  }
  
  return(new_s)
}

#' Get numerator and denominator of a fraction
#' 
#' @param x Fraction
#' 
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
  
  # See if x has fun method
  out <- tryCatch({
    p <- do.call(x$pyobj[[fun]], args)
    res <- construct_symbol_from_pyobj(p)
    res
  }, error = function(cond) {
    
    # ...it did not, try from global namespace:
    
    s <- get_sympy()
    
    args <- c(x$pyobj, args)
    
    p <- do.call(s[[fun]], args)
    res <- construct_symbol_from_pyobj(p)
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
#'   x <- (a-b)^4
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
#' @export
all_vars <- function(x){
  all.vars(as_expr(x))
}

