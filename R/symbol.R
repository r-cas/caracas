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
    numerator = construct_symbol_from_pyobj(frac[0]),
    denominator = construct_symbol_from_pyobj(frac[1])
  )
  
  return(y)
}
























#' Create matrix symbol
#' 
#' @param x Name of matrix symbol
#' @param nrow Number of rows
#' @param ncol Number of columns
#' 
#' @examples 
#' if (has_sympy()) {
#'    A <- matrix_symbol("A", 3, 3)
#'    A
#'    dim(A)
#'    t(A)
#'    inv(A)
#' }
#' 
#' @seealso [sympy_declare()]
#' 
#' @concept caracas_symbol
#' 
#' @export
matrix_symbol <- function(x, nrow = 1L, ncol = 1L) {
  ensure_sympy()
  verify_variable_name(x)
  
  cmd <- paste0(x, " = MatrixSymbol('", x, "', ", nrow, ", ", ncol, ")")
  s <- reticulate::py_run_string(cmd, convert = FALSE)
  res <- s[[x]]
  y <- construct_symbol_from_pyobj(res)
  
  return(y)
}




#' Declare symbol manually by Python syntax
#' 
#' @param x Name of symbol
#' @param cmd Command (SymPy/Python syntax)
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- sympy_declare("x", "Symbol('x')")
#'    x
#'    
#'    x <- sympy_declare("x", "Symbol('x', positive = True)")
#'    x
#'    ask(x, "positive")
#'    
#'    A <- matrix_symbol("A", 3, 3) # or: sympy_declare("A", "MatrixSymbol('A', 3, 3)")
#'    A
#'    dim(A)
#'    t(A)
#'    inv(A)
#'    
#'    A <- matrix_symbol("A", "MatrixSymbol('A', 3, 3)")
#'    O <- sympy_declare("O", "ZeroMatrix(3, 3)")
#'    A %*% O
#' }
#' 
#' @seealso [matrix_symbol()]
#' 
#' @concept caracas_symbol
#' 
#' @export
sympy_declare <- function(x, cmd) {
  caracas:::ensure_sympy()
  
  python_cmd <- paste0(x, " = ", cmd)
  s <- reticulate::py_run_string(python_cmd, convert = FALSE)
  res <- s[[x]]
  y <- caracas:::construct_symbol_from_pyobj(res)
  
  return(y)
}





#' Inverse of 2x2 matrix
#' 
#' Finds the inverse of
#' ```
#' [A  B]
#' [C  D]
#' ```
#' 
#' Arguments by row.
#' 
#' @param x
#' 
#' 
#' @examples 
#' if (has_sympy()) {
#'    W <- matrix_symbol("W") # 1 x 1 by default
#'    J <- sympy_declare("J", "Identity(1)") # Instead of I to avoid confusion with imaginary unit
#'    O <- sympy_declare("O", "ZeroMatrix(1, 1)")
#'    L <- matrix(c("J", "-W", "0", "J"), nrow = 2) %>%
#'      as_sym(declare_symbols = FALSE)
#'    L
#'    #inv(L): SymPy cannot figure it out...
#'    Linv <- inv_2x2(L)
#'    Linv
#'    
#'    t(Linv) %*% Linv
#'    O2 <- sympy_declare("O", "ZeroMatrix(2, 2)")
#'    A <- O2 %*% t(Linv) %*% Linv
#'    A
#'    dim(A)
#' }
#' 
#' @return Columns of result. 
#' The reason it is not a matrix is because SymPy has problems as 'I' both means ide
#' 
#' @concept caracas_symbol
#' 
#' @export
inv_2x2 <- function(x) {
  # https://en.wikipedia.org/wiki/Block_matrix#Block_matrix_inversion
  
  ensure_sympy()
  
  d <- dim(x)
  
  if (is.null(d) || length(d) != 2L || d[1L] != 2L || d[2L] != 2L) {
    stop("'x' must be a 2x2 matrix")
  }
  
  Ap <- x$pyobj[0]
  Aip <- Ap$inverse()
  Bp <- x$pyobj[1]
  Cp <- x$pyobj[2]
  Dp <- x$pyobj[3]

  s <- get_sympy()
  
  mult3 <- function(x, y, z) {
    s$MatMul(x, y, z)
  }

  mult5 <- function(x1, x2, x3, x4, x5) {
    s$MatMul(x1, x2, x3, x4, x5)
  }
  
  add <- function(x, y) {
    s$MatAdd(x, y)
  }
  
  neg_mat <- function(z) {
    reticulate::py_eval(paste0("-", as.character(z)))
  }

  CAiB <- mult3(Cp, Aip, Bp)
  nCAiB <- neg_mat(CAiB)
  DnCAiB <- add(Dp, nCAiB)
  DnCAiBinv <- DnCAiB$inverse()
  
  e11 <- add(Aip, mult5(Aip, Bp, DnCAiBinv, Cp, Aip))
  e12 <- neg_mat(mult3(Aip, Bp, DnCAiBinv))
  e21 <- neg_mat(mult3(DnCAiBinv, Cp, Aip))
  e22 <- DnCAiBinv
  
  e11 <- e11$doit()
  e12 <- e12$doit()
  e21 <- e21$doit()
  e22 <- e22$doit()
  
  
  elements <- list(e11, e12, e21, e22)
  elements_str <- lapply(seq_along(elements), function(i) {
    e <- elements[[i]]
    
    if (inherits(e, "sympy.matrices.expressions.special.Identity")) {
      if (as.character(e$rows) != as.character(e$cols)) {
        stop("Unexpected")
      }
      
      return(paste0("Identity(", as.character(e$rows), ")"))
    }

    else if (inherits(e, "sympy.core.numbers.Zero")) {
      return(paste0("ZeroMatrix(1, 1)"))
    }
    
    else if (inherits(e, "sympy.matrices.expressions.special.ZeroMatrix")) {
      return(paste0("ZeroMatrix(", as.character(e$rows), ",", 
                    as.character(e$cols), ")"))
    }
    
    return(as.character(e))
  })
  
  y <- paste0("BlockMatrix([[",
              elements_str[[1L]], ", ",
              elements_str[[2L]],
              "], [",
              elements_str[[3L]], ", ",
              elements_str[[4L]], "]])")
  z <- reticulate::py_eval(y, convert = FALSE)
  w <- construct_symbol_from_pyobj(z)
  return(w)
  
#   e11 <- construct_symbol_from_pyobj(e11)
#   e12 <- construct_symbol_from_pyobj(e12)
#   e21 <- construct_symbol_from_pyobj(e21)
#   e22 <- construct_symbol_from_pyobj(e22)
#   
#   # Columns
#   return(list(
#     list(e11, e21),
#     list(e12, e22))
#   )
}
