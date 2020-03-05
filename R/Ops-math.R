get_pyobj <- function(e, method) {
  o <- if (method == "") {
    reticulate::py_eval(as.character(e), convert = FALSE)
  } else {
    e$pyobj
  }
  
  return(o)
}

#' Math operators
#'
#' @param e1 A `caracas_symbol`.
#' @param e2 A `caracas_symbol`.
#'
#' @export
Ops.caracas_symbol = function(e1, e2) {
  if (!(.Generic %in% c("+", "-", "*", "/", "^"))) {
    stop("Function '", .Generic, "' not yet implemented for caracas_symbol")
  }
  
  # E.g. -4 etc...
  if (missing(e2)) {
    o1 <- get_pyobj(e1, .Method[1L])
    txt <- paste0(.Generic, "(", as.character(o1), ")")
    s <- eval_to_symbol(txt)
    return(s)
  }

  # Both e1 and e2 given:

  # LHS may be constant, e.g. 2*x: e1 is a number 2
  o1 <- get_pyobj(e1, .Method[1L])
  
  # RHS may be constant, e.g. x*2: e2 is a number 2
  o2 <- get_pyobj(e2, .Method[2L])
  
  op <- if (.Generic == "^") {
    "**"
  } else {
    .Generic
  }
  
  cmd <- paste0("(", as.character(o1), ")", op, 
                "(", as.character(o2), ")")
  
  x <- eval_to_symbol(cmd)

  return(x)
}

Math_transtab <- matrix( c(
  #R					Python
  "sin",			"Sin",
  "cos",			"Cos",
  "tan",			"Tan",

  "asin",	  	"ArcSin",
  "acos",	  	"ArcCos",
  "atan",    	"ArcTan",
  "asinh", 	  "ArcSinh",
  "acosh", 	  "ArcCosh",
  "atanh",   	"ArcTanh",

  "exp", 	  	"Exp",
  "log", 	  	"Ln",
  "sqrt", 	  "Sqrt"
), byrow = TRUE, ncol = 2)
colnames(Math_transtab) <- c("R", "Python")
#paste0(Math_transtab[, 1], "()", collapse = ", ")

#' Math functions
#'
#' @param x `caracas_symbol`.
#' @param \dots further arguments passed to methods
#'
#' @export
Math.caracas_symbol = function(x, ...) {
  ensure_sympy()

  i <- match(.Generic, Math_transtab[, 1L])

  if (is.na(i)) {
    stop("Function '", .Generic, "' not yet implemented for caracas_symbol")
  }

  fn <- Math_transtab[i, 1L]

  if (is.null(sympy[fn])) {
    stop(paste0("Could not find function '", fn, "' in Python"))
  }

  y <- sympy[fn](x$pyobj)
  z <- construct_symbol_from_pyobj(y)

  return(z)
}
