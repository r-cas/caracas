
################################################################################


#' Substitute symbols in an expression
#'
#' Perform symbolic substitution in a \code{caracas_symbol} object.
#'
#' @param sym A \code{caracas_symbol} expression (scalar, vector, matrix, ...).
#' @param nms Symbols to replace. Either:
#' \itemize{
#'   \item a named list \code{list(name = value, ...)} (recommended), or
#'   \item a character / \code{caracas_symbol} vector of symbols.
#' }
#' @param vls Replacement values (only used when \code{nms} is not a named list).
#'
#' @details
#' Two calling styles are supported:
#' \enumerate{
#' \item \strong{Mapping as a named list:} Supply \code{nms} as a named list and omit \code{vls}.
#'       Names are the symbols to be replaced; list elements are the replacement values.
#' \item \strong{Parallel vectors:} Supply both \code{nms} and \code{vls} of the same length.
#' }
#'
#' Replacements may be numbers, character strings, \code{caracas_symbol} objects,
#' or other objects that \pkg{caracas} can convert to SymPy.
#'
#' If the same symbol is provided more than once, the result depends on evaluation order
#' in the backend and should be considered undefined.
#'
#' @return A \code{caracas_symbol} object with substitutions applied.
#'
#' @examples
#' if (has_sympy()) {
#'   x <- symbol("x")
#'   e <- 2*x^2
#'   e
#'
#'   subs(e, "x", "2")
#'   subs(e, x, 2)
#'   subs(e, list(x = 2))
#'
#'   A <- matrix_sym(2, 2, "a")
#'   B <- matrix_sym(2, 2, "b")
#'   e2 <- A %*% A
#'   subs(e2, A, B)
#' }
#'
#'
#'

#' @concept caracas_symbol
#' 
#' @export
subs <- function(sym, nms, vls) {
    ensure_sympy()
    stopifnot_symbol(sym)

    ## FIXME sorenh: is this good
    ## Must catch case where nms is a character vector
    if (is.vector(nms) && !is.null(names(nms))){
        nms <- as.list(nms)
    }
    
    ## subs() from named list
    if (inherits(nms, "list")) {
        if (!missing(vls)) {
            warning("vls ignored")
        }
        
        args <- list_to_nms_vls(nms)
        sym2 <- update_sym(sym, args$nms, args$vls)
        return(sym2)
    }
    
    ## subs() with both nms and vls
    nms_ <- any_to_char(nms)
    vls_ <- any_to_char(vls)
    ## str(list(nms_=nms_, vls_=vls_))
    sym2 <- update_sym(sym, nms_, vls_)
    return(sym2)
}




#' Substitute symbols (expression interface)
#'
#' Convenience wrapper for symbolic substitution in \code{caracas_symbol} objects.
#'
#' @param sym A \code{caracas_symbol} expression.
#' @param old Symbols to replace (character or \code{caracas_symbol}).
#' @param new Replacement value(s) corresponding to \code{old}.
## #' @param ... Passed on to the backend (if applicable).
#'
#' @return A \code{caracas_symbol} object with substitutions applied.
#'
#' @examples
#' if (has_sympy()) {
#'   x <- symbol("x"); y <- symbol("y")
#'   e <- (x + 1)^2 + y
#'
#'   # Named arguments:
#'   subs_expr(e, x = 2)
#'
#'   # Named list:
#'   subs_expr(e, list(x = 2, y = 10))
#'
#'   # Works with matrices too:
#'   A <- matrix_sym(2, 2, "a")
#'   B <- matrix_sym(2, 2, "b")
#'   subs_expr(A %*% A, A = B)
#' }
#'
#' @concept caracas_symbol
#' @export
#' 
subs_expr <- function(sym, old, new) {
  ensure_sympy()
  stopifnot_symbol(sym)

  sympy <- get_sympy()

  to_py_expr <- function(x) {
    if (inherits(x, "caracas_symbol")) {
      sympy$sympify(as.character(x))
    } else {
      sympy$sympify(any_to_char(x))
    }
  }

  from_py_expr <- function(py_obj, template = sym) {
    out <- as_sym(as.character(py_obj))
    d <- dim(template)
    if (!is.null(d)) dim(out) <- d
    out
  }

  py_sym <- to_py_expr(sym)

  # Multiple substitutions:
  # 1) named list: list("pattern1" = replacement1, "pattern2" = replacement2, ...)
  if (inherits(old, "list") && missing(new)) {
    # Named list: patterns are names(old)
    if (!is.null(names(old)) && any(nzchar(names(old)))) {
      py_res <- py_sym
      for (nm in names(old)) {
        py_old <- sympy$sympify(nm)     # pattern as string
        py_new <- to_py_expr(old[[nm]])
        py_res <- py_res$subs(py_old, py_new)
      }
      return(from_py_expr(py_res, sym))
    }

    # 2) list of pairs: list(list(old1,new1), list(old2,new2), ...)
    py_res <- py_sym
    for (pair in old) {
      stopifnot(is.list(pair), length(pair) == 2)
      py_old <- to_py_expr(pair[[1]])
      py_new <- to_py_expr(pair[[2]])
      py_res <- py_res$subs(py_old, py_new)
    }
    return(from_py_expr(py_res, sym))
  }

  # Single substitution old -> new
  py_old <- to_py_expr(old)
  py_new <- to_py_expr(new)

  from_py_expr(py_sym$subs(py_old, py_new), sym)
}





## ' Substitute into an expression (expression-friendly)
## '
## ' Convenience wrapper around \code{\link{subs}} for the common workflow:
## ' start from an expression, provide a mapping of symbol names to values,
## ' and obtain the substituted symbolic expression.
## '
## ' @param expr An expression (typically a \code{caracas_symbol}) in which symbols are substituted.
## '
## ' @param ... Substitution specification. You can use either:
## '
## ' \itemize{
## '   \item named arguments, e.g. \code{subs_expr(expr, x = 2, y = 3)}, or
## '   \item a single named list, e.g. \code{subs_expr(expr, list(x = 2, y = 3))}.
## ' }
## ' @param .env Optional environment used to look up values when using named arguments
## '        that evaluate to objects (advanced usage). Default is \code{parent.frame()}.
## '
## ' @details
## ' \code{subs_expr()} is intended as a user-facing convenience function.
## ' Compared to \code{\link{subs}}, it focuses on a lightweight interface where
## ' replacements are given as a mapping from names to values.
## '
## ' The function does \emph{symbolic} substitution; it does not necessarily evaluate
## ' the full expression numerically unless the backend simplifies to a numeric result.
## #'


## #' @concept caracas_symbol
## #' @export
## subs <- function(sym, nms, vls = NULL, ...) {
##   # implementation...
## }


## #' Substitute symbol for value
## #' 
## #' @param sym Expression
## #' @param nms Names of symbols (see Details)
## #' @param vls Values that `nms` is substituted with (see Details)
## #' 
## #' @details Two different ways to call this function is supported:
## #' 1) Supplying `nms` as a named list and omitting `vls`. 
## #'    If two components have the same name, the behaviour is undefined.
## #' 2) Supplying both `nms` and `vls`
## #' See Examples.
## #' 
## #' @examples 
## #' if (has_sympy()) {
## #'    x <- symbol('x')
## #'    e <- 2*x^2
## #'    e
## #'    subs(e, "x", "2")
## #'    subs(e, x, 2)
## #'    subs(e, list(x = 2))
## #'    
## #'    A <- matrix_sym(2, 2, "a")
## #'    B <- matrix_sym(2, 2, "b")
## #'    e <- A %*% A
## #'    subs(e, A, B)
## #' }
## #' 






update_sym <- function(sym, nms, vls, declare_symbols = TRUE) {
    if (!is.character(nms)) stop("nms must be character - use as.char() first")
    if (!is.character(vls)) stop("vls must be character - use as.char() first")
  
    if (declare_symbols) {
        declare_symbols_worker(nms)
        
        varnames <- extract_vars(vls)
        declare_symbols_worker(varnames)
    }
    
    dict <- paste0("{", paste0(nms, ": ", r_strings_to_python(vls), collapse=", "), "}")
    ## print(dict)
    e <- reticulate::py_eval(dict)
    construct_symbol_from_pyobj(sym$pyobj$subs(e))
}

# Gives character vector (i.e. no dimensions)
any_to_char <- function(x, ...) { ## JUST A WRAPPER
  if (!inherits(x, "caracas_symbol")) {
    return(as.character(x))
  }
  switch(symbol_class(x),
         "atomic" = {
           as.character(x)
         },
         "matrix" = {
           c(as_character_matrix(x))
         },           
         "vector" = {
           gsub(" *", "", c(as_character_matrix(x)))
         },
         "list" = {
           cat("NOT IMPLEMENTED\n")
         },
         cat("DON'T KNOW WHAT TO DO\n")
  )
}


list_to_nms_vls <- function(nms) {
  if (!inherits(nms, "list")) {
    stop("'nms' is not a list\n")
  }
  
  vls_ <- unname(sapply(nms, as.character))
  nms_ <- names(nms)
  if (any(nchar(nms_) == 0)) {
    stop("'nms' is a list but not properly named\n")
  }
  list(nms=nms_, vls=vls_)
}



## FIXME : sorenh need example 
#' Substitute symbol for value
#' 
#' @param sym_list Expression
#' @param nms_list Names of symbols (see Details)
#' @param vls_list Values that `nms` is substituted with (see Details)
#'
#' @export
subs_list <- function(sym_list, nms_list, vls_list) {
    if (is_sym(sym_list))
        sym_list <- listify(sym_list)
    if (is_sym(nms_list))
        nms_list <- listify(nms_list)
    if (!is.null(dim(vls_list)))
        vls_list  <- byrow(vls_list)
    
    ## list(sym_list, nms_list, vls_list) |> lapply(print)
    mapply(function(sym, nms, vls){
        ## list(sym, nms, vls) |> lapply(print)
        subs(sym, nms, vls)
    }, sym_list, nms_list, vls_list,
    SIMPLIFY = FALSE)}


## FIXME better name
## FIXME export?

bycol <- function(x) {
    lapply(seq_len(ncol(x)), function(i) x[,i])
}

byrow <- function(x) {
    lapply(seq_len(nrow(x)), function(i) x[i,])
}
