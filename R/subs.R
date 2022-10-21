update_sym <- function(sym, nms, vls, declare_symbols = TRUE){
  if (!is.character(nms)) stop("nms must be character - use as.char() first")
  if (!is.character(vls)) stop("vls must be character - use as.char() first")
  
  if (declare_symbols) {
    declare_symbols_worker(nms)
    
    varnames <- extract_vars(vls)
    declare_symbols_worker(varnames)
  }
  
  dict <- paste0("{", paste0(nms, ": ", caracas:::r_strings_to_python(vls), collapse=", "), "}")
  #print(dict)
  e <- reticulate::py_eval(dict)
  construct_symbol_from_pyobj(sym$pyobj$subs(e))
}

# Gives character vector (i.e. no dimensions)
any_to_char <- function(x, ...){ ## JUST A WRAPPER
  if (!inherits(x, "caracas_symbol")){
    return(as.character(x))
  }
  switch(symbol_class(x),
         "atomic"={
           as.character(x)
         },
         "matrix"={
           c(as_character_matrix(x))
         },           
         "vector"={
           gsub(" *", "", c(as_character_matrix(x)))
         },
         "list"={
           cat("NOT IMPLEMENTED\n")
         },
         cat("DON'T KNOW WHAT TO DO\n")
  )
}


list_to_nms_vls <- function(nms){
  if (!inherits(nms, "list")){
    stop("'nms' is not a list\n")
  }
  
  vls_ <- unname(sapply(nms, as.character))
  nms_ <- names(nms)
  if (any(nchar(nms_) == 0)){
    stop("'nms' is a list but not properly named\n")
  }
  list(nms=nms_, vls=vls_)
}


################################################################################


#' Substitute symbol for value
#' 
#' @param sym Expression
#' @param nms Names of symbols (see Details)
#' @param vls Values that `nms` is substituted with (see Details)
#' 
#' @details Two different ways to call this function is supported:
#' 1) Supplying `nms` as a named list and omitting `vls`. 
#'    If two components have the same name, the behaviour is undefined.
#' 2) Supplying both `nms` and `vls`
#' See Examples.
#' 
#' @examples 
#' if (has_sympy()) {
#'    x <- symbol('x')
#'    e <- 2*x^2
#'    e
#'    subs(e, "x", "2")
#'    subs(e, x, 2)
#'    subs(e, list(x = 2))
#'    
#'    A <- matrix_sym(2, 2, "a")
#'    B <- matrix_sym(2, 2, "b")
#'    e <- A %*% A
#'    subs(e, A, B)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
subs <- function(sym, nms, vls){
  # subs() from named list
  if (inherits(nms, "list")){
    if (!missing(vls)) {
      warning("vls ignored")
    }
    
    args <- list_to_nms_vls(nms)
    sym2 <- update_sym(sym, args$nms, args$vls)
    return(sym2)
  }
  
  # subs() with both nms and vls
  nms_ <- any_to_char(nms)
  vls_ <- any_to_char(vls)
  sym2 <- update_sym(sym, nms_, vls_)
  return(sym2)
}
