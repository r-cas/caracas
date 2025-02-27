
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
subs <- function(sym, nms, vls) {
    ensure_sympy()
    stopifnot_symbol(sym)

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
