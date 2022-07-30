#' Create list of factors as in a product
#'
#' @param ... factors
#' 
#' @export
as_factor_list <- function(...){
    lst <- list(...)
    out <- lapply(lst, as_sym)  
    class(out) <- "factor_list"
    out
}

#' Print factor list
#'
#' @param x factor list
#' @export
tex.factor_list <- function(x){
    a<- lapply(x, tex)  |> unlist()
    paste(a, collapse="  ")
}

#' Is object a caracas symbol
#' @param x factor list
#' @export
is_sym <- function(x){
    inherits(x, "caracas_symbol")
}



## #' Convert caracas object to R (col or row wise)
## #'
## #' Convert caracas object to R (col or row wise)
## #'
## #' @param x caracas_symbol
## #' @param first_doit Try `doit()` first
## #' @param column_major
## #' 
## #' @export
## as_expr2 <- function(x, first_doit = TRUE, column_major=TRUE) {
##   UseMethod("as_expr2")
## }

## #' @export
## as_expr2.default <- function(x, first_doit = TRUE, column_major=TRUE) {
##   return(x)
## }

## #' @export
## as_expr2.caracas_symbol <- function(x, first_doit = TRUE, column_major=TRUE) {
##     if (column_major && symbol_is_matrix(x)){
##         as_expr(t(x))        
##     } else
##     {
##         as_expr(x)        
##     }
## }





#' Get basis
#'
#' Get basis
#'
#' @param x caracas vector / matrix
#' @examples
#' if (has_sympy()) {
#' x <- vector_sym(3)
#' get_basis(x)
#' 
#' W <- matrix(c("r_1", "r_1", "r_2", "r_2", "0", "0", "u_1", "u_2"), nrow=4)
#' W <- as_sym(W)
#' get_basis(W)
#' }
#' @export
get_basis <- function(x){
    ensure_sympy()
    zz <- as_character_matrix(x)
    ##unique symbols
    us <- setdiff(unique(as.character(zz)), "0")
    out <- lapply(seq_along(us),
                  function(i){
                      1*(us[i] == zz)           
                  })
    names(out) <- us
    ## attr(out, "symbols") <- us
    out
}
