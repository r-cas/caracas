#' Compute Jacobian
#'
#' @param expr 'caracas expression'.
#' @param vars variables to take derivative with respect to 
#'
#' @examples
#' if (has_sympy()) {
#'  x <- paste0("x", seq_len(3))
#'  def_sym_vec(x)
#'  y1 <- x1 + x2
#'  y2 <- x1^2 + x3
#'  y <- c(y1, y2)
#'  jacobian(y, x)
#'  u <- 2 + 4*x1^2
#'  jacobian(u, x1)
#' }
#' @export
jacobian <- function(expr, vars){
    out <- der(expr, vars)
    out <- matrify(out)
    out <- t(out)
    return(out)
}

#' Define symbol for components in vector
#'
#' @param x Character vector.
#' @param x The environment in which the assignment is made.
#' @examples
#' if (has_sympy()) {
#'   def_sym(z1, z2, z3)
#'   u <- paste0("u", seq_len(3))
#'   ## Creates symbols u1, u2, u3 and binds to names u1, u2, u3 in R.
#'   def_sym_vec(u) 
#'   ## Same as (but easier than)
#'   def_sym(u1, u2, u3)
#'   ## Notice: this creates matrix [u1, u2, u3]
#'   as_sym(u)
#'  }
#' @export  
def_sym_vec <- function(x, env=parent.frame()){  ## FIXME: OK; maybe different name???
    for (i in seq_along(x)){
        assign(x[i], as_sym(x[i]), envir = env)
    }
}


#' Create list of factors as in a product
#'
#' @param ... factors
#'
#' @examples
#' if (has_sympy()) {
#'  d <- 2
#'  m <- matrix_sym(d, d)
#'  mi <- inv(m)
#'  det_m <- det(m)
#'  fl <- as_factor_list(1/det_m, det_m * mi)
#'  tex(fl)
#'  m <- matrix(1:4, nrow=2)
#'  mi <- solve(m)
#'  det_m <- det(m)
#'  fl <- as_factor_list(1 / as_sym(det_m), det_m * mi)
#'  tex(fl)
#' }
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
