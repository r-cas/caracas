#' Special matrices: zeros, ones, eyes
#' @name special_matrices
#' @param nrow,ncol Number of rows and columns of output
#' @seealso [diag_()], [matrix_sym()], [vector_sym()]
#' @examples
#'
#' if (has_sympy()){
#'   zeros(3, 4)
#'   ones(3, 4)
#'   eye(3, 4)
#' }
#' 
#' @export
#' @rdname special_matrices
zeros <- function(nrow, ncol){
    as_sym(matrix(0, nrow=nrow, ncol=ncol))
}

#' @export
#' @rdname special_matrices
ones <- function(nrow, ncol){
    as_sym(matrix(1, nrow=nrow, ncol=ncol))
}

#' @export
#' @rdname special_matrices
eye <- function(nrow, ncol){
    if (nrow==ncol)
        return(diag_(1, nrow))
    m <- min(nrow, ncol)
    out <- matrix(0, nrow=nrow, ncol=ncol)
    d <- diag(1, m)
    out[1:m, 1:m] <- d
    as_sym(out)
}


#' Difference matrix
#'
#' @param N Number of rows (and columns)
#' @param l Value / symbol below main diagonal
#' @param d Value / symbol on main diagonal
#'
#' @examples
#' if (has_sympy()){
#' Dm <- diff_mat(4)
#' Dm
#' y <- vector_sym(4, "y")
#' Dm %*% y
#' }

#' @export
diff_mat <- function(N, l="-1", d=1){
    L1 <- diag(d, N)
    L1[cbind(1 + (1:(N-1)), 1:(N-1))] <- l
    L1 <- as_sym(L1)
    L1
}



#' Score and Hessian matrix
#'
#' Compute column vector of first derivatives and matrix of second
#' derivatives of univariate function.
#' 
#' @name score_hessian
#' @param expr 'caracas expression'.
#' @param vars variables to take derivative with respect to.
#' @param simplify Try to simplify result using `simplify()`; may be time consuming.
#' @seealso [jacobian()], [der()]
#' @examples
#' 
##' if (has_sympy()) {
##' def_sym(b0, b1, x, x0)
##' f <- b0 / (1 + exp(b1*(x-x0)))
##' S <- score(f, c(b0, b1))
##' H <- hessian(f, c(b0, b1))
##' }
##' 
#' @export
#' @rdname score_hessian
score <- function(expr, vars, simplify=TRUE){
    ensure_sympy()
    stopifnot_symbol(expr)
    if (!symbol_is_atomic(expr)){
        stop("'expr' must be atomic")
    }
    out <- der(expr, vars, simplify = simplify)
    out <- to_matrix(out)
    return(out)
}

#' @export
#' @rdname score_hessian
hessian <- function(expr, vars, simplify=TRUE){
    ensure_sympy()
    stopifnot_symbol(expr)
    if (!symbol_is_atomic(expr)){
        stop("'expr' must be atomic")
    }
    out <- der2(expr, vars, simplify = simplify)
    return(to_matrix(out))
}

#' Compute Jacobian
#'
#' @param expr 'caracas expression'.
#' @param vars variables to take derivative with respect to 
#'
#' @seealso [score()], [hessian()] [der()]
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
    ensure_sympy()
    stopifnot_symbol(expr)
    out <- der(expr, vars)
    out <- matrify(out)
    out <- t(out)
    return(out)
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
    class(out) <- c("caracas_factor_list", "list")
    out
}

#' Divide or multiply matrix with factor.
#' @name mat_div_mult
#' @param m Matrix
#' @param s Factor
#' 
#' @export
#' @rdname mat_div_mult
mat_factor_div <- function(m, s){
  numer <- 
    as_factor_list(paste0("1/S(", s, ")"), s * m)
}

#' @export
#' @rdname mat_div_mult
mat_factor_mult <- function(m, s){
    as_factor_list(s, m / s)
}

#' Matrix cross product
#'
#' @name matrix_cross_product
#' @param x,y caracas matrices
#'
#' @export
#' @rdname matrix_cross_product
crossprod_ <- function(x, y=NULL){
    if (is.null(y)){
        t(x) %*% x        
    } else {
        t(x) %*% y
    }
}
#' @export
#' @rdname matrix_cross_product
tcrossprod_ <- function(x, y=NULL){
    if (is.null(y)){
        x %*% t(x) 
    } else {
        x %*% t(y) 
    }
}

#' Print factor list
#'
#' @param x factor list
#' @param \dots Other arguments passed along
#' 
#' @export
tex.caracas_factor_list <- function(x, ...){
    a <- unlist(lapply(x, tex, ...))
    o <- paste(a, collapse = "  ")
    o
}







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
    stopifnot_symbol(x)
    
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
