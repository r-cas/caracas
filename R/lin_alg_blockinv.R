##' @title Inverse of block matrix 
##' @param x A block matrix, either a caracas matrix, or a dense or a sparse matrix.
##' @return The inverse in the same form as x.
##' @author Søren Højsgaard
##'
#' @examples 
#' if (has_sympy()) {
#'  I <- diag_(1, 3)
#'  M <- as_sym(matrix(c("a", "b", "c", "d"), nrow=2))
#'  IM <- kronecker(I, M)
#'  inv(IM)
#'  inv_block(IM)
#'
#'  IM. <- subs(IM, c("a", "b", "c", "d"), c(2,1,2,2))
#'  inv_block(IM.)
#' }
##' @importFrom blockmatrix as.blockmatrix
##' @export
inv_block <- function(x){
    inv(x, method="block")
}

## inv_block <- function(x) {
##     if (!inherits(x, c("caracas_symbol", "matrix", "sparseMatrix")))
##         stop("'x' must be caracas_symbol, matrix or sparseMatrix")
##     if (inherits(x, "carcas_symbol")){
##         stopifnot_symbol(x)
##         stopifnot(symbol_is_matrix(x))
        
##         xx1 <- as_character_matrix(x)
##         xx. <- as.blockmatrix(xx1)
##         xx.$value <- NULL
##         end <- cumsum(sapply(xx., nrow))
##         start <- c(1, end[-length(end)]+1)
        
##         ddd <- mapply(function(s,e){
##             seq(s,e)
##         },  start, end, SIMPLIFY = FALSE)
        
##         uxx. <- unique(xx.)
##         ## This is only done on the unique elements:
##         uai <-lapply(uxx., function(z) as_character_matrix(inv_yac(as_sym(z))))
        
##         mmm <- sapply(xx., paste0, collapse=" ")
##         uuu <- sapply(unique(xx.), paste0, collapse=" ")
##         ppp <- sapply(mmm, match, uuu)
        
##         for (i in seq_along(ddd)) {
##             idx <- ddd[[i]]
##             xx1[idx, idx] <- uai[[ppp[i]]]   
##         }
        
##         as_sym(xx1)
##     } else {
##         solve(x)
##     }
## }


##' @title Inverse using woodburys matrix identity
##' @description Computes the inverse of (A+UCV) provided that the inverse of A and C exists.
##' @param A,U,C,V Either a caracas matrix, or a dense or a sparse matrix.
##' @param method One of the methods that can be supplied to inv().
##' ## @param timing Should timing be printed
##' @return The inverse of (A+UCV)
##' @author Søren Højsgaard
##'
##' @examples
##' if (has_sympy()) {
##'
##' n <- 8
##' m <- 4
##' A <- diag_("a", n)
##' U <- round(10*(matrix(rnorm(n*m), nrow=n)))
##' U[U < 0] <- 0
##' U <- as_sym(U)
##' V <- t(U)
##' C <- diag_("c", m)
##'
##' B <- A + U %*% C %*% V
##' B
##'
##' Bi <- inv_woodbury(A, U, C)
##' }
##' @export
inv_woodbury <- function(A, U, C, V=t(U), method="ge", simplify=TRUE) {
    ## t0 <- Sys.time()
    Ai <- inv(A, method=method)
    Ci <- inv(C, method=method)
    VAiU <- V %*% Ai %*% U 
    if (simplify){
        VAiU <- simplify(VAiU)
    }
    tmp <- Ci + VAiU
    tmp <- inv(tmp, method=method)
    if (simplify){
        tmp <- simplify(tmp)
    }
    out <- Ai - Ai %*% U %*% tmp %*% V %*% Ai
    ## tt <- Sys.time() - t0
    ## if (timing) print(tt)
    return(out)
}


## inv_fast <- function(x){
##     ## The transpose of the cofactor matrix
##     return(sympy_func(x, "adjugate") / sympy_func(x, "det"))
## }

