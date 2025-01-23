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
inv_block <- function(x) {
    if (!inherits(x, c("caracas_symbol", "matrix", "sparseMatrix")))
        stop("'x' must be caracas_symbol, matrix or sparseMatrix")
    if (inherits(x, "carcas_symbol")){
        stopifnot_symbol(x)
        stopifnot(symbol_is_matrix(x))
        
        xx1 <- as_character_matrix(x)
        xx. <- as.blockmatrix(xx1)
        xx.$value <- NULL
        end <- cumsum(sapply(xx., nrow))
        start <- c(1, end[-length(end)]+1)
        
        ddd <- mapply(function(s,e){
            seq(s,e)
        },  start, end, SIMPLIFY = FALSE)
        
        uxx. <- unique(xx.)
        ## This is only done on the unique elements:
        uai <-lapply(uxx., function(z) as_character_matrix(inv(as_sym(z))))
        
        mmm <- sapply(xx., paste0, collapse=" ")
        uuu <- sapply(unique(xx.), paste0, collapse=" ")
        ppp <- sapply(mmm, match, uuu)
        
        for (i in seq_along(ddd)) {
            idx <- ddd[[i]]
            xx1[idx, idx] <- uai[[ppp[i]]]   
        }
        
        as_sym(xx1)
    } else {
        solve(x)
    }
}


##' @title Inverse using woodburys matrix identity
##' @description Computes the inverse of (A+UCV) provided that the inverse of A and C exists.
##' @param A,U,C,V Either a caracas matrix, or a dense or a sparse matrix.
##' @return The inverser of (A+UCV)
##' @author Søren Højsgaard
##'
##' @export
inv_woodbury <- function(A, U, C, V=t(U)) {
  Ai <- solve(A)
  out <- Ai - Ai %*% U %*% solve((solve(C) + V %*% Ai %*% U)) %*% V %*% Ai
  return(out)
}
