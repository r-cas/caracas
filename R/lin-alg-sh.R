
#' getLA
#' 
#' @param x A matrix for which a property is requested
#' @param slot The property requested
#' @param ... Auxillary arguments
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", "0", "0", "1"), 2, 2) %>% as_sym()
#'   getLA(A, "QR")
#'   getLA(A, "eigenval")
#'   getLA(A, "eigenvec")
#'   getLA(A, "inv")
#'   getLA(A, "echelon_form")
#'   getLA(A, "rank")
#'   getLA(A, "det")
#' }
#' 
#' @return Returns the requested property of a matrix.
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
getLA <- function(x, slot, ...) {
    
    switch(slot,
           "QRdecomposition"=, "QR"= {
               out <- getLA_worker(x, "QRdecomposition", ...)
               return(do_QR(out))
           },
           "eigenval"=, "eigenvals"={
               out <- getLA_worker(x, "eigenvals", ...)
               return(do_eigenval(out))
           },
           "eigenvec"=, "eigenvects"={
               out <- getLA_worker(x, "eigenvects", ...)
               return(do_eigenvec(out))
           },           
           {
               out <- getLA_worker(x, slot, ...)
               return(construct_symbol_from_pyobj(out))
           })
}

getLA_worker <- function(x, slot, ...) {

    ensure_sympy()
    is_symbol_check(x)
    is_matrix_check(x)
    dots <- list(...)

    if (length(dots) > 3) stop("Too many dot arguments")

    if (length(dots) == 0) vals <- x$pyobj[[slot]]()
    if (length(dots) == 1) vals <- x$pyobj[[slot]](dots[[1]])
    if (length(dots) == 2) vals <- x$pyobj[[slot]](dots[[1]], dots[[2]])
    if (length(dots) == 3) vals <- x$pyobj[[slot]](dots[[1]], dots[[2]], dots[[3]])

    out <- reticulate::py_to_r(vals)    
}

do_eigenval <- function(vals){
    eig_info <- vector("list", length(vals)) 
    for (i in seq_along(vals)) {
        eig_info[[i]] <- list(
            eigval  = as_sym(names(vals)[i]),
            eigmult = as.integer(vals[i])
        )
    } 
    return(eig_info)
}

do_eigenvec <- function(vals) {
    eig_info <- vector("list", length(vals))
    for (i in seq_along(vals)) {
        eigvalvec <- vals[[i]]
        res <- list(
            eigval = construct_symbol_from_pyobj(eigvalvec[[1L]]),
            eigmult = as.integer(eigvalvec[[2L]]),
            eigvec = construct_symbol_from_pyobj(eigvalvec[[3L]][[1L]])
        )    
        eig_info[[i]] <- res
    }
    return(eig_info)
}

do_QR <- function(vals){
    qr_info <- list(
        Q = construct_symbol_from_pyobj(vals[[1L]]),
        R = construct_symbol_from_pyobj(vals[[2L]])
    )  
    return(qr_info)    
}

