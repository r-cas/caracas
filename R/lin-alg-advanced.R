#' Do linear algebra operation
#' 
#' @param x A matrix for which a property is requested
#' @param slot The property requested
#' @param ... Auxillary arguments
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", "0", "0", "1"), 2, 2) %>% as_sym()
#'   
#'   do_la(A, "QR")
#'   QRdecomposition(A)
#'   
#'   do_la(A, "eigenval")
#'   eigenval(A)
#'   
#'   do_la(A, "eigenvec")
#'   eigenvec(A)
#'   
#'   do_la(A, "inv")
#'   inv(A)
#'   
#'   do_la(A, "echelon_form")
#'   do_la(A, "rank")
#'   
#'   do_la(A, "det") # Determinant
#'   det(A)
#' }
#' 
#' @return Returns the requested property of a matrix.
#' 
#' @concept linalg
#' 
#' @importFrom reticulate py_to_r
#' @export
do_la <- function(x, slot, ...) {
    switch(slot,
           "QRdecomposition"=, "QR"= {
               out <- do_la_worker(x, "QRdecomposition", ...)
               return(finalise_QR(out))
           },
           "eigenval"=, "eigenvals"={
               out <- do_la_worker(x, "eigenvals", ...)
               return(finalise_eigenval(out))
           },
           "eigenvec"=, "eigenvects"={
               out <- do_la_worker(x, "eigenvects", ...)
               return(finalise_eigenvec(out))
           },           
           "charpoly"={
                out <- do_la_worker(x, "charpoly", ...)
                poly <- out$as_expr()
                sym <- construct_symbol_from_pyobj(poly)
                return(sym)
           },          
           "GramSchmidt"={
               out <- GramSchmidt_worker(x)
               return(out)
           },          
           "rref"={
               out <- do_la_worker(x, "rref", ...)
               return(finalise_rref(out))
           },          
           "nullspace"=,"columnspace"=,"rowspace"=, "singular_values"={
               out <- do_la_worker(x, slot, ...)
               out <- reticulate::py_to_r(out)
               out <- lapply(out, construct_symbol_from_pyobj)
               return(out)
           },
           {
               out <- do_la_worker(x, slot, ...)
               out <- reticulate::py_to_r(out)
               return(construct_symbol_from_pyobj(out))
           })
}


do_la_worker <- function(x, slot, ...) {

    ensure_sympy()
    stopifnot_symbol(x)
    stopifnot_matrix(x)
    dots <- list(...)

    if (length(dots) > 3L) stop("Too many dot arguments")
    
    dots <- lapply(dots, function(l) {
        if (inherits(l, "caracas_symbol")) {
            return(l$pyobj)
        }
        
        return(l)
    })

    vals <- if (length(dots) == 0L) { 
        x$pyobj[[slot]]() 
    } else if (length(dots) == 1L) {
        x$pyobj[[slot]](dots[[1L]])
    } else if (length(dots) == 2L) {
        x$pyobj[[slot]](dots[[1L]], dots[[2L]])
    } else if (length(dots) == 3L) {
        x$pyobj[[slot]](dots[[1L]], dots[[2L]], dots[[3L]])
    } else {
        stop("Too many dot arguments")
    }

    return(vals)
}

finalise_eigenval <- function(vals) {
    if (inherits(vals, "python.builtin.dict")) {
        vals <- reticulate::py_to_r(vals)
    }
    
    eig_info <- vector("list", length(vals)) 
    for (i in seq_along(vals)) {
        eig_info[[i]] <- list(
            eigval  = as_sym(names(vals)[i]),
            eigmult = as.integer(vals[i])
        )
    } 
    return(eig_info)
}

finalise_eigenvec <- function(vals) {
    if (inherits(vals, "python.builtin.dict")) {
        vals <- reticulate::py_to_r(vals)
    } else if (inherits(vals, "python.builtin.list")) {
        vals <- reticulate::py_to_r(vals)
    }
    
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

finalise_QR <- function(vals) {
    vals <- reticulate::py_to_r(vals)
    
    qr_info <- list(
        Q = construct_symbol_from_pyobj(vals[[1L]]),
        R = construct_symbol_from_pyobj(vals[[2L]])
    )  
    return(qr_info)    
}

finalise_rref <- function(vals) {
    vals <- reticulate::py_to_r(vals)
    
    rref_info <- list(
        mat = construct_symbol_from_pyobj(vals[[1L]]),
        pivot_vars = unlist(vals[[2L]])+1
    )  
    return(rref_info)   
}





#' Do linear algebra operation
#' 
#' Performs various linear algebra operations like finding the inverse, 
#' the QR decomposition, the eigenvectors and the eigenvalues.
#' 
#' @param x A matrix for which a property is requested
#' @param ... Auxillary arguments
#' 
#' @seealso [do_la()]
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", "0", "0", "1"), 2, 2) %>% as_sym()
#'   
#'   QRdecomposition(A)
#'   eigenval(A)
#'   eigenvec(A)
#'   inv(A)
#'   det(A)
#'   
#'   
#'   A <- matrix(c("a", "b", "c", "d"), 2, 2) %>% as_sym()
#'   evec <- eigenvec(A)
#'   evec
#'   evec1 <- evec[[1]]$eigvec
#'   evec1
#'   simplify(evec1)
#'   
#'   lapply(evec, function(l) simplify(l$eigvec))
#'
#'   A <- as_sym("[[1, 2, 3], [4, 5, 6]]")
#'   pinv(A)
#' }
#' 
#' @return Returns the requested property of a matrix.
#' 
#' @concept linalg
#' @name linalg
NULL


#' @rdname linalg
#' @export
columnspace <- function(x) {
    return(do_la(x, "columnspace"))
}

#' @rdname linalg
#' @export
nullspace <- function(x) {
    return(do_la(x, "nullspace"))
}

#' @rdname linalg
#' @export
rowspace <- function(x) {
    return(do_la(x, "rowspace"))
}

#' @rdname linalg
#' @export
singular_values <- function(x) {
    return(do_la(x, "singular_values"))
}


#' @rdname linalg
#' @export
inv <- function(x) {
    return(do_la(x, "inv"))
}

#' @rdname linalg
#' @export
eigenval <- function(x) {
    return(do_la(x, "eigenvals"))
}

#' @rdname linalg
#' @export
eigenvec <- function(x) {
    return(do_la(x, "eigenvects"))
}

#' @rdname linalg
#' @export
GramSchmidt <- function(x) {
    return(do_la(x, "GramSchmidt"))
}


#' @rdname linalg
#' @export
pinv <- function(x) {
    return(do_la(x, "pinv"))
}

#' @rdname linalg
#' @export
rref <- function(x) {
    return(do_la(x, "rref"))
}


#' @rdname linalg
#' @export
QRdecomposition <- function(x) {
    return(do_la(x, "QR"))
}



#' @rdname linalg
#' @export
det <- function(x, ...) {
    UseMethod("det")
}

#' @export
det.default <- function(x, ...) {
    return(base::det(x, ...))
}

#' @export
det.caracas_symbol <- function(x, ...) {
    return(do_la(x, "det"))
}



GramSchmidt_worker <- function(x) {
    ensure_sympy()
    stopifnot_symbol(x)
    stopifnot_matrix(x)
    
    vs <- lapply(seq_len(ncol(x)), function(i) {
        x[,i, drop = FALSE]$pyobj
    })
    
    val <- pkg_globals$internal_py$GramSchmidt(vs)
    
    s_lst <- lapply(val, function(s) {
        construct_symbol_from_pyobj(s)
    })
    
    out <- do.call(cbind, s_lst)
    
    return(out)
}
