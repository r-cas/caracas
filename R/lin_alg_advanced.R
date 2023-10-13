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
#'   do_la(A, "trace")
#'   trace_(A)
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
#' @param x A matrix for which a property is requested.
#' @param matrix When relevant should a matrix be returned.
#' @param ... Auxillary arguments.
#' 
#' @seealso [do_la()]
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", "0", "0", "1"), 2, 2) |> as_sym()
#'   
#'   QRdecomposition(A)
#'   eigenval(A)
#'   eigenvec(A)
#'   inv(A)
#'   det(A)
#'   
#'   ## Matrix inversion:
#'   d <- 3
#'   m <- matrix_sym(d, d)
#'   print(system.time(inv(m)))                  ## Gauss elimination
#'   print(system.time(inv(m, method="cf")))     ## Cofactor 
#'   print(system.time(inv(m, method="lu")))     ## LU decomposition
#'   if (requireNamespace("Ryacas")){
#'     print(system.time(inv(m, method="yac")))  ## Use Ryacas
#'   }
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
columnspace <- function(x, matrix=TRUE) {
    out <- do_la(x, "columnspace")
    if (matrix)
        return(do.call(cbind, out))
    else 
        return(out)
}

#' @rdname linalg
#' @export
nullspace <- function(x, matrix=TRUE) {
    out <- do_la(x, "nullspace")
    if (matrix)
        return(do.call(cbind, out))
    else 
        return(out)
}

#' @rdname linalg
#' @export
rowspace <- function(x, matrix=TRUE) {
    out <- do_la(x, "rowspace")
    if (matrix)
        return(do.call(cbind, out))
    else 
        return(out)
}

#' @rdname linalg
#' @export
singular_values <- function(x) {
    return(do_la(x, "singular_values"))
}


#' @rdname linalg
#' @param method The default works by Gaussian elimination.  The
#'     alternatives are $LU$ decomposition (`lu`), the cofactor
#'     method (`cf`), and `Ryacas` (`yac`).
#' @export
inv <- function(x, method = c("gauss", "lu", "cf", "yac")) {
  method <- match.arg(method)
  
  stopifnot_symbol(x)
  stopifnot(symbol_is_matrix(x))
  
  ## if (FALSE) {
    ## microbenchmark::microbenchmark(
      ## inv(A, "lu"),
      ## inv(A, "gauss"),
      ## inv(A, "cf"),
      ## inv(A, "yac"),
      ## times = 10
    ## )
  ## }
  
  switch(method,
         gauss = do_la(x, "inv"),
         lu    = inv_lu(x),
         cf    = inv_cf(x),
         yac   = inv_yac(x))
}

inv_cf <- function(x) {
    return(t(sympy_func(x, "cofactor_matrix")) / det(x))
}

inv_lu <- function(x) {
      construct_symbol_from_pyobj(x$pyobj$inv(method="LU"))
}

inv_yac <- function(x) {
    if (!requireNamespace("Ryacas", quietly = TRUE)) {
      stop("This method requires Ryacas - please install.packages('Ryacas')")
    }

    A_ <- as_character_matrix(x)
    Ay <- Ryacas::as_y(A_)
    Ai <- Ay %>% Ryacas::y_fn("Inverse") %>% Ryacas::yac_str()
    B <- as_sym(Ryacas::as_r(Ai))
    return(B)
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

#' @rdname linalg
#' @export
trace_ <- function(x) {
    ensure_sympy()
    stopifnot_matrix(x)
    ## return(sympy_func(x, "Trace"))
    return(sum(diag(x)))
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

#' @importFrom methods setOldClass
setOldClass("caracas_symbol")

#' Kronecker product of two matrices
#'
#' Computes the Kronecker product of two matrices.
#'
#' @param X,Y matrices as caracas symbols.
#' @param FUN a function; it may be a quoted string.
#' @param make.dimnames Provide dimnames that are the product of the dimnames of
#' ‘X’ and ‘Y’.
#' @param ... optional arguments to be passed to ‘FUN’.
#' 
#' @return Kronecker product of A and B.
#'
#' @examples
#'
#' A <- matrix_sym(2, 2, "a")
#' B <- matrix_sym(2, 2, "b")
#' II <- matrix_sym_diag(2)
#' EE <- eye_sym(2,2)
#' JJ <- ones_sym(2,2)
#'
#' kronecker(A, B)
#' kronecker(A, B, FUN = "+")
#' kronecker(II, B)
#' kronecker(EE, B)
#' kronecker(JJ, B)
#'
#' @concept linalg
#' @export
setMethod(
  "kronecker", 
  signature = c("caracas_symbol", "caracas_symbol"), 
  definition = function(X, Y, FUN = "*", make.dimnames = FALSE, ...) {
    
    stopifnot_matrix(X)
    stopifnot_matrix(Y)
    
    FUN <- match.fun(FUN)
    
    do_col <- function(i, X, Y) {
      rr <-
        lapply(seq_len(ncol(X)), function(j) {
          FUN(X[i,j], Y)
        } 
        )
      out <- do.call(cbind, rr)
      out
    }
    
    rr <- lapply(seq_len(nrow(X)),
                 function(i) {
                   do_col(i, X, Y)
                 })
    out <- do.call(rbind, rr)
    out
  }
)

