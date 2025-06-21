#' Do linear algebra operation
#' 
#' @param x A matrix for which a property is requested
#' @param slot The property requested
#' @param ... Auxillary arguments
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", "0", "0", "1"), 2, 2) |> as_sym()
#'   
#'   do_la(A, "QR")
#'   QRdecomposition(A)
#'
#'   do_la(A, "LU")
#'   LUdecomposition(A)
#'
#'   do_la(A, "cholesky", hermitian = FALSE)
#'   chol(A, hermitian = FALSE)
#'   
#'   do_la(A, "singular_value_decomposition")
#'   do_la(A, "svd")
#'   svd_res <- svd_(A)
#'   svd_res
#'   U_expr <- svd_res$U |> as_expr()
#'   U_expr
#'   eval(U_expr, list(a = 3+2i))
#'   
#'   b <- symbol("b", real = TRUE)
#'   B <- matrix(c("b", "0", "0", "1"), 2, 2) |> as_sym(declare_symbols = FALSE)
#'   svd_(B)
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
           "LUdecomposition"=, "LU"= {
               out <- do_la_worker(x, "LUdecomposition", ...)
               return(finalise_LU(out))
           },
           "singular_value_decomposition"=, "svd"= {
             out <- do_la_worker(x, "singular_value_decomposition", ...)
             return(finalise_svd(out))
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
           "rowspace"=, "nullspace"=, "columnspace"=, "singular_values"={
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

finalise_LU <- function(vals) {
    vals <- reticulate::py_to_r(vals)
    
    lu_info <- list(
        L = construct_symbol_from_pyobj(vals[[1L]]),
        U = construct_symbol_from_pyobj(vals[[2L]])
    )  
    return(lu_info)    
}

finalise_svd <- function(vals) {
  vals <- reticulate::py_to_r(vals)
  
  svd_info <- list(
    U = construct_symbol_from_pyobj(vals[[1L]]),
    S = construct_symbol_from_pyobj(vals[[2L]]),
    V = construct_symbol_from_pyobj(vals[[3L]])
  )  
  return(svd_info)    
}


finalise_rref <- function(vals) {
    vals <- reticulate::py_to_r(vals)
    
    rref_info <- list(
        mat = construct_symbol_from_pyobj(vals[[1L]]),
        pivot_vars = unlist(vals[[2L]]) + 1
    )  
    return(rref_info)   
}






#' Do linear algebra operation
#' 
#' Performs various linear algebra operations like finding the inverse, 
#' the QR decomposition, the eigenvectors and the eigenvalues.
#' 
#' @param x,x2 A matrix for which a property is requested.
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
#'   LUdecomposition(A)
#'   #chol(A) # error
#'   chol(A, hermitian = FALSE)
#'   eigenval(A)
#'   eigenvec(A)
#'   inv(A)
#'   det(A)
#'   rowspace(A)
#'   columnspace(A)
#'   nullspace(A)
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
#'   A <- matrix(c("a", "b", "c", "d"), 2, 2) |> as_sym()
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
    # out <- do_la(x, "rowspace")
    # if (matrix)
    #     return(t(do.call(rbind, out)))
    # else 
    #     return(out)
    columnspace(t(x), matrix=matrix)
}




#' @rdname linalg
#' @export
orthcompspace <- function(x, x2=NULL){
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  if (!is.null(x2)) {
    stopifnot_symbol(x2)
    stopifnot_matrix(x2)
  }

  NS <- nullspace(t(x))
  if (is.null(x2))
    return(NS)
  else {
    return(intersectionspace(NS, x2))
  }
}

#' @rdname linalg
#' @export
intersectionspace <- function(x, x2){
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  if (!is.null(x2)) {
    stopifnot_symbol(x2)
    stopifnot_matrix(x2)
  }
  A <- cbind(x, -x2)
  N <- nullspace(A)
  S1 <- N[1:ncol(x),]
  W = x %*% S1
  
  aa <- colSums_(W^2) |> as_character()
  bb <- c(aa != 0)
  if (any(bb)) {
    W[, which(bb)]
  } else {
    NULL
  }
}

#' @rdname linalg
#' @export
leftnullspace <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  orthcompspace(columnspace(x))
}




#' @rdname linalg
#' @export
singular_values <- function(x) {
    return(do_la(x, "singular_values"))
}



#' @rdname linalg
#' @param method The default works by Gaussian elimination.  
#'     Alternatives are $LU$ decomposition (`lu`), the cofactor
#'     method (`cf`), cholesky (`ch`), qr (`qr`), adjugate (`adj`), ldl (`ldl`), block (`block`) and `Ryacas` (`yac`).
#' @export
inv <- function(x, method = c("ge", "gauss", "lu", "cf", "ch", "qr", "adj", "ldl", "block", "yac")) {
  
    stopifnot_symbol(x)
    stopifnot(symbol_is_matrix(x))

    method <- match.arg(method)
      
    switch(method,
           ge    = ,
           gauss = do_la(x, "inv"),
           lu    = inv_lu(x),
           cf    = inv_cf(x),
           ch    = {sympy_func(x, "inverse_CH")}, 
           qr    = {sympy_func(x, "inverse_QR")},
           adj   = {sympy_func(x, "inverse_ADJ")},
           ldl   = {sympy_func(x, "inverse_LDL")},
           block = {sympy_func(x, "inverse_BLOCK")},         
           yac   = inv_yac(x)
           )
}

inv_cf <- function(x) {
    return(sympy_func(x, "adjugate") / sympy_func(x, "det"))
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
    Ai <- Ay |> Ryacas::y_fn("Inverse") |> Ryacas::yac_str()
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
eigen_ <- function(x){
  z <- eigenvec(x)
  a <- lapply(z, function(z.) z.$eigvec)
  eigvec <- do.call(cbind, a)
  
  b <- lapply(z, function(z.) z.$eigval) 
  eigval <- do.call(rbind, b)
  
  eigmult <- sapply(z, function(z.) z.$eigmult) 
  
  out <- list(values=eigval, vectors=eigvec, eigmult=eigmult)
  return(out)
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
QRdecomposition <- function(x) {
    return(do_la(x, "QR"))
}

#' @importFrom Matrix qr
#' @method qr caracas_symbol
#' @export
#' @rdname linalg
setMethod(
  "qr",
  signature(x = "caracas_symbol"),
  function(x) {
      out <- QRdecomposition(x)
      out$rank <- ncol(out$Q)
      class(out) <- c("QRdecomposition", "list")
      out
  }
)


#' @importFrom Matrix chol
#' @method chol caracas_symbol
#' @export
#' @rdname linalg
setMethod(
  "chol",
  signature(x = "caracas_symbol"),
  function(x, ...) {
      return(do_la(x, "cholesky", ...))
      ## out <- QRdecomposition(x)
      ## out$rank <- ncol(out$Q)
      ## class(out) <- c("QRdecomposition", "list")
      ## out
  }
)


## #' @importFrom Matrix chol
## #' @rdname linalg
## #' @export
## chol.caracas_symbol <- function(x, ...) {
##     print(x)
##   return(do_la(x, "cholesky", ...))
## }


setOldClass("QRdecomposition")

#' @importFrom Matrix qr.Q
#' @param qr A QRdecomposition object.
#' @method qr caracas_symbol
#' @export
#' @rdname linalg
setMethod(
  "qr.Q",
  signature(qr = "QRdecomposition"),
  function(qr) {
    qr$Q
  }
)

#' @importFrom Matrix qr.R
#' @method qr caracas_symbol
#' @export
#' @rdname linalg
setMethod(
  "qr.R",
  signature(qr = "QRdecomposition"),
  function(qr) {
    qr$R
  }
)



#' @importFrom Matrix determinant
#' @method x caracas_symbol
#' @param logarithm logical.
#' @export
#' @rdname linalg
setMethod(
  "determinant",
  signature(x = "caracas_symbol", logarithm="ANY"),
  function(x, logarithm=TRUE, ...) {
      if (logarithm){
          return(log(do_la(x, "det")))          
      } else {
          return(do_la(x, "det"))          
      }
  }
)












#' @rdname linalg
#' @export
LUdecomposition <- function(x) {
    return(do_la(x, "LU"))
}


#' @rdname linalg
#' @export
svd_ <- function(x, ...) {
  return(do_la(x, "singular_value_decomposition", ...))
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
det_ <- function(x, ...) {
    ensure_sympy()
    stopifnot_symbol(x)
    stopifnot_matrix(x)    
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
#' ‘X’ and ‘Y’. NOT IMPLEMENTED
#' @aliases kronecker
#' @param ... optional arguments to be passed to ‘FUN’.
#' 
#' @return Kronecker product of A and B.
#'
#' @examples
#' if (has_sympy()) {
#'   A <- matrix_sym(2, 2, "a")
#'   B <- matrix_sym(2, 2, "b")
#'   II <- matrix_sym_diag(2)
#'   EE <- eye_sym(2,2)
#'   JJ <- ones_sym(2,2)
#'
#'   kronecker(A, B)
#'   kronecker(A, B, FUN = "+")
#'   kronecker(II, B)
#'   kronecker(EE, B)
#'   kronecker(JJ, B)
#'
#' Y <- matrix_sym(2, 2)
#' X1 <- diag_(1, 3)
#' X2 <- diag_(as_sym(c("a1", "a2", "a3")))
#' X3 <- matrix_sym(2,2, "u")
#'
#' kronecker(X1, Y)
#' kronecker(X2, Y, FUN="-")
#' kronecker(X3, Y)
#' }
#'
#' @concept linalg
#' @export
setMethod(
  "kronecker", 
  signature = c("caracas_symbol", "caracas_symbol"), 
  definition = function(X, Y, FUN = "*", make.dimnames = FALSE, ...) {
      
      stopifnot_matrix(X)
      stopifnot_matrix(Y)

      is_diagonal_caracas <- function(X) {
          mat <- as_character_matrix(X)
          nr <- nrow(mat)
          nc <- ncol(mat)
          if (nr != nc) return(FALSE)  # Must be square
          
          ## Check off-diagonal elements are "0"
          off_diag <- which(row(mat) != col(mat), arr.ind = TRUE)
          all(mat[off_diag] == "0")
      }

      if (is_diagonal_caracas(X)){
          return(kronecker_fast(X, Y, FUN=FUN))
      }
      
      
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



kronecker_fast <- function(X, Y, FUN = "*") {

    ## cat("fast kronecker\n")
    
    ## Convert X to character matrix
    X_chr <- as_character_matrix(X)
    diag_vals <- diag(X_chr)  # Assumes X is diagonal
    
    n <- length(diag_vals)
    Y <- as.matrix(as_character_matrix(Y))  # Ensure character matrix
    ry <- nrow(Y)
    cy <- ncol(Y)
    
    out <- matrix("0", n * ry, n * cy)
    
    for (i in seq_len(n)) {
        ## block <- matrix(paste0("(", diag_vals[i], ")*(", Y, ")"), ry, cy)
        block <- matrix(paste0("(", diag_vals[i], ")", FUN, "(", Y, ")"), ry, cy)
        row_idx <- ((i - 1) * ry + 1):(i * ry)
        col_idx <- ((i - 1) * cy + 1):(i * cy)
        out[row_idx, col_idx] <- block
    }
    
    out <- as_sym(out)
    return(out)
}






## setMethod(
##   "kronecker", 
##   signature = c("caracas_symbol", "caracas_symbol"), 
##   definition = function(X, Y, FUN = "*", make.dimnames = FALSE, ...) {
      
##       stopifnot_matrix(X)
##       stopifnot_matrix(Y)
      
##       FUN <- match.fun(FUN)
      
##       do_col <- function(i, X, Y) {
##           rr <-
##               lapply(seq_len(ncol(X)), function(j) {
##                   FUN(X[i,j], Y)
##               } 
##               )
##           out <- do.call(cbind, rr)
##           out
##       }
      
##       rr <- lapply(seq_len(nrow(X)),
##                    function(i) {
##                        do_col(i, X, Y)
##                    })
##       out <- do.call(rbind, rr)
##       out
##   }
## )
















## finalise_nullspace <- function(vals) {
##     vals2 <- reticulate::py_to_r(vals)
##     out <- lapply(vals2,
##                   caracas:::construct_symbol_from_pyobj
##                   )
##     B <- do.call(cbind.caracas_symbol, out)
##     B

## }
