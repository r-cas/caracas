
#' @title Finds the basis of the (right) null space.
#' 
#' @description Finds the basis of the (right) null space of a matrix, a vector
#'     (a 1-column matrix) or a model object for which a model matrix can be
#'     extracted. I.e. finds basis for the (right) null space x : Mx = 0.
#'
#' @name linear_algebra
#' 
#' @param M,M2 A matrix or a vector (a 1-column matrix).
#' @return A matrix (possibly with zero columns if the null space consists only
#'     of the zero vector).
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link[MASS]{Null}}
#' @keywords utilities
#' @examples
#' 
#' M <- matrix(c(1,1,1,1,1,1,0,0,0,0,1,1), nrow=4)
#' basis_null(M)
#' MASS::Null(t(M))
#' 
#' M <- matrix(c(1,1,1,1))
#' basis_null(M)
#' MASS::Null(t(M))
#' 
#' m0 <- lm(breaks ~ wool + tension, data=warpbreaks)
#' basis_null(model.matrix(m0))
#' MASS::Null(t(model.matrix(m0)))
#' 
#' ## Make balanced dataset
#' dat.bal   <- expand.grid(list(A=factor(1:2), B=factor(1:3), C=factor(1:3)))
#' dat.bal$y <- rnorm(nrow(dat.bal))
#' 
#' ## Make unbalanced dataset: 'B' is nested within 'C' so B=1 is only
#' ## found when C=1 and B=2,3 are found in each C=2,3,4
#' dat.nst <- dat.bal
#' dat.nst$C <-factor(c(1,1,2,2,2,2,1,1,3,3,3,3,1,1,4,4,4,4))
#' xtabs(y ~ C+B+A , data=dat.nst)
#' 
#' mod.bal  <- lm(y ~ A + B*C, data=dat.bal)
#' mod.nst  <- lm(y ~ A + B*C, data=dat.nst)
#' 
#' basis_null( model.matrix(mod.bal) )
#' basis_null( model.matrix(mod.nst) )
#' 
#' MASS::Null( t(model.matrix(mod.bal)) )
#' MASS::Null( t(model.matrix(mod.nst)) )
#' 
#' 
#' 
#' @export basis_null

is_caracas_matrix <- function(M){
    inherits(M, "caracas_symbol") && symbol_is_matrix(M)    
}

is_numeric_matrix <- function(M) {
    is.matrix(M) && is.numeric(M)
}

get_type <- function(M){
    vvv <- NULL
    if (is_numeric_matrix(M)){
        vvv <- "numeric"
    } else {
        if (is_caracas_matrix(M)) {
            vvv <- "caracas"
        }
    }
    return(vvv)
}

#' @importFrom MASS Null
MASS_Null <- function (M) 
{
    tmp <- qr(M)
    set <- if (tmp$rank == 0L) 
               seq_len(ncol(M))
           else -seq_len(tmp$rank)
    qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}


#' @rdname linear_algebra
#' @export
basis_intersect <- function(M, M2){
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={
               out <- intersectionspace(as_sym(M), as_sym(M2))
               as_expr(out)
           },
           "caracas"={intersectionspace(M, M2)}
           )
        
}


    
#' @rdname linear_algebra
#' @export
basis_orthcomp <- function(M, M2=NULL) {
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={
               if (is.null(M2)){
                   tmp <- qr(M)
                   seq <- -seq_len(tmp$rank) ## Notice -sign
                   M_perp <- qr.Q(tmp, complete=TRUE)[ , seq, drop=FALSE]
                   M_perp       
               } else {
                   out <- orthcompspace(as_sym(M), as_sym(M2))
                   as_expr(out)
               }},               
           "caracas"={orthcompspace(M, M2)}
           )
}

#' @rdname linear_algebra
#' @export
basis_col <- function(M) {
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={
               tmp <- qr(M)
               seq <- seq_len(tmp$rank)
               M_col <- qr.Q(tmp, complete=TRUE)[ , seq, drop=FALSE]
               M_col
           },
           "caracas"={columnspace(M)}
           )
}

#' @rdname linear_algebra
#' @export
basis_row <- function(M) {
  basis_col(t(M))
}

#' @rdname linear_algebra
#' @export
basis_null <- function(M) {
#  orth_comp_basis(col_basis(t(M)))
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={MASS_Null(t(M))},
           "caracas"={nullspace(M)}
           )
}

#' @rdname linear_algebra
#' @export
basis_leftnull <- function(M) {
##    orth_comp_basis(col_basis(A))
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={MASS_Null(M)},
           "caracas"={leftnullspace(M)}
           )
}


#' @rdname linear_algebra
#' @export
rref <- function(M) {

    rref_worker <- function (A) 
    {
        eps <- function (x = 1) 
        {
            stopifnot(is.numeric(x))
            x <- max(abs(x))
            if (x < .Machine$double.xmin) {
                e <- .Machine$double.xmin
            }
            else {
                e <- 2^floor(log2(x)) * .Machine$double.eps
            }
            e
        }


        stopifnot(is.numeric(A))
        if (!is.matrix(A)) 
            stop("Input parameter 'A' must be a matrix.")
        nr <- nrow(A)
        nc <- ncol(A)
        tol <- eps() * max(nr, nc) * max(abs(A))
        r <- 1
        for (i in 1:nc) {
            pivot <- which.max(abs(A[r:nr, i]))
            pivot <- r + pivot - 1
            m <- abs(A[pivot, i])
            if (m <= tol) {
                A[r:nr, i] <- 0
            }
            else {
                A[c(pivot, r), i:nc] <- A[c(r, pivot), i:nc]
                A[r, i:nc] <- A[r, i:nc]/A[r, i]
                if (r == 1) {
                    ridx <- c((r + 1):nr)
                }
                else if (r == nr) {
                    ridx <- c(1:(r - 1))
                }
                else {
                    ridx <- c(1:(r - 1), (r + 1):nr)
                }
                A[ridx, i:nc] <- A[ridx, i:nc] - A[ridx, i, drop = FALSE] %*% 
                    A[r, i:nc, drop = FALSE]
                if (r == nr) 
                    break
                r <- r + 1
            }
        }
        A[abs(A) < tol] <- 0
        return(A)
    }
    
    vvv <- get_type(M)
    switch(vvv,
           "numeric"={rref_worker(M)},
           "caracas"={return(do_la(M, "rref"))}
           )
    
    
}
