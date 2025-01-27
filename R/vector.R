# sum
# prod
# length
# dim
# rbind
# cbind

#' Extract unique elements
#'
#' @param x A caracas vector or matrix
#' @param incomparables Same meaning as for other unique methods
#' @param ... Additional arguments; currently not used.
#'
#' @examples
#' if (has_sympy()){
#' v <- vector_sym(4)
#' v2 <- rep(v, each=2)
#' unique(v2)
#' }
#' @export
unique.caracas_symbol <- function(x, incomparables = FALSE, ...){
    .x <- unique(as_character(x), incomparables = incomparables, ...)
    o <- as_sym(.x)
    return(o)
}




#' Summation
#'
#' @param \dots Elements to sum
#' @param na.rm Not used
#'
#' @concept vectors
#'
#' @export
sum.caracas_symbol <- function(..., na.rm = FALSE) {
    ## cat("sum.caracas_symbol\n")

    expr <- list(...)
    ## str(expr)
    if (length(expr) != 1L) {
        stop("To be implemented")
    }

    expr_ <- expr[[1L]]
    ## str(expr_)
    sum_worker(expr_)
}

sum_worker <- function(expr) {
    ## cat("HERE1\n")
    expr_dim <- dim(expr)
    ## cat("HERE2\n")
    if (is.null(expr_dim)) {
        ## cat("HERE3\n")
        return(expr)
    }
    
    if (!symbol_is_matrix(expr)) {
        stop("Unexpected")
    }
    
    ones1 <- as_sym(matrix(1, nrow = 1L, ncol = expr_dim[1L]))
    z1 <- ones1 %*% expr
    
    ones2 <- as_sym(matrix(1, nrow = expr_dim[2L], ncol = 1L))
    z2 <- z1 %*% ones2
    
    z <- remove_mat_prefix(z2)
    z <- gsub("^\\[\\[(.*)\\]\\]$", "\\1", z)
    z <- eval_to_symbol(z)
    
    return(z)
     
}




#' @export
cbind.caracas_symbol <- function(..., deparse.level = 1) {
  expr <- list(...)
  
  z <- lapply(expr, as_character_matrix)
  z <- do.call(cbind, z)
  z <- as_sym(z)
  
  return(z)
}

#' @export
rbind.caracas_symbol <- function(..., deparse.level = 1) {
  expr <- list(...)
  
  z <- lapply(expr, as_character_matrix)
  
  col_vecs <- unlist(lapply(z, function(w) ncol(w) == 1L))
  
  if (isTRUE(all(col_vecs))) {
    # Transpose:
    z <- lapply(z, t)
  }
  
  z <- do.call(rbind, z)
  z <- as_sym(z)
  
  return(z)
}

#' @export
rev.caracas_symbol <- function(x) {
  if (!symbol_is_matrix(x)) {
    stop("x was not a vector")
  }
  
  x_dim <- dim(x)
  
  if (is.null(x_dim)) {
    stop("x was not a matrix")
  }
  
  if (x_dim[2] != 1L) {
    stop("x was not a vector")
  }
  
  y <- as_character_matrix(x)
  y <- rev(y[, 1L])
  z <- as_sym(y)
  return(z)
}

#' @export
rep.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  
  z <- c(x)
  z <- as_character(z)
  z <- base::rep(z, ...)
  z <- as_sym(z)
  return(z)
}



#' Cumulative Sums
#'
#' @param x Elements to sum
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(1:9, 3)
#'   cumsum(A)
#'   B <- matrix_sym(3, 3)
#'   cumsum(B)
#'   C <- vector_sym(3)
#'   cumsum(C)
#' }
#' 
#' @concept vectors
#'
#' @export
cumsum.caracas_symbol <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)  
  
  y <- c(x) # coerce
  n <- nrow(y)

  if (n == 0L) {
    return(NULL)
  }
  
  if (n == 1L) {
    return(y)
  }
  
  # Now n >= 2L:
  res <- y
  
  for (i in 2L:n) {
    res[i] <- res[i] + res[i-1L]
  }
  
  return(res)
}
