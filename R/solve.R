sol_to_r_symbol_list <- function(x) {
  ensure_sympy()
  
  sols <- lapply(x, function(l) {
    lapply(l, function(z) construct_symbol_from_pyobj(z))
  })
  
  return(sols)
}


#' Solve a linear system of equations
#' 
#' Find `x` in `Ax = b`. If `b` not supplied, 
#' the inverse of `A` is returned.
#' 
#' @param A matrix
#' @param b vector
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix_sym(2, 2, "a")
#'   b <- vector_sym(2, "b")
#'   # Inverse of A:
#'   solve_lin(A) %*% A |> simplify()
#'   # Find x in Ax = b
#'   x <- solve_lin(A, b)
#'   A %*% x |> simplify()
#' }
#' 
#' @concept solve
#' 
#' @export
solve_lin <- function(A, b) {
  Ainv <- inv(A)
  
  if (missing(b)) {
    return(Ainv)
  }
  
  x <- Ainv %*% b
  return(x)
}



rootsolve <- function(lhs, vars) {

    ## USED only in solve.R
    is_atomic <- function(x) {
        xstr <- as.character(x)
        
        PATTERN_PYHTON_VARIABLE <- "[a-zA-Z]+[a-zA-Z0-9_]*"
        
        pattern <- paste0("^", PATTERN_PYHTON_VARIABLE, "$")
        
        return(grepl(pattern, x))
    }
    
    # vars itself is not caracas_symbol, but contains a list of them
    if (!inherits(vars, "caracas_symbol") && is.list(vars) && length(vars) >= 1) {
      all_caracas_symbol <- TRUE
      
      for (v in vars) {
        if (!inherits(v, "caracas_symbol")) {
          all_caracas_symbol <- FALSE
          break
        }
      }
      if (all_caracas_symbol) {
        vars <- unbracket(vars)
      }
    }
    
    if (!is.null(dim(lhs))) {
        if (nrow(lhs) != 1L && ncol(lhs) == 1L) {
            lhs <- t(lhs)
        }
        
        if (nrow(lhs) != 1L) {
            stop("Only 1 row in LHS allowed (multiple columns are allowed)")
        }
        
        if (length(vars) == 1L && !is.null(dim(vars))) {
            vars <- listify(vars)
        }
        
        if (length(vars) != ncol(lhs)) {
            stop("The number of columns must equal the number of variables")
        }
    } 
    
    ## Single variable
    if (inherits(vars, "caracas_symbol")) {
        ##vars <- as.character(vars)
        vars <- vars$pyobj
    } else {
        ## Multiple variables
        vars <- unlist(lapply(vars, function(x) {
            if (inherits(x, "caracas_symbol")) {
                if (!is_atomic(x)) {
                    ##stop("Variable is not atomic")
                    
                    y <- as.vector(as_character_matrix(do.call(rbind, vars)))
                    return(y)
                }
                
                return(as.character(x))
            }
            
            return(x)
        }))
    }
    
    y <- get_sympy()$solve(lhs$pyobj, vars, dict = TRUE, set = FALSE)
    solset <- sol_to_r_symbol_list(y)
    
    class(solset) <- c("caracas_solve_sys_sol", class(solset))
  
  return(solset)
}


#' Solves a system of non-linear equations
#' 
#' If called as `solve_sys(lhs, vars)` 
#' the roots are found. 
#' If called as `solve_sys(lhs, rhs, vars)` 
#' the solutions to `lhs = rhs` for `vars` are found.
#' 
#' @param lhs Equation (or equations as row vector/1xn matrix)
#' @param rhs Equation (or equations as row vector/1xn matrix)
#' @param vars vector of variable names or symbols
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol('x')
#'   exp1 <- 2*x + 2
#'   exp2 <- x
#'   solve_sys(cbind(exp1), cbind(exp2), x)
#'   
#'   x <- symbol("x")
#'   y <- symbol("y")
#'   lhs <- cbind(3*x*y - y, x)
#'   rhs <- cbind(-5*x, y+4)
#'   sol <- solve_sys(lhs, rhs, list(x, y))
#'   sol
#' }
#' 
#' @return A list with solutions (with class `caracas_solve_sys_sol` 
#' for compact printing), each element containing a named 
#' list of the variables' values.
#' 
#' @concept solve
#' 
#' @export
solve_sys <- function(lhs, rhs, vars) {
  if (missing(vars)) {
    # roots:
    vars <- rhs
    sol <- rootsolve(lhs, vars)
    return(sol)
  }
  
  lhs <- lhs - rhs # LHS = RHS <=> LHS - RHS = 0
  sol <- rootsolve(lhs, vars)
  return(sol)
}


#' Solve a System of Linear Equations
#' 
#' @param a caracas_symbol
#' @param b If provided, either a caracas_symbol (if not, `as_sym()` is called on the object)
#' @param \dots Not used
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix_sym(2, 2, "a")
#'   b <- vector_sym(2, "b")
#'   # Inverse of A:
#'   solve(A)
#'   inv(A)
#'   solve(A) %*% A |> simplify()
#'   # Find x in Ax = b
#'   x <- solve(A, b)
#'   A %*% x |> simplify()
#'   solve(A, c(2, 1)) |> simplify()
#' }
#' 
#' @concept solve
#' 
#' @export
solve.caracas_symbol <- function(a, b, ...) {
  ensure_sympy()
  stopifnot_symbol(a)
  
  if (missing(b)) {
    return(inv(a))
  } else {
    
    if (!inherits(b, "caracas_symbol")) {
      b <- as_sym(b)
    }
    
    return(inv(a) %*% b)
  } 
}


#' Solve lower or upper triangular system
#'
#' @name solve_triangular
#' @param a caracas_symbol
#' @param b If provided, either a caracas_symbol (if not, `as_sym()` is called on the object)
#' @param \dots Not used
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix_sym(3, 3)
#'   A[upper.tri(A)] <- 0
#'   solve_lower_triangular(A) |> simplify()
#'   A <- matrix_sym(3, 3)
#'   A[lower.tri(A)] <- 0
#'   solve_upper_triangular(A) |> simplify()
#' }
#' 
#' @concept solve
#'
#' @rdname solve_triangular
#' @export
solve_lower_triangular <- function(a, b, ...){
    ensure_sympy()
    stopifnot_symbol(a)
    if (missing(b))
        b <- diag_(1, nrow(a))
    if (!inherits(b, "caracas_symbol")) {
      b <- as_sym(b)
    }

    out <- do_la_worker(a, "lower_triangular_solve", b)
    out <- construct_symbol_from_pyobj(out)
    return(out)
}

#' @rdname solve_triangular
#' @export
solve_upper_triangular <- function(a, b, ...){
    ensure_sympy()
    stopifnot_symbol(a)
    if (missing(b))
        b <- diag_(1, nrow(a))
    if (!inherits(b, "caracas_symbol")) {
      b <- as_sym(b)
    }

    out <- do_la_worker(a, "upper_triangular_solve", b)
    out <- construct_symbol_from_pyobj(out)
    return(out)
}
