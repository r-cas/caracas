sol_to_r_symbol_list <- function(x) {
  ensure_sympy()
  
  sols <- lapply(x, function(l) {
    lapply(l, function(z) construct_symbol_from_pyobj(z))
  })
  
  return(sols)
}

#' Find inverse of matrix
#' 
#' @param A matrix
#' 
#' 
#' @concept solve
#' 
#' @export
inverse <- function(A) {
  ensure_sympy()
  
  if (!symbol_is_matrix(A)) {
    stop("'A' must be a matrix")
  }
  
  Ainv <- A$pyobj$inv()
  return(construct_symbol_from_pyobj(Ainv))
}

#' Solve a linear system of equations
#' 
#' Find `x` in `Ax = b`
#' 
#' @param A matrix
#' @param b vector
#' 
#' @concept solve
#' 
#' @export
linsolve <- function(A, b) {
  Ainv <- inverse(A)
  x <- Ainv %*% b
  return(x)
}

#' Find the roots in a system of non-linear equations
#' 
#' @param lhs Equation (or equations as row vector/1xn matrix)
#' @param vars vector of variable names or symbols
#' 
#' @concept solve
#' 
#' @export
rootsolve <- function(lhs, vars) {
  if (!is.null(dim(lhs))) {
    if (nrow(lhs) != 1L) {
      stop("Only 1 row in LHS allowed (multiple columns are allowed)")
    }
    
    if (length(vars) != ncol(lhs)) {
      stop("The number of columns must equal the number of variables")
    }
  } 
  
  # Single variable
  if (inherits(vars, "caracas_symbol")) {
    vars <- as.character(vars)
  } else {
    # Multiple variables
    vars <- unlist(lapply(vars, function(x) {
      if (inherits(x, "caracas_symbol")) {
        if (!is_atomic(x)) {
          stop("Variable is not atomic")
        }
        
        return(as.character(x))
      }
      
      return(x)
    }))
  }
  
  y <- get_sympy()$solve(lhs$pyobj, vars, dict = TRUE, set = FALSE)
  solset <- sol_to_r_symbol_list(y)
  
  return(solset)
}

#' Solves a system of non-linear equations
#' 
#' Finds roots in `lhs - rhs`.
#' 
#' @param lhs Equation (or equations as row vector/1xn matrix)
#' @param rhs Equation (or equations as row vector/1xn matrix)
#' @param vars vector of variable names or symbols
#' 
#' @concept solve
#' 
#' @export
systemsolve <- function(lhs, rhs, vars) {
  eqs <- lhs - rhs # LHS = RHS <=> LHS - RHS = 0
  
  return(rootsolve(eqs, vars))
}

