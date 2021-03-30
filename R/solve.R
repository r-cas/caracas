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
  
  # Single variable
  if (inherits(vars, "caracas_symbol")) {
    #vars <- as.character(vars)
    vars <- vars$pyobj
  } else {
    # Multiple variables
    vars <- unlist(lapply(vars, function(x) {
      if (inherits(x, "caracas_symbol")) {
        if (!is_atomic(x)) {
          #stop("Variable is not atomic")
          
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


