stopifnot_symbol <- function(x){
  if (!inherits(x, "caracas_symbol")) {
    stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
  }
}

stopifnot_matrix <- function(x){
    if (!symbol_is_matrix(x)) {
        stop("'x' must be a matrix")
    }
}

symbol_is_matrix <- function(x) {
  xstr <- as.character(x)
  
  if (grepl("^Matrix\\(\\[", xstr)) {
    return(TRUE)
  }
  
  # FIXME: From der() and der2()
  # if (grepl("^\\[\\[", xstr)) {
  #   return(TRUE)
  # }
  
  return(FALSE)
}

# Still needed?
#symbol_is_list_of_lists_matrix <- function(x) {
#   if (grepl("^\\[\\[", as.character(x))) {
#     return(TRUE)
#   } 
#   
#   return(FALSE)
# }

# symbol_is_vector <- function(x) {
#   xstr <- as.character(x)
#   
#   if (grepl("^\\[", xstr)) {
#     return(TRUE)
#   }
#   
#   return(FALSE)
# }

scalar_to_matrix <- function(scalar, dims) {
  matrix(rep(scalar, prod(dims)), nrow = dims[1L], ncol = dims[2L])
}


# ensure_symbol_is_matrix <- function(x) {
#   if (!inherits(x, "caracas_symbol")) {
#     stop(paste0("'x' ", TXT_NOT_CARACAS_SYMBOL))
#   }
#   
#   if (!symbol_is_matrix(x)) {
#     stop(paste0("'x' is not a matrix"))
#   }
# }

# On purpose: Does not ensure sympy nor check that x is a matrix
number_rows <- function(x) {
  rows <- x$rows
  
  if (inherits(rows, "python.builtin.int")) {
    rows <- reticulate::py_to_r(rows)
  }
  
  return(rows)
}

# On purpose: Does not ensure sympy nor check that x is a matrix
number_cols <- function(x) {
  cols <- x$cols
  
  if (inherits(cols, "python.builtin.int")) {
    cols <- reticulate::py_to_r(cols)
  }
  
  return(cols)
}

#' Get matrix as character matrix
#' 
#' @param x caracas symbol
#' 
#' @concept linalg
#' 
#' @export
as_character_matrix <- function(x) {
  y <- as_expr_worker(x, as_character = TRUE)
  return(eval(parse(text = y)))
}


#' Dimensions of a caracas symbol
#' 
#' @param x caracas symbol
#' 
#' @concept linalg
#' 
#' @export
dim.caracas_symbol <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
    
  if (!symbol_is_matrix(x)) { return(NULL) }
  
  rows <- number_rows(x$pyobj)
  cols <- number_cols(x$pyobj)
  
  return(c(rows, cols))
}



#' Transpose of matrix
#'
#' @param x If `caracas_symbol` treat as such, else
#' call [base::t()].
#'
#' @concept linalg
#' @export
t.caracas_symbol <- function(x) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
    
  xT <- x$pyobj$T
  return(construct_symbol_from_pyobj(xT))
}






#' Matrix diagonal
#'
#' @param x Object `x`
#' @param \dots Passed on
#' 
#' @concept linalg
#' 
#' @export
diag <- function(x, ...) {
  UseMethod("diag")
}

#' @export
diag.default <- function(x, ...) {
  return(base::diag(x, ...))
}

#' Matrix diagonal
#' 
#' @param x Object `x`
#' @param \dots Not used
#' 
#' @concept linalg
#' 
#' @export
diag.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)  
  
  y <- eval_to_symbol(x$pyobj$diagonal())
  
  return(y)
}


#' Replace matrix diagonal
#'
#' @param x Object `x`
#' @param value Replacement value
#' 
#' @concept linalg
#' 
#' @export
`diag<-` <- function(x, value) {
  UseMethod("diag<-")
}

#' @export
`diag<-.default` <- function(x, value) {
  return(base::`diag<-`(x, value))
}

#' Replace diagonal
#' 
#' @param x A `caracas_symbol`.
#' @param value Replacement value
#' 
#' @examples 
#' if (has_sympy()) {
#'   A <- matrix(c("a", 0, 0, 0, "a", "a", "a", 0, 0), 3, 3)
#'   B <- as_sym(A)
#'   B
#'   diag(B)
#'   diag(B) <- "b"
#'   B
#'   diag(B)
#' }
#' 
#' @concept vectors
#' 
#' @export
`diag<-.caracas_symbol` <- function(x, value) {
  ensure_sympy()
  stopifnot_symbol(x)
  stopifnot_matrix(x)
  
  xmat <- convert_to_r_mat(x)
  
  if (inherits(value, "caracas_symbol")) {
    value <- c(as_character_matrix(value))
  }
  
  diag(xmat) <- value
  
  y <- as_sym(xmat)
  return(y)
}



