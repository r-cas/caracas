#' @importFrom methods setOldClass
setOldClass("caracas_symbol")

## ## ### From ->->-> To  ###

#' @importFrom methods setAs
setAs("caracas_symbol", "character",    function(from) as.character(from))
setAs("caracas_symbol", "matrix",       function(from) matrify(from))

## setAs("character", "caracas_symbol",    function(from) as_sym(from))

## FIXME: vectorfy


#' Ask type of caracas symbol
#'
#' @param x An object, a caracas object is expected
#' 
#' @concept caracas_symbol
#' 
#' @export
symbol_class <- function(x) {
  stopifnot_symbol(x)
  if (symbol_is_atomic(x)) return("atomic")  
  if (symbol_is_vector(x)) return("vector")  
  if (symbol_is_list(x))   return("list")  
  if (symbol_is_matrix(x)) return("matrix")  
  return(character(0L))
}

#' @inherit symbol_class
#' @export
sym_class <- symbol_class


symbol_is_atomic <- function(x) {
  z <- as.character(x)
  ## Symbol is atomic if it does not start with "[" or "Matrix([".
  !grepl("^\\[|Matrix\\(\\[", z)
}

symbol_is_vector <- function(x) { 
  z <- as.character(x)
  ## z starts with [ followed by anything which is not [
  grepl("^\\[[^\\[]", z)
}

symbol_is_list <- function(x) { 
  z <- as.character(x)
  ## z starts with [[ followed by anything which is not [
  grepl("^\\[\\[[^\\[]", z)
}

symbol_is_matrix <- function(x) { 
  z <- as.character(x)
  ## z starts with Matrix([ 
  grepl("^Matrix\\(\\[", z)
}

#' Ask if type of caracas symbol is of a requested type
#'
#' @param x An object, a caracas object is expected
#' @param what Requested type (e.g. atomic, vector, list, matrix)
#' 
#' @concept caracas_symbol
#' 
#' @export
sym_inherits <- function(x, what) {
    cls <- sym_class(x)
    return(identical(cls, what))
}


#' Coerce caracas object
#'
#' @name to_something
#' 
#' @param x a caracas object is expected
#' 
#' @concept caracas_symbol
#'
#' @export
#' @rdname to_something
to_list <- function(x) {
  stopifnot_symbol(x)
  z <- as.character(x)
  switch(symbol_class(x),
         "atomic"={
           o <- paste0("[[", z, "]]")
           return(caracas::eval_to_symbol(o))
         },
         "vector"={
           o <- paste0("[", z, "]")
           return(caracas::eval_to_symbol(o))
         },
         "list"={
           return(x)
         },
         "matrix"={
           return(sympy_func(x, "tolist"))
         }
  )
}

#' @export
#' @rdname to_something
to_vector <- function(x) {
  stopifnot_symbol(x)
  z <- as.character(x)
  switch(symbol_class(x),
         "atomic"={
           o <- paste0("[", z, "]")
           return(caracas::eval_to_symbol(o))
         },
         "vector"={
           return(x)
         },
         "list"={
           return(unbracket(x))
         },
         "matrix"={
           o <- extract_elements(t(x))
           o2 <- paste0("[", o, "]")
           return(caracas::eval_to_symbol(o2))
         }
  )
}

#' @export
#' @rdname to_something
to_matrix <- function(x) {
  stopifnot_symbol(x)
  z <- as.character(x)
  switch(symbol_class(x),
         "atomic"={
           o <- paste0("Matrix([", z, "])")
           return(eval_to_symbol(o))
         },
         "vector"={
           o <- paste0("Matrix(", z, ")")
           return(eval_to_symbol(o))
         },
         "list"={
             o <- paste0("Matrix(", paste0(x, collapse = ", "), ")")
             return(eval_to_symbol(o))
         },
         "matrix"={
           return(x)
         }
  )
}


#' Creates matrix from array symbol
#' 
#' @param x Array symbol to convert to matrix
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol("x")
#'   y <- symbol("y")
#'   f <- 3*x^2 + x*y^2
#'   matrify(f)
#'   h <- der2(f, list(x, y))
#'   h
#'   dim(h)
#'   H <- matrify(h)
#'   H
#'   dim(H)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
matrify <- function(x) {
  stopifnot_symbol(x)
  
  if (!grepl("^\\[", as.character(x))) {
    #x <- c(x) 
    x <- paste0("[", as_character(x), "]")
  }
  
  z <- paste0("Matrix(", paste0(x, collapse = ", "), ")")
  y <- eval_to_symbol(z)
  return(y)
}

#' Creates symbol vector from list of caracas symbols
#' @param x Symbol to be coerced to vector
#' @concept caracas_symbol
#' @export
vectorfy <- function(x) {
  z <- paste0(unlist(lapply(x, as.character)), collapse = ", ")
  z <- paste0("[", z, "]")
  y <- eval_to_symbol(z)
  return(y)
}

#' Convert object to list of elements
#' @param x Object
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
#'   listify(x)
#'   
#'   xT <- t(x)
#'   listify(xT)
#'
#'   def_sym(s)
#'   listify(s)
#'   listify(c_(s, s))
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
listify <- function(x) {
    ## zz <- convert_to_r_mat(x)
    
    zz <- as_character_matrix(x)    
    dim(zz) <- NULL
    zz <- as.list(zz)
    zz <- lapply(zz, as_sym, declare_symbols = FALSE)
    return(zz)
}


#' Convert object to tuple
#' 
#' @param x Object
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- as_sym("Matrix([[b1*x1/(b2 + x1)], [b1*x2/(b2 + x2)], [b1*x3/(b2 + x3)]])")
#'   tuplify(x)
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
tuplify <- function(x) {
  zz <- extract_elements(x)
  zz <- paste0("(", zz, ")")
  
  y <- eval_to_symbol(zz)
  return(y)
}



## #' @concept caracas_symbol
## #' @export
## as_sym_list <- function(x) {
##     listify(x)
## }

## as_sym_matrix <- function(x) {
##     if (is_sym(x) || is_sym_list(x)){
##         matrify(vectorfy(x))            
##     } else {
##         stop("'x' is not list of caracas symbols\n")        
##     }
## }

##' @title Is list of caracas symbols
##' @param x Object
##' @return logical
##' @author Søren Højsgaard
##' @export
is_sym_list <- function(x) {
    if (!is.list(x))
        return(FALSE)
    all(sapply(x, is_sym))
}
