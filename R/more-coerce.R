#' Ask type of caracas symbol
#'
#' @param x An object, a caracas object is expected
#'
#' @export
symbol_class <- function(x){
  stopifnot_symbol(x)
  if (symbol_is_atomic(x)) return("atomic")  
  if (symbol_is_vector(x)) return("vector")  
  if (symbol_is_list(x))   return("list")  
  if (symbol_is_matrix(x)) return("matrix")  
  return(character(0L))
}

symbol_is_atomic <- function(x){
  stopifnot_symbol(x)
  z <- as.character(x)
  ## Symbol is atomic if it does not start with "[" or "Matrix([".
  !grepl("^\\[|Matrix\\(\\[", z)
}

symbol_is_vector <- function(x){ 
  stopifnot_symbol(x)
  z <- as.character(x)
  ## z starts with [ followed by anything which is not [
  grepl("^\\[[^\\[]", z)
}

symbol_is_list <- function(x){ 
  stopifnot_symbol(x)
  z <- as.character(x)
  ## z starts with [[ followed by anything which is not [
  grepl("^\\[\\[[^\\[]", z)
}

symbol_is_matrix <- function(x){ 
  stopifnot_symbol(x)
  z <- as.character(x)
  ## z starts with Matrix([ 
  grepl("^Matrix\\(\\[", z)
}

