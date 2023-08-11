#' Pipe
#'
#' Pipe operator
#'
#' @param lhs,rhs specify what lhs and rhs are
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


# This was created, or else CRAN complained about that the first example
# run took too long - because that was the first call that 
# actually prepared SymPy
.onLoad <- function(libname, pkgname){
  silent_prepare_sympy() 
}
