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
  
  # Prepare chunk engine ''
  if (requireNamespace("knitr", quietly = TRUE)) {
    # Avoid global variables
    local({
      R_engine  <- knitr::knit_engines$get("R")
      
      prefix <- "cat(paste0('$$', tex("
      postfix <- "), '$$'))"
      
      knitr::knit_engines$set(rtex = function(options) {
        options$code <- paste0(prefix, options$code, postfix)
        options$results <- "asis"
        y <- R_engine(options)
        
        if (options$echo) {
          y <- gsub(prefix, "", y, fixed = TRUE)
          y <- gsub(postfix, "", y, fixed = TRUE)
        }
        
        y
      })
    })
  }
}
