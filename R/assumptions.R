#' List defined symbols
#' 
#' @concept caracas_symbol
#' 
#' @export
ls_sym <- function() {
 ensure_sympy()
 
 glb_syms <- reticulate::py_eval("globals()")
 glb_syms <- glb_syms[-(seq_len(pkg_globals$internal_globals_length))]
 
 return(names(glb_syms))
}

#' Ask for a symbol's property
#' 
#' @param x symbol
#' @param property property, e.g. 'positive'
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol("x", positive = TRUE)
#'   ask(x, "positive")
#' }
#' 
#' @concept assumptions
#' 
#' @export
ask <- function(x, property) {
  ensure_sympy()
  
  sympy <- get_sympy()
  
  answer <- sympy$ask(sympy$Q[[property]](x$pyobj))
  
  if (is.logical(answer)) {
    return(answer)
  }
  
  return(NA)
}
