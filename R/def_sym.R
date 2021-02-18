#' Define caracas symbols in global environment
#' 
#' @seealso [symbol()], [as_sym()]
#' 
#' @param ... Names for new symbols, also supports non-standard evaluation
#' @param warn Warn if existing variable names are overwritten
#' 
#' @return Names of declared variables (invisibly)
#' 
#' @examples 
#' if (have_sympy()) {
#'   ls()
#'   global_symbols(n1, n2, n3)
#'   ls()
#'   global_symbols("x1", "x2", "x3")
#'   ls()
#'   global_symbols("x1", "x2", "x3", warn = TRUE)
#'   ls()
#' }
#' 
#' @concept caracas_symbol
#' 
#' @export
def_sym <- function(..., 
                    warn = FALSE,
                    env = parent.frame()) {
  
  ensure_sympy()
  
  dots <- match.call(expand.dots = FALSE)$...
  
  nms <- lapply(seq_along(dots), function(i) {
    y <- if (is.symbol(dots[[i]])) {
      deparse(dots[[i]])
    } else {
      dots[[i]]
    }
    
    return(y)
  })
  
  existing_nms <- c()
  
  for (i in seq_along(nms)) {
    v <- nms[[i]]
    
    verify_variable_name(v)
    
    if (warn && exists(v, envir = env)) {
      existing_nms <- c(existing_nms, v)
    }
    
    cmd <- paste0(v, " = symbols('", v, "')")
    # py_run_string instead of py_eval because we need to assign inside Python
    s <- reticulate::py_run_string(cmd, convert = FALSE)
    res <- s[[v]]

    y <- construct_symbol_from_pyobj(res)
    
    assign(v, value = y, envir = env)
  }
  
  if (warn && length(existing_nms) > 0) {
    warning("The following names were overwritten: ", paste0(nms, collapse = ", "))
  }
  
  return(invisible(nms))
}
