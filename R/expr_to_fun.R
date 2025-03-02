## FIXME (SH): Perhaps remove dependency on doBy?

#' Convert expression into function object.
#'
#' @param x caracas expression.
#' @param order desired order of function argument. Defaults to
#'     alphabetical ordering.
#' @param vec_arg should the function take vector valued argument.
#'
#' @examples
#' if (has_sympy()) {
#'   def_sym(b0, b1, b2, k, x)
#'   e <- b1 + (b0 - b1)*exp(-k*x) + b2*x
#' 
#'   f1 <- as_func(e)
#'   f1
#'   f1(1, 2, 3, 4, 5)
#'   f1 <- as_func(e, order = sort(all_vars(e)))
#'   f1(1, 2, 3, 4, 5)
#'   f2 <- as_func(e, vec_arg = TRUE)
#'   f2
#'   f2(c(1, 2, 3, 4, 5))
#'   f2 <- as_func(e, order = sort(all_vars(e)), vec_arg = TRUE)
#'   f2
#'   f2(c(1,2,3,4,5))
#'   
#'   f1a <- as.function(e)
#'   f1a
#'   f1a(1, 2, 3, 4, 5)
#'   f1(1, 2, 3, 4, 5)
#' }
#' @concept caracas_symbol
#' @export
as_func <- function(x, order=NULL, vec_arg=FALSE){
    ensure_sympy()
    stopifnot_symbol(x)

    x <- as_expr(x)
    doBy::expr_to_fun(x, order=order, vec_arg=vec_arg)
}


#' @rdname as_func
#' @param \dots not used
#' @export
as.function.caracas_symbol <- function(x, ...) {
  as_func(x, vec_arg = TRUE)
}
