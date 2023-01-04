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
#' }
#' @concept caracas_symbol
#' @export
as_func <- function(x, order=NULL, vec_arg=FALSE){
    ensure_sympy()
    stopifnot_symbol(x)

    x <- as_expr(x)
    if (vec_arg){
        doBy::expr_to_fun(x, order=order, vec_arg=TRUE)
    } else {
        doBy::expr_to_fun(x, order=order, vec_arg=FALSE)
    }    
}






















## ' @export
## expr_to_one_param_fun <- function(e, order=NULL){
##     if (!is.null(order)) ## FIXME FRAGILE
##         nms <- order
##     else
##         nms <- all.vars(e)
##     e_str <- expr_to_string(e)

##     if (length(nms)) {
##         aux <- sapply(1:length(nms),
##                       function(i) {
##                           nm <- nms[i]
##                           paste0(nm, " = parm[", i, "]")
##                       }
##                       )
##     } else {
##         aux <- NULL
##     }

##     comb <- c(aux, e_str)    
    
##     fun_str <- "function(parm)"
    
##     bd <- paste0("\n{ \n", paste0(comb, collapse=";\n "), "\n}")
##     ff <- paste0(fun_str, bd)
##     fun <- eval(parse(text=ff))
##     return(fun)
## }

## ## ' @export
## expr_to_multi_param_fun <- function(e, order=NULL){
##     if (!is.null(order)) ## FIXME FRAGILE
##         nms <- order
##     else
##         nms <- all.vars(e)
##     e_str <- expr_to_string(e)

##     fun_str <- paste0("function(", paste0(nms, collapse=", "), ")")
    
    
##     bd <- paste0("\n{ \n", paste0(e_str, collapse=";\n "), "\n}")
    
##     ff <- paste0(fun_str, bd)

##     fun <- eval(parse(text=ff))
##     return(fun)
## }


## expr_to_string <- function(e){
##     e_str <- lapply(e, deparse)

##     e_str <-
##         lapply(e_str,
##            function(e){
##                paste0(e, collapse=" ")               
##            })
##     e_str
## }



## expr_to_fun <- function(e){
    ## vn <- all.vars(e)
    ## fmls <- vector("list", length(vn))
    ## names(fmls) <- vn
    
    ## out <- function(){}

    ## formals(out) <- fmls
    ## body(out) <- e
    ## return(out)
## }
