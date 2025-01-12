##' @title General purpose optimization of caracas symbol.
##' @description Adaptation of optim() to work on caracas symbols.
##' @param par As for optim()
##' @param fn A caracas symbol
##' @param gr As for optim()
##' @param ... As for optim()
##' @param method As for optim()
##' @param lower As for optim()
##' @param upper As for optim()
##' @param control As for optim()
##' @param hessian As for optim()
##' @return As for optim()
##' @details The caracas symbol `fn` is coerced into an R function on which optim() is subsequently applied. 
##' @author Søren Højsgaard
##' 
##' @export 
optim_sym <- function (par, fn, gr = NULL, ..., method = c("Nelder-Mead", 
    "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"), lower = -Inf, 
    upper = Inf, control = list(), hessian = FALSE) 
{
   cl <- match.call()

   if (!inherits(fn, c("caracas_symbol")))
      stop("fn must be a caracas symbol\n")
   fn <- as_func(fn, vec_arg = TRUE)
  
   names(par) <- formals(fn)$names_parm
   cl[[1]] <- as.name("optim")
   cl$par <- par
   cl$fn <- fn
   eval(cl, parent.frame())
}
