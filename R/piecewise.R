

##' @title Create a piecewise object
##' @param pw A caracas object containg a Piecewise specification
##' @return A list
##' @author Søren Højsgaard
##' @name piecewise
##'
##' @examples
##' if (has_sympy()) {
##'   library(caracas)
##'   def_sym(r, n, j)
##'   sum1 <- sum_(r^j, var=j, lower=0, upper=n)
##'   pw <- as_piecewise(sum1)
##'   pw[[2]]$expr
##'
##'   pw |> piecewise_cond()
##'   pw |> piecewise_expr()
##' 
##' }
##'
##' @export
##' @rdname piecewise
as_piecewise <- function(pw){

    do_logicals <- function(x){
        
        if (identical(as_character(x), "True")){
            return(TRUE)
        }
        if (identical(as_character(x), "False")){
            return(FALSE)
        }
        return(x)
    }
    
    if (pw |> as.character() |> grepl("Piecewise", x=_)){
        pw <- sympy_func(pw, "piecewise_fold")    
        ll <- pw$pyobj$as_expr()$args ## A list

        out <- lapply(ll, function(z){
            vv <- list(expr=z[[0]], cond=z[[1]])
            ww <- lapply(vv, as_sym) |> lapply(do_logicals)
            ww
        })
        
        class(out) <- c("caracas_piecewise", class(out))
        return(out)
    } else {
        return(pw)
    }
}

##' @export
##' @rdname piecewise
piecewise_cond <- function(pw){
    out <-
        lapply(pw, function(z) {
            z$cond
        })
    out
}

##' @export
##' @rdname piecewise
piecewise_expr <- function(pw){
    out <-
        lapply(pw, function(z) {
            z$expr
        })
    out
}

