#' All variables
#'
#' Return all variables in caracas symbol
#'
#' @param x caracas symbol
#'
#' @examples
#'
#' x <- vector_(5)
#' all_vars(x)
#' 
#' @export

all_vars <- function(x){
    all.vars(as_expr(x))
}



#' Get basis
#'
#' Get basis
#'
#' @param x caracas vector / matrix
#' @examples
#' x <- vector_sym(3)
#' get_basis(x)
#' 
#' W <- matrix(c("r_1", "r_1", "r_2", "r_2", "0", "0", "u_1", "u_2"), nrow=4)
#' W <- as_sym(W)
#' get_basis(W)

get_basis <- function(x){
    ensure_sympy()
    zz <- as_character_matrix(x)
    ##unique symbols
    us <- setdiff(unique(as.character(zz)), "0")
    out <- lapply(seq_along(us),
                  function(i){
                      1*(us[i] == zz)           
                  })
    names(out) <- us
    ## attr(out, "symbols") <- us
    out
}
