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
