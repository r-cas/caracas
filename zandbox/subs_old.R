#
##' Substitute symbol for value
##' 
##' @param s Expression
##' @param x Name of symbol (character)
##' @param v Value for `x`
##' 
##' @examples 
##' if (has_sympy()) {
##'    x <- symbol('x')
##'    e <- 2*x^2
##'    e
##'    subs_single(e, "x", "2")
##'    y <- as_sym("2")
##'    subs_single(e, "x", y)
##'    b1 <- symbol("a"); str(b1)
##'    b2 <- b1 + 1; str(b2)
##'    b3 <- subs(b2, "a", "k");
##'    str(b3)
##' }
##' 
##' @seealso [subs], [subs_lst()]
##' 
##' @concept caracas_symbol
##' 
##' @export
#subs_single <- function(s, x, v) {
#  ensure_sympy()
#  
#  sym <- as.character(x)
#  
#  val <- if (inherits(v, "caracas_symbol")) {
#    v$pyobj
#  } else {
#    v
#  }
#  
#  y <- construct_symbol_from_pyobj(s$pyobj$subs(sym, val))
#  return(y)
#}
#
#
#
##' Substitute af vector of symbols for a vector of values
##' 
##' @param s Expression
##' @param x Names of symbol (vector)
##' @param v Values for `x` (vector)
##' 
##' @examples 
##' if (has_sympy()) {
##'    x <- as_sym(paste0('x', 1:3))
##'    e <- 2*x^2
##'    e
##'    subs(e, x, 1:3)
##'    subs(e, x, x^2)
##'    
##' }
##' 
##' @seealso [subs_single()], [subs_lst()]
##' @concept caracas_symbol
##' 
##' @export
#subs <- function(s, x, v) {
#  ensure_sympy()
#  
#  if (is.character(x)){
#    x <- as_sym(matrix(x))
#  }
#  
#  if (any(dim(x) > 1)){
#    x <- as_sym(matrix(c(as_character_matrix(x))))
#  }
#  
#  if (!symbol_is_matrix(x)){ ## FIXME SH hack
#    x <- cbind(x)
#  }
#  
#  stopifnot_matrix(x)
#  
#  if (is.character(v) || is.numeric(v)){
#    v <- as_sym(v)
#  }
#  
#  if (any(dim(v) > 1)){
#    v <- as_sym(c(as_character_matrix(v)))      
#  }
#  
#  
#  vv <- v
#  
#  if (inherits(v, "caracas_symbol") && symbol_is_matrix(v)) {
#    if (ncol(v) == 1L) {
#      vv <- lapply(seq_len(nrow(v)), function(i) v[i, ])
#    } else if (nrow(v) == 1L) {
#      vv <- lapply(seq_len(ncol(v)), function(i) v[, i])
#    } else {
#      stop("When v is a caracas matrix, one dimension must be 1")
#    }
#  }
#  
#  if (nrow(x) != 1L && ncol(x) != 1L) {
#    stop("x must have either 1 row or 1 column")
#  }
#  
#  if (ncol(x) > 1L) {
#    x <- t(x)
#  }
#  
#  if (length(vv) != nrow(x)) {
#    stop("Dimension mismatch")
#  }
#  
#  ss <- s
#  for (i in seq_along(vv)) {
#    ss <- subs_single(ss, x[i, ], vv[[i]])
#  }
#  
#  return(ss)
#}
#
##' Substitute symbol for of value given by a list
##' 
##' Useful for substituting solutions into expressions.
##' 
##' @param s Expression
##' @param x Named list of values OR vector of symbols.
##' @param v Vector of values.
##' 
##' @examples 
##' if (has_sympy()) {
##'      p <- as_sym(paste0("p", 1:3))
##'      y <- as_sym(paste0("y", 1:3))
##'      a <- as_sym("a")
##'      l <- sum(y*log(p))
##'      L <- -l + a*(sum(p) - 1)
##'      g <- der(L, c(a, p))
##'      sols <- solve_sys(g, c(a, p))
##'      sol <- sols[[1L]]
##'      sol
##'      H <- der2(L, c(p, a))
##'      H
##'      H_sol <- subs_lst(H, sol)
##'      H_sol
##'      
##'      p <- vector_sym(3, "p")
##'      y <- vector_sym(3, "y")
##'      subs_lst(p, list("p1" = 1, "p2" = y[1], "p3" = 0))
##'
##'      def_sym(aa, bb)
##'      cc <- aa + bb
##'      # Short
##'      subs_lst(cc, c(aa, bb), c(1122, aa^2+bb))
##'      # Same as
##'      l <- as.list(c(1122, aa^2+bb))
##'      names(l) <- all_vars(c(aa, bb))
##'      subs_lst(cc, l)
##' }
##' 
##' @seealso [subs()]
##' 
##' @concept caracas_symbol
##' 
##' @export
#subs_lst <- function(s, x, v) {
#  ensure_sympy()
#  
#  if ((!inherits(x, "list")) && (!missing(v))){
#    nms <- all_vars(x)
#    x <- as.list(v)
#    names(x) <- nms      
#  }
#  
#  x <- lapply(x, as.character)
#  nms <- names(x)
#  x <- as.character(x)
#  
#  st <- paste0("{", paste0(nms, ": ", x, collapse=", "), "}")
#  e <- reticulate::py_eval(st)
#  
#  new_s <- construct_symbol_from_pyobj(s$pyobj$subs(e))
#  
#  return(new_s)
#}
#
#
#
## subs_lst <- function(s, x) {
##   ensure_sympy()
##   
##   new_s <- s
##   
##   for (i in seq_along(x)) {
##     new_s <- subs_single(new_s, names(x)[i], x[[i]])
##   }
##   
##   return(new_s)
## }
#