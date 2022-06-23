

#' Dump latex representation of sympy object.
#'
#' Dump latex representation of sympy object and compile document into pdf.
#'
#' @param x An object that can be put in latex format with caracas' tex() function.
#' @param name Name of tex file.
#' @return Nothing, but a .tex file and a .pdf file is generated.
#' 
#' @examples
#' S <- matrix_sym_symmetric(3, "s")
#' \dontrun{
#' texdump(S)
#' }
#'
#' @importFrom tinytex pdflatex
#' @export
texdump <- function(x, name="obj"){

    tex_name <- paste0("_dump_", name, ".tex", collapes="")    
    s1 <- c("\\documentclass{article}",
            "\\usepackage{amsmath}", 
            "\\begin{document}",
            "\\["
            )
    
    s2 <- c("\\]",
            "\\end{document}")
    
    s1 <- paste0(s1, "\n")
    s2 <- paste0(s2, "\n")

    st <- tex(x)
    st_all <- paste0(c(s1, st, s2), collapse=" ")

    cat(st_all, file=tex_name)
    if (requireNamespace("tinytex", quietly = TRUE))
        tinytex::pdflatex(tex_name)
    invisible()
}



#' All variables
#'
#' Return all variables in caracas symbol
#'
#' @param x caracas symbol
#'
#' @examples
#'
#' x <- vector_sym(5)
#' all_vars(x)
#' 
#' @export
all_vars <- function(x){
    all.vars(as_expr(x))
}


## #' Convert caracas object to R (col or row wise)
## #'
## #' Convert caracas object to R (col or row wise)
## #'
## #' @param x caracas_symbol
## #' @param first_doit Try `doit()` first
## #' @param column_major
## #' 
## #' @export
## as_expr2 <- function(x, first_doit = TRUE, column_major=TRUE) {
##   UseMethod("as_expr2")
## }

## #' @export
## as_expr2.default <- function(x, first_doit = TRUE, column_major=TRUE) {
##   return(x)
## }

## #' @export
## as_expr2.caracas_symbol <- function(x, first_doit = TRUE, column_major=TRUE) {
##     if (column_major && symbol_is_matrix(x)){
##         as_expr(t(x))        
##     } else
##     {
##         as_expr(x)        
##     }
## }





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
#' @export
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
