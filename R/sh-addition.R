

#' Dump latex representation of sympy object.
#'
#' Dump latex representation of sympy object and compile document into pdf.
#'
#' @param x An object that can be put in latex format with caracas' tex() 
#'          function or a character string with tex code (in math mode).
#' @return Nothing, but a .tex file and a .pdf file is generated.
#' 
#' @examples
#' if (has_sympy()) {
#' S <- matrix_sym_symmetric(3, "s")
#' S
#' \dontrun{
#' texshow(S)
#' texshow(paste0("S = ", tex(S)))
#' }
#' }
#'
##@importFrom tinytex pdflatex
##@importFrom magick image_read_pdf image_trim
##@importFrom pdftools pdf_info
#' @export
texshow <- function(x){#, name="obj"){
  if (!requireNamespace("tinytex", quietly = TRUE) ||
      !requireNamespace("magick", quietly = TRUE) ||
      !requireNamespace("pdftools", quietly = TRUE)) {
    stop("This function requires tinytex, magick, and pdftools packages")
  }

    tex_name <- paste0(tempfile(), ".tex") #paste0("_dump_", name, ".tex", collapes="")    
    s1 <- c("\\documentclass{article}",
            "\\pagestyle{empty}",
            "\\usepackage{amsmath}", 
            "\\begin{document}",
            "\\["
            )
    
    s2 <- c("\\]",
            "\\end{document}")
    
    s1 <- paste0(s1, "\n")
    s2 <- paste0(s2, "\n")

    st <- if (inherits(x, "caracas_symbol")) {
      tex(x)
    } else {
      x
    }
    
    st_all <- paste0(c(s1, st, s2), collapse=" ")

    cat(st_all, file=tex_name)
    out <- tinytex::pdflatex(tex_name)
    
    im <- magick::image_read_pdf(out)
    im_content <- magick::image_trim(im)
    plot(im_content)
    return(invisible(NULL))
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
#' if (has_sympy()) {
#' x <- vector_sym(3)
#' get_basis(x)
#' 
#' W <- matrix(c("r_1", "r_1", "r_2", "r_2", "0", "0", "u_1", "u_2"), nrow=4)
#' W <- as_sym(W)
#' get_basis(W)
#' }
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
