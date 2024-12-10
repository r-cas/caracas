indent_not_first_line <- function(x, indent = 0) {
  spaces <- paste0(rep(" ", indent), collapse = '')
  
  # If newline, multiple lines:
  if (grepl("\n", x)) {
    
    x <- gsub("\n", paste0("\n", spaces), x)
  }
  
  #x <- paste0(spaces, x)
  
  return(x)
}

get_caracas_out <- function(x, 
                            prompt = getOption("caracas.prompt", default = "c: "), 
                            method = getOption("caracas.print.method", default = "utf8"),
                            rowvec = getOption("caracas.print.rowvec", default = TRUE)) {
  ensure_sympy()
  
  if (is.null(x$pyobj)) {
    stop("Unexpected")
  }
  
  method <- match.arg(method, choices = c("utf8", "prettyascii", "ascii", "compactascii"))
  
  py <- get_py()
  
  suffix <- ""
  
  out <- if (method == "prettyascii") {
    # 'prettyascii'
    if (rowvec && symbol_is_matrix(x) && ncol(x) == 1L && nrow(x) > 1L) {
      #suffix <- intToUtf8(7488L) # T utf-8
      suffix <- "^T"
      #reticulate::py_capture_output(get_sympy()$pprint(t(x)$pyobj, use_unicode = FALSE))
      reticulate::py_capture_output(py$print_caracas(t(x)$pyobj))
    } else {
      #reticulate::py_capture_output(get_sympy()$pprint(x$pyobj, use_unicode = FALSE))
      reticulate::py_capture_output(py$print_caracas(x$pyobj))
    }
  } else if (method == "ascii") {
    # 'ascii'
    python_strings_to_r(get_sympy()$sstr(x$pyobj))
  } else if (method == "compactascii") {
    # 'compactascii'
    
    suffix <- ""
    obj <- x$pyobj
    
    if (rowvec && symbol_is_matrix(x) && ncol(x) == 1L && nrow(x) > 1L) {
      suffix <- "^T"
      obj <- obj$T
    }
    
    z <- python_strings_to_r(get_sympy()$sstr(obj))
    if (symbol_is_matrix(x)) {
      z <- gsub("\\n\\[", "\n [", z)
      z <- gsub("^Matrix\\(\\[\\n \\[", "[[", z)
      z <- gsub("\\)$", "", z)
      
      z <- gsub("^Matrix\\(\\[", "[", z)
      #z <- gsub("Matrix\\(\\[(.*)\\]\\)", "[(\\1)]", z)
      #z <- gsub("Matrix\\(", "", z)
    } else if (grepl("*Matrix", z)) {
      # Probably caracas_scaled_matrix, but we do not have x's class
      z <- gsub("Matrix\\((.*)\\)", "\\1", z)
    }
    z
    
  } else {
    stopifnot(method == "utf8")
    
    # 'utf8'
    if (rowvec && symbol_is_matrix(x) && ncol(x) == 1L && nrow(x) > 1L) {
      suffix <- intToUtf8(7488L) # T utf-8
      #reticulate::py_capture_output(get_sympy()$pprint(t(x)$pyobj))
      reticulate::py_capture_output(py$print_caracas_unicode(t(x)$pyobj))
    } else {
      #reticulate::py_capture_output(get_sympy()$pprint(x$pyobj))
      reticulate::py_capture_output(py$print_caracas_unicode(x$pyobj))
    }
  }
  
  out <- gsub("[ \n]+$", "", out) 
  
  if (nchar(prompt) > 0) {
    out <- indent_not_first_line(out, indent = nchar(prompt))
    out <- paste0(prompt, out)
  }
  
  out <- paste0(out, suffix)
  
  return(out)
}

#' Print symbol
#' 
#' @param x A `caracas_symbol`
#' @param prompt Which prompt/prefix to print (default: 'c: ')
#' @param method What way to print (`utf8`, `prettyascii`, `ascii`, `compactascii`)
#' @param rowvec `FALSE` to print column vectors as is
#' @param \dots not used
#'
#' @concept output
#' 
#' @export
print.caracas_symbol <- function(x, 
                                 prompt = getOption("caracas.prompt", default = "c: "), 
                                 method = getOption("caracas.print.method", default = "utf8"), 
                                 rowvec = getOption("caracas.print.rowvec", 
                                                               default = TRUE),
                                 ...) {
  
  out <- get_caracas_out(x, 
                         prompt = prompt,
                         method = method, 
                         rowvec = rowvec)
  out <- paste0(out, "\n")
  cat(out)
  
  return(invisible(x))
}

#' Print solution
#' 
#' @param x A `caracas_symbol`
#' @param simplify Print solution in a simple format
#' @param \dots Passed to [print.caracas_symbol()]
#'
#' @concept output
#' 
#' @examples 
#' if (has_sympy()) {
#'   x <- symbol('x')
#'   solve_sys(x^2, -1, x)
#'   
#'   y <- symbol("y")
#'   lhs <- cbind(3*x*y - y, x)
#'   rhs <- cbind(-5*x, y+4)
#'   sol <- solve_sys(lhs, rhs, list(x, y))
#'   sol
#' }
#' 
#' @export
print.caracas_solve_sys_sol <- function(x, 
                                        simplify = getOption("caracas.print.sol.simplify", default = TRUE), 
                                        ...) {
  ensure_sympy()
  
  if (simplify) {
    if (length(x) == 0L) {
      cat("No solutions\n")
      return(invisible(x))
    }
    
    num_vars <- length(names(x[[1L]]))
    # If there is only one variable, do more compact printing:
    
    if (num_vars == 1L) {
      nm <- names(x[[1L]])
      
      for (i in seq_along(x)) {
        val <- get_caracas_out(x[[i]][[1L]], prompt = "", ...)
        cat(nm, " = ", val, "\n", sep = "")
      }
    } else {
      # More variables:
      for (i in seq_along(x)) {
        cat("Solution ", i, ":\n", sep = "")
        #print(i)
        
        nms <- names(x[[i]])
        nms <- sprintf(paste0("%-", max(nchar(nms)), "s"), nms)
        
        vals <- lapply(x[[i]], get_caracas_out, prompt = "", ...)
        
        prefix <- "  "
        nms <- paste0(prefix, nms, " = ")
        
        #vals <- lapply(vals, function(l) paste0(prefix, l))
        vals <- lapply(seq_along(vals), function(j) {
          indent_not_first_line(vals[[j]], nchar(nms[j]))
        })
        
        for (j in seq_along(nms)) {
          cat(nms[j], vals[[j]], "\n", sep =)
        }
        
        # xi <- paste0(nms, " = ", vals)
        # xi <- paste0("{", paste0(xi, collapse = ", "), "}")
        # cat(xi, "\n", sep = "")
      }
    }

  } else {
    y <- x
    class(y) <- setdiff(class(y), "caracas_solve_sys_sol")
    print(y)
  }
  
  return(invisible(x))
}



#' Export object to TeX
#'
#' @param x A `caracas_symbol`
#' @param zero_as_dot Print zero as dots
#' @param matstr Replace `\begin{matrix}` with another environment, e.g. `pmatrix`. 
#' If vector of length two, the second element is an optional argument.
#' @param \dots Other arguments passed along
#'
#' @concept output
#' 
#' @examples
#' if (has_sympy()) {
#' S <- matrix_sym_symmetric(3, "s")
#' S[1, 2] <- "1-x"
#' S
#' tex(S)
#' tex(S, matstr = "pmatrix")
#' tex(S, matstr = c("pmatrix", "r"))
#' }
#'
#' @export
tex <- function(x, zero_as_dot = FALSE, matstr = NULL, ...) {
  UseMethod("tex")
}




#' @export
tex.caracas_symbol <- function(x, zero_as_dot = FALSE, matstr = NULL, ...) {
    ensure_sympy()

    ## cat("x:\n"); print(x)
    if (!is.null(x$pyobj)) {
        o <- get_sympy()$latex(x$pyobj)
        
        if (zero_as_dot) {
                                        # Matrices
            o <- gsub("([^0-9])0([^0-9])", "\\1.\\2", o)
                                        # FIXME:
                                        # Replaces e0 in matrix
        }
        
        if (!is.null(matstr) && is.character(matstr) && length(matstr) >= 1L) { 
            opt <- ifelse(length(matstr) == 2L, paste0("[", matstr[2L], "]"), "")
            
            o <- gsub("\\begin{matrix}", paste0("\\begin{", matstr[1L], "}", opt), o, fixed = TRUE)
            o <- gsub("\\end{matrix}", paste0("\\end{", matstr[1L], "}"), o, fixed = TRUE)
        }
        
        return(o)
    }
  
  # if (!is.null(x$pyobj)) {
  #   py <- get_py()
  #   o <- reticulate::py_capture_output(py$print_caracas_latex(x$pyobj))
  #   return(o)
  # }
  
  stop("Unexpected")
}




#' Export object to TeX
#'
#' @param \dots Objects to be put in tex for. Can be `caracas_symbol`s
#'     and other (simple) R objects (atomics, dataframes, matrices).
#' @param x A such objects described above.
#' @param zero_as_dot Print zero as dots
#' @param matstr Replace `\begin{matrix}` with another environment,
#'     e.g. `pmatrix`.  If vector of length two, the second element is
#'     an optional argument.
#' 
#'
#' @concept output
#'
#' @examples
#' if (has_sympy()) {
#'   X <- matrix_sym(4,2,"a")
#'   b <- vector_sym(2,"b")
#'   y <- vector_sym(4,"y")
#'   tex_list(y, "=", X, b)
#'   tex_list(x=list(y, "=", X, b))
#'
#'   M <- iris[1:3, 1:2]
#'   tex_list(M, "+", M, "=", M + M)
#'   tex_list(x=list(M, "+", M, "=", M + M))
#' 
#' }
#' @export
tex_list <- function(..., x=NULL, zero_as_dot=FALSE, matstr=NULL){
    ensure_sympy()
    x <- c(list(...), x)
    ## xx <<- x
    ## print(x)
    if (!is.list(x)){
        stop("'x' must be a list")
    }

    ## cat("x:\n"); print(x)
    o <- sapply(x, function(z){
        ## cat("z:\n"); print(z)

        if (inherits(z, c("matrix", "data.frame"))){
            z <- as_sym(as.matrix(z))
        }
 
        if (is_sym(z)){
            return(tex(z, zero_as_dot=zero_as_dot, matstr=matstr))
        }

        if (is.atomic(z)){
            return(z)
        }

        stop("Unexpected input")
        })
           
    out <- paste0(o, collapse="\n")
    out
}





#' Convert symbol to character
#'
#' @param x A `caracas_symbol`
#' @param replace_I Replace constant I (can both be identity and imaginary unit)
#' @param \dots not used
#'
#' @concept output
#'
#' @export
as.character.caracas_symbol <- function(x, replace_I = TRUE, ...) {
  y <- as.character(x$pyobj)
  y <- python_strings_to_r(y, replace_I = replace_I)
  return(y)
}


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
#' @concept output
#' 
#' @export
texshow <- function(x){#, name="obj"){
  if (!requireNamespace("tinytex", quietly = TRUE) ||
      !requireNamespace("magick", quietly = TRUE) ||
      !requireNamespace("pdftools", quietly = TRUE) ||
      !requireNamespace("qpdf", quietly = TRUE)) {
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
  
  st_all <- paste0(c(s1, st, s2), collapse = " ")
  
  cat(st_all, file = tex_name)

  out <- tinytex::pdflatex(tex_name)

  im <- magick::image_read_pdf(out)
  im_content <- magick::image_trim(im)
  plot(im_content)
  
  return(invisible(NULL))
}
