print_type <- 'pretty_utf8'

# #' Set how printing is done
# #'
# #' @param type 'pretty_utf8', 'string'
# #'
# #' @export
# set_print_type <- function(type = c('pretty_utf8', 
#                                     'string')) {
#   print_type <<- match.arg(type)
# }

#' @export
print.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  
  if (is.null(x$pyobj)) {
    stop("Unexpected")
  }
  
  out <- if (print_type == 'pretty_utf8') {
    reticulate:::py_capture_output(sympy$pprint(x$pyobj))
  } else {
    # 'string'
    python_strings_to_r(sympy$sstr(x$pyobj))
  }

  out <- gsub("[ \n]+$", "", out)
  
  # If newline, multiple lines:
  if (grepl("\n", out)) {
    spaces <- paste0(rep(" ", nchar('[caracas]: ')), collapse = '')
    out <- gsub("\n", paste0("\n", spaces), out)
  } 
  
  cat("[caracas]: ", out, sep = "")
  
  return(invisible(x))
}


#' Export object to TeX
#'
#' @param x A `caracas_symbol`
#'
#' @concept caracas_symbol
#'
#' @export
tex <- function(x) {
  UseMethod("tex")
}

#' @export
tex.caracas_symbol <- function(x) {
  ensure_sympy()
  
  if (!is.null(x$pyobj)) {
    return(sympy$latex(x$pyobj))
  }
  
  return(x$content)
}
