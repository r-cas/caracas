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

#' Print symbol
#' 
#' @param x A `caracas_symbol`
#' @param \dots not used
#'
#' @concept output
#' 
#' @export
print.caracas_symbol <- function(x, ...) {
  ensure_sympy()
  
  if (is.null(x$pyobj)) {
    stop("Unexpected")
  }
  
  out <- if (print_type == 'pretty_utf8') {
    reticulate::py_capture_output(get_sympy()$pprint(x$pyobj))
  } else {
    # 'string'
    python_strings_to_r(get_sympy()$sstr(x$pyobj))
  }

  out <- gsub("[ \n]+$", "", out)
  
  # If newline, multiple lines:
  if (grepl("\n", out)) {
    spaces <- paste0(rep(" ", nchar('[caracas]: ')), collapse = '')
    out <- gsub("\n", paste0("\n", spaces), out)
  } 
  
  cat("[caracas]: ", out, "\n", sep = "")
  
  return(invisible(x))
}


#' Export object to TeX
#'
#' @param x A `caracas_symbol`
#'
#' @concept output
#'
#' @export
tex <- function(x) {
  UseMethod("tex")
}

#' @export
tex.caracas_symbol <- function(x) {
  ensure_sympy()
  
  if (!is.null(x$pyobj)) {
    return(get_sympy()$latex(x$pyobj))
  }
  
  stop("Unexpected")
}

#' Convert symbol to character
#'
#' @param x A `caracas_symbol`
#' @param \dots not used
#'
#' @concept output
#'
#' @export
as.character.caracas_symbol <- function(x, ...) {
  y <- as.character(x$pyobj)
  y <- python_strings_to_r(y)
  return(y)
}
