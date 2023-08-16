#' Pipe
#'
#' Pipe operator
#'
#' @param lhs,rhs specify what lhs and rhs are
#' @importFrom magrittr %>%
#' @name %>%
#' @rdname pipe
#' @export
NULL


# This was created, or else CRAN complained about that the first example
# run took too long - because that was the first call that 
# actually prepared SymPy
.onLoad <- function(libname, pkgname){
  silent_prepare_sympy() 
  
  # Prepare chunk engine ''
  if (requireNamespace("knitr", quietly = TRUE)) {
    # Avoid global variables
    local({
      prefix <- "cat(paste0('$$', tex("
      postfix <- "), '$$'))"
      
      R_engine  <- knitr::knit_engines$get("R")
      knitr::knit_engines$set(rtex = function(options) {
        if (length(options$code) == 0L) {
          return(R_engine(options))
        }
        
        # First version is to assume that the chunk is one line and do:
        # options$code <- paste0("cat(paste0('$$', tex(", options$code, "), '$$'))")
        # options$results <- "asis"
        # y <- R_engine(options)
        # if (options$echo) {
        #  y <- gsub("cat(paste0('$$', tex(", "", y, fixed = TRUE)
        #  y <- gsub("), '$$'))", "", y, fixed = TRUE)
        # }
        # y
        
        # To support non-print lines and multiple outputs, 
        # we need multiple steps:
        # 1) Disable tidy
        # 2) Run 'R_engine' and find matching lines
        org_tidy <- options$tidy
        org_echo <- options$echo
        
        options$tidy <- FALSE
        options$echo <- TRUE
        
        input <- options$code
        output <- R_engine(options)
        o_l <- strsplit(output, "\n", fixed = TRUE)[[1L]]
        
        # Remove empty lines and chunk beginnings/endings:
        input <- input[nchar(input) > 0]
        
        o_l <- o_l[nchar(o_l) > 0]
        o_l <- o_l[!grepl("^[ ]*```rtex", o_l)]
        o_l <- o_l[!grepl("[ ]*```$", o_l)]
        

        # ###
        # cat("===== input> =====\n")
        # print(input)
        # cat("===== <input =====\n")
        # 
        # cat("===== output> =====\n")
        # print(o_l)
        # cat("===== <output =====\n")
        # 
        
        #print(o_l)
        
        # Detect output lines, i.e. lines in output that are not in input.
        # They are used because the code preceding those lines
        # need to be wrapped with tex() as they cause the output.
        # FIXME: This can probably be optimised.
        #        E.g.: Instead of searching all input, only allow each input
        #              one time.
        
        # Which of o_l are output?
        candidates <- seq_along(input)
        lines_output <- logical(length(o_l))
        
        for (i in seq_along(o_l)) {
          idx <- match(o_l[i], input[candidates], incomparables = c(""))
          
          # If line not found in input, then it's output:
          if (length(idx) == 1L && is.na(idx)) {
            lines_output[i] <- TRUE
          } else {
            candidates <- candidates[-idx]
          }
        }
        #print(lines_output)
        #cat("===== done =====")
        
        # Only code lines with output after needs mod
        lines_needing_mod <- logical(length(o_l))
        
        for (i in seq_along(o_l)[-1L]) {
          if (lines_output[i] && !lines_output[i-1L]) {
            lines_needing_mod[i-1L] <- TRUE
          }
        }
        
        # for (i in seq_along(o_l)) {
        #   p1 <- if (lines_output[i]) "OUTPUT" else "CODE  "
        #   p2 <- if (lines_needing_mod[i]) "MOD " else "    "
        #   p <- paste0(p1, p2)
        #   cat(p, ": ", o_l[i], "\n", sep = "")
        # }
        
        input_generating_tex <- match(o_l[lines_needing_mod], input)
        #print(input_generating_tex)
        new_input <- input
        new_input[input_generating_tex] <- paste0(prefix, new_input[input_generating_tex], postfix)

        new_options <- options
        new_options$code <- new_input
        new_options$results <- "asis"
        new_options$engine <- "r" # for syntax highlighting
        new_options$echo <- org_echo
        new_output <- R_engine(new_options)
        
        if (options$echo) {
          new_output <- gsub(prefix, "", new_output, fixed = TRUE)
          new_output <- gsub(postfix, "", new_output, fixed = TRUE)
        }
        
        #cat("\n\n\n\n")
        #cat("====================\n")
        #print(new_output)
        #cat("====================\n")
        #cat("\n\n\n\n")
        
        return(new_output)
      })
    })
  }
}
