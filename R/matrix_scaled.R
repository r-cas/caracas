#' Create list of factors as in a product
#'
#' @param X matrix
#' @param k scalar to be factored out
#' @param divide Should `X` be divided with `k` before constructing scaled matrix?
#'
#' @examples
#' if (has_sympy()) {
#'   V <- matrix_sym(2, 2, "v")
#'   a <- symbol("a")
#'   
#'   K <- a*V
#'   scale_matrix(K, a)
#'   scale_matrix(V, a, divide = FALSE)
#' 
#'   Ks <- scale_matrix(V, a, divide = FALSE)
#'   Ks
#'   W <- matrix_sym(2, 2, "w")
#'   unscale_matrix(Ks) %*% W
#'   unscale_matrix(Ks) %*% W |> scale_matrix(a)
#'   Ksi <- unscale_matrix(Ks) |> inv() |> scale_matrix(a/det(unscale_matrix(Ks)))
#'   (Ksi |> unscale_matrix()) %*% (Ks |> unscale_matrix()) |> simplify()
#'   tex(Ksi)
#' }
#' 
#' @concept linalg
#' 
#' @export
scale_matrix <- function(X, k = NULL, divide = TRUE) {
  ensure_sympy()
  stopifnot_symbol(X)

  if (is.null(k)) {
    out <- list(mat = X, scale = as_sym("S(1)"))
    class(out) <- c("caracas_scaled_matrix", "list")
    return(out)
  }
  
  if (!inherits(k, "caracas_symbol")) {
    #k <- as_sym(k)
    k <- as_sym(paste0("S(", k, ")"))
  }
  
  k <- simplify(k)
  
  out <- if (divide) {
    list(mat = X/k, scale = k)
  } else {
    list(mat = X, scale = k)
  }
  
  out$mat <- simplify(out$mat)
  class(out) <- c("caracas_scaled_matrix", "list")
  out
}

#' Extract matrix from scaled matrix
#'
#' @param X scaled matrix created with [scale_matrix()]
#'
#' @examples
#' if (has_sympy()) {
#'   V <- matrix_sym(2, 2, "v")
#'   a <- symbol("a")
#'   Ks <- scale_matrix(V, a, divide = FALSE)
#'   Ks
#'   unscale_matrix(Ks)
#'   V %*% a
#' }
#' 
#' @concept linalg
#' 
#' @export
unscale_matrix <- function(X) {
  ensure_sympy()
  
  if (!inherits(X, "caracas_scaled_matrix")) {
    stop("Expected caracas_scaled_matrix")
  }
  
  X$scale * X$mat
}



#' Print scaled matrix
#' 
#' @param x A `caracas_scaled_matrix`
#' @param \dots Passed to [print.caracas_symbol()]
#'
#' @concept output
#' 
#' @export
print.caracas_scaled_matrix <- function(x, ...) {
  ensure_sympy()
  
  if (!inherits(x, "caracas_scaled_matrix")) {
    stop("Expected caracas_scaled_matrix")
  }
  
  z <- c(as.character(x$scale), as.character(x$mat))
  
  dots <- list(...)
  
  if ("method" %in% names(dots) && dots[["method"]] == "compactascii") {
    prompt <- getOption("caracas.prompt", default = "c: ")
    rowvec <- getOption("caracas.print.rowvec", default = TRUE)
    
    if ("prompt" %in% names(dots)) {
      prompt <- dots[["prompt"]]
    }
    
    if ("rowvec" %in% names(dots)) {
      rowvec <- dots[["rowvec"]]
    }
    
    z1 <- get_caracas_out(x$scale,
                          prompt = prompt, 
                          method = "compactascii", 
                          rowvec = rowvec)
    z2 <- get_caracas_out(x$mat,
                          prompt = prompt, 
                          method = "compactascii", 
                          rowvec = rowvec)
    
    # scale on its own line:
    z2 <- gsub(paste0("^", prompt),
               paste0(rep(" ", nchar(prompt)), collapse = ""),
               z2)
    cat(z1, " * \n", z2, "\n", sep = "")
    
    # first row of matrix at same line as scale:
    # z2 <- gsub(paste0("^", prompt), "", z2)
    # indent <- paste0(rep(" ", nchar(z1) - nchar(prompt) + 3), collapse = "")
    # z2 <- gsub("\n", paste0("\n", indent), z2)
    # cat(z1, " * ", z2, "\n", sep = "")
    
    return(invisible(x))
  } 
  
  w <- paste0("UnevaluatedExpr(", z, ")", collapse = "*")
  v <- eval_to_symbol(w)
  print(v, ...)

  return(invisible(x))
}

#' Export scaled matrix to tex
#'
#' @param x scaled matrix
#' @param \dots Other arguments passed along
#' @concept output
#' @export
tex.caracas_scaled_matrix <- function(x, ...){
  if (!inherits(x, "caracas_scaled_matrix")) {
    stop("Expected caracas_scaled_matrix")
  }
  
  a <- c(tex(x$scale), tex(x$mat))
  o <- paste(a, collapse = "  ")
  o
}
