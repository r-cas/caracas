
#' Structured sums for Toeplitz, AR(1), and heterogeneous AR(1) matrices. 
#'
#' @description
#' Utilities for computing symbolic (or numeric) sums of matrix entries for
#' common structured matrices, without explicitly constructing the full matrix.
#'
#' \describe{
#' \item{\code{toeplitz_sum()}}{Sum of entries in a banded symmetric Toeplitz matrix
#' with diagonal values given by \code{r = (r0, r1, r2, ...)}.}
#' \item{\code{ar1_sum()}}{Sum of entries in an \eqn{n \times n} AR(1) correlation
#' matrix \eqn{R_{ij} = \rho^{|i-j|}}.}
#' \item{\code{har1_sum()}}{Sum of entries in a heterogeneous AR(1) covariance matrix
#' \eqn{Sigma = D R D} where \eqn{D=\mathrm{diag}(sigma_1,\ldots,sigma_n)} and
#' \eqn{R_{ij} = \rho^{|i-j|}}, i.e. \eqn{Sigma_{ij}=sigma_isigma_j\rho^{|i-j|}}.}
#' }
#'
#' 
#' This is returned expanded in terms of the provided \eqn{sigma} vector.
#'
#' @param n Integer (or symbolic) matrix dimension for \code{toeplitz_sum()} and \code{ar1_sum()}.
#' @param r A caracas symbolic vector (or numeric vector) \code{(r0, r1, r2, ...)} giving diagonal
#'   values for the symmetric Toeplitz structure. \code{r[1]} is the main diagonal.
#' @param rho AR(1) parameter (symbolic or numeric).
#' @param sigma A caracas symbolic vector (or numeric vector) \code{(sigma_1,\ldots,sigma_n)}.
#' @param upper Logical; if \code{TRUE}, sum over the upper triangle (\eqn{i<j}) only.
#'   If \code{FALSE}, sum over all off-diagonals (both upper and lower).
#' @param diag Logical; include the diagonal contribution if \code{TRUE}.
#'
#' @return
#' A caracas symbolic expression (typically a scalar), or a numeric value if inputs are numeric.
#'
#' @examples
#' if (has_sympy()) {
#' ## Toeplitz: sum of all entries in an n x n banded Toeplitz matrix
#' def_sym(n, r0, r1, r2)
#' r <- c(r0, r1, r2)  # diagonals: r0 on main, r1 on first, r2 on second
#' toeplitz_sum(n, r)
#' toeplitz_sum(n, r, upper = TRUE, diag = FALSE)  # strictly upper triangular sum
#'
#' ## AR(1) correlation: R_ij = rho^|i-j|
#' def_sym(n, rho)
#' ar1_sum(n, rho)
#' ar1_sum(n, rho, upper = TRUE, diag = FALSE)
#'
#' ## Heterogeneous AR(1): Sigma = diag(sigma) %*% AR1 %*% diag(sigma)
#' def_sym(rho, sigma_1, sigma_2, sigma_3, sigma_4)
#' sigma <- c(sigma_1, sigma_2, sigma_3, sigma_4)
#' simplify(har1_sum(rho, sigma))
#'
#' ## Numeric check (small n)
#' ar1_sum(5, 0.2)
#' }
#' @name structured_sums
NULL

#' @rdname structured_sums
#' @export
toeplitz_sum <- function(n, r, upper = FALSE, diag = TRUE) {
  m <- prod(dim(r)) - 1 ## Ugly that length does not work...
  S <- 0

  if (diag) {
    S <- S + n * r[1]
  }

  for (k in 1:m) {
    coeff <- if (upper) (n - k) else 2 * (n - k)
    S <- S + coeff * r[k + 1]
  }
  return(S)
}

#' @rdname structured_sums
#' @export
ar1_sum <- function(n, rho, upper = FALSE, diag = TRUE) {
  #caracas::def_sym(k)
  k <- caracas::symbol("k")
  S <- caracas::as_sym(0)

  if (diag) S <- S + n
  coeff <- if (upper) 1 else 2

  S <- S + coeff * sum_((n - k) * rho^k, var = k, lower = 1, upper = n - 1)
  simplify(S)
}

#' @rdname structured_sums
#' @export
har1_sum <- function(rho, sigma) {
  n <- prod(dim(sigma))
  S <- 0

  for (i in 1:n) {
    S <- S + sigma[i]^2
  }

  for (k in 1:(n - 1)) {
    inner <- 0
    for (i in 1:(n - k)) {
      inner <- inner + sigma[i] * sigma[i + k]
    }
    S <- S + 2 * rho^k * inner
  }

  S
}


## ' @details
## ' \strong{Toeplitz.} Let \eqn{T_{ij}=r_{|i-j|}} for \eqn{|i-j| \le m} and zero otherwise,
## ' where \code{r} contains \eqn{(r_0,\ldots,r_m)}. The function sums contributions by
## ' counting the number of elements on each diagonal.
## '
## ' \strong{AR(1).} Uses the diagonal counting identity
## ' \deqn{\sum_{i,j} R_{ij} = n + 2 \sum_{k=1}^{n-1} (n-k)\rho^k,}
## ' with options to restrict to the upper triangle and/or exclude the diagonal.
## '
## ' \strong{Heterogeneous AR(1).} For a given vector \eqn{sigma}, the sum is
## ' 
## ' \deqn{\sum_{i=1}^{n} sigma_{i}^2 \\
## ' \quad + 2\sum_{k=1}^{n-1}\rho^k \sum_{i=1}^{n-k}sigma_i sigma_{i+k}.}

