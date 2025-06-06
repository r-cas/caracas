% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculus.R
\name{drop_remainder}
\alias{drop_remainder}
\title{Remove remainder term}
\usage{
drop_remainder(x)
}
\arguments{
\item{x}{Expression to remove remainder term from}
}
\description{
Remove remainder term
}
\examples{
if (has_sympy()) {
  def_sym(x)
  f <- cos(x)
  ft_with_O <- taylor(f, x0 = 0, n = 4+1)
  ft_with_O
  ft_with_O |> drop_remainder() |> as_expr()
}

}
\seealso{
\code{\link[=taylor]{taylor()}}
}
\concept{calculus}
