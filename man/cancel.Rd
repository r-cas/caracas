% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simplify.R
\name{cancel}
\alias{cancel}
\title{Put rational function into standard form}
\usage{
cancel(x)
}
\arguments{
\item{x}{A \code{caracas_symbol}}
}
\description{
cancel() will take any rational function and put it into the standard canonical form
}
\examples{
if (has_sympy()){
  def_sym(x, y, z)
  expr = cancel((x**2 + 2*x + 1)/(x**2 + x))
  cancel(expr)
  expr = (x*y**2 - 2*x*y*z + x*z**2 + y**2 - 2*y*z + z**2)/(x**2 - 1)
  cancel(expr)
  factor_(expr)
}
}
\concept{simplify}
