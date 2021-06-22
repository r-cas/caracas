---
title: 'caracas: Computer algebra in R'
authors:
- affiliation: 1
  name: Mikkel Meyer Andersen
  orcid: 0000-0002-0234-0266
- affiliation: 1
  name: Søren Højsgaard
  orcid: 0000-0002-3269-9552
date: "22 June 2021"
bibliography: paper.bib
tags:
- cas
- mathematics
- symbolic mathematics
- statistics
- tex
- latex
affiliations:
- index: 1
  name: Department of Mathematical Sciences, Aalborg University, Denmark
---

# Summary

`caracas` is an `R` [@R] package that enables a 
computer algebra system (CAS) within `R` via the open source Python 
CAS `SymPy` [@sympy], which is made possible via `reticulate` [@reticulate]. 
`caracas` is published at The Comprehensive R Archive Network (CRAN) [@R] at <https://cran.r-project.org/package=caracas>, its source is available at <https://github.com/r-cas/caracas> and the documentation is available at <https://r-cas.github.io/caracas/>.

Much work went into integrating `caracas` into `R` such that `caracas` behaves much like 
other `R` libraries and objects. 

`caracas` contains a number of vignettes demonstrating both basic functionality like solving equations 
as well as more advanced tasks like finding the concentration and covariance matrix in a dynamic linear model. 

Compared to other CAS `R` packages like `Ryacas` [@Andersen2019] based on `yacas` [@yacas;@Pinkus2002] , 
`caracas` is more feature complete, for example with respect to solving equations.

# Statement of Need

From a statistician's perspective, `R` is excellent for data handling,
graphics, for model fitting and statistical inference and as a
programming environment. However, `R` largely lacks the ability to
perform symbolic computations. That is, `R` only support to a small
extent the step from posing a problem (for example a model) in
mathematical terms over symbolic manipulations of the model and further onto a stage where a model can be combined with data. The `caracas` provides capapilities for these steps directly in `R`. Topicas that can be handled in `caracas` include:

* Sums, 
* limits, 
* integration, 
* differentiation, 
* symbolic matrices and vectors,
* simplification of mathematical expressions and
* outputting in TeX format.

Several (commerical) systems are available for such tasks (and many more). However, we will argue that there is a virtue in being able to handle such tasks directly from within `R` using the familiar `R` syntax. Moreover, it is an integrated part of the design of `caracas` that it is straight forward to coerce a mathematical object into an `R` expression which can, e.g. be evaluated numerically. 


# Acknowledgements

We would like to thank the R Consortium for financial support for
creating the `caracas` package ([link to details on the funded project](https://www.r-consortium.org/projects/awarded-projects/2019-group-2#Symbolic+mathematics+in+R+with+SymPy)) and to users for pin pointing points
that can be improved in `caracas`.

# References
