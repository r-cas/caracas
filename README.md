# `caracas`: Computer algebra in R

<!-- badges: start -->
[![R build status](https://github.com/r-cas/caracas/workflows/R-CMD-check/badge.svg)](https://github.com/r-cas/caracas/actions) 
[![codecov.io](https://codecov.io/gh/r-cas/caracas/branch/master/graphs/badge.svg)](https://codecov.io/gh/r-cas/caracas?branch=master)
<!-- badges: end -->
  
## Installation

`caracas` is available on CRAN and can be installed as usual:

```
install.packages('caracas')
```

To build and install from Github with vignettes run this command from within `R` (please install `remotes` first if not already installed):

```
# install.packages('remotes')
remotes::install_github("r-cas/caracas", build_vignettes = TRUE)
```

You can also install the package without vignettes if needed as follows:

```
remotes::install_github("r-cas/caracas")
```

## Development site

See <https://github.com/r-cas/caracas>.

## Online documentation

See <https://r-cas.github.io/caracas/>.

## Origin of name

The name "caracas" is intended to mean "(inter)face to computer algebra system(s)" - notice that "cara" is Spanish (Castellano to be precise) for "face".

## Code of conduct

Please note that the `caracas` project is released with a Contributor Code of Conduct (available in `CODE_OF_CONDUCT.md`). By contributing to this project, you agree to abide by its terms.
