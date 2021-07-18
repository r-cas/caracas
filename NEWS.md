# caracas 1.1.2

* `sympy_func(x, fun)` first tries calling `fun` on `x`; and if it does not exist it tries from the global namespace
* `expand_func()` added
* Bug with `Ops` (functions) fixed

# caracas 1.1.1

* Journal of Open Source Software submission

# caracas 1.1.0

* Global symbol assignment by `def_sym()` (#18)
* Linear algebra: New `do_la()` function with convinience functions like `eigenval()`, `eigenvec()`, `QRdecomposition()`; new vignette demonstrating these
* Assumptions being made available, see e.g. `symbol()` and `ask()`
* Arbitrary precision arithmetic: `N()` function and vignette on 
  "Arbitrary precision arithmetic"
* Rename `eigen_val()`/`eigen_vec()` to `eigenval()`/`eigenvec()`
* More clear naming convention: R has expressions and caracas has symbols; 
  in this connection `as_r()` was renamed to `as_expr()` and 
  `as_symbol()` to `as_sym()`. Also, `as_sym()` changed argument from `declare_variables` to `declare_symbols`.
* Changed internals such that `der()`, `der2()` and `solve_sys()` now takes multiple variables with `list()` (or as a vector symbol) instead of `c()`; see also `matrify()` and `listify()`
* Added `diag_()` and `matrix_()` (postfix `_` to avoid name clashes)
* `sumf()` renamed to `sum_()` and `prodf()` to `prod_()` (postfix `_` to avoid name clashes)
* `intf()` renamed to `int()` and `limf()` to `lim()` (because there are no name clashes with base R)
* Call SymPy functions directy with `sympy_func()`
* Added `taylor()` and `drop_remainder()`
* Minor bugs fixed

# caracas 1.0.1

* Require Python 3

# caracas 1.0.0

* An entire new interface for using SymPy, including symbols, symbolic 
  matrices, solving equations, limits and lots of other functionality.

# caracas 0.0.1

* Initial release
