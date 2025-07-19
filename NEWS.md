# caracas 2.1.2

* `kronecker(X, Y)` is much faster when X is diagonal matrix
* `as_piecewise()` added. 
* Now depends on matrix package.
* Now `==` is handled for caracas symbols. Not sure if perfect, though.
* `solve_lower_triangular()` and `solve_upper_triangular()` added.
* `unique()` method added.
*  Bug fixed in `rowspace()`
* `tex_list()` added
* `toeplitz_()` added.

# caracas 2.1.1

* `print(..., method = "compactascii")`: column vector as transposed row vector
   and handling `caracas_scaled_matrix`
* `LUdecomposition()`, `chol()` (Cholesky), `svd_()` (Singular Value decomposition) added.
* `cumsum()` added.
* `as.expression()` (alias for `as_expr()`) and `as.function()` (alias for `as_func()`) cf. #75

# caracas 2.1.0

* Added `knitr` engine `rtex` (see vignette "Chunk engine for easy tex output in Rmarkdown and Quarto")
* New functions: `as_expr.caracas_solve_sys_sol`
* `inv()` by default uses `gauss` like SymPy
* `kronecker()`, `rep()` added
* `solve()` now dispatches on caracas_symbols and works like `solve_lin()`
* `scale_matrix()` can construct matrix-scalar products for prettier output
* `print(..., method = "compactascii")` as `prettyascii` except more compact matrices by removing 
  empty rows
* More compact `solve_sys()` result printing when there is only one unknown

# caracas 2.0.1

* New functions, e.g.: `as_vec()`, `trace_()`
* Initialisation of SymPy/Python is now done at package at start up. This may move 
  the waiting time from first time using the SymPy backend to loading the package. 
  This was done to satisfy CRAN checks.
* Minor bug fixes

# caracas 2.0.0

* Reworked `subs()` - see documentation. `subs_lst()` removed.
* Reworked output format (`utf8`, `prettyascii` or `ascii`) to one argument and one option, i.e. 
  `options(caracas.print.method = 'prettyascii')` and `print(x, method = "prettyascii")` (default is `utf8`)
* New function: `as_func()`
* `dim()<-` assignment for `caracas` matrices

# caracas 1.1.3

* Imports the Matrix package instead of only suggesting
* New functions: `vector_sym()`, `matrix_sym()`, `matrix_sym_diag()`, `matrix_sym_symmetric()`
* New function: `colspan()` (Column space (range) of a symbolic matrix)
* New function: `get_basis()` Get basis for symbolic matrix / vector.
* New function: `all_vars` Get all symbols in caracas symbol.
* `matrify()` now works on atomic elements.
* New function: `def_sym_vec()` defines symbol for each element in a character vector.
* New function: `jacobian()` function added. 
* New functions: `zeros()`, `ones()`, `eye()` added.
* New function: `diff_mat()` added.
* New functions: `crossprod_()`, `tcrossprod_()` added.

# caracas 1.1.2

* `sympy_func(x, fun)` first tries calling `fun` on `x`; and if it does not exist it tries from the global namespace
* New function: `mat_pow()` for raising a matrix to a power (not component-wise), requires SymPy >= 1.6
* New function: `expand_func()` added
* Added `rev()` for `caracas_symbol`'s
* Bug with `Ops` (functions) fixed
* SymPy 1.9 bug with elementwise matrix multiplication (https://github.com/sympy/sympy/issues/22353) addressed
* Enabling pretty ASCII print option (`options(caracas.print.prettyascii = TRUE)`) instead of UTF-8, if the system locale is not UTF-8; this be disabled with `options(caracas.print.prettyascii = FALSE)`

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
