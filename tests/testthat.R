library(testthat)

py_ver <- tryCatch({
  cfg <- reticulate::py_config()
  base::numeric_version(cfg$version)
}, error = function(e) {
  base::numeric_version("1")
})

if (py_ver >= "3" && reticulate::py_module_available("sympy")) {
  local_sympy <- reticulate::import("sympy")
  
  if (base::numeric_version(local_sympy$`__version__`) >= "1.4") {
    # All okay:
    library(caracas)

    test_check("caracas")
  }
}
