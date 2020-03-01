library(testthat)

if (reticulate::py_module_available("sympy")) {
  local_sympy <- reticulate::import("sympy")
  
  if (base::numeric_version(local_sympy$`__version__`) >= "1.4") {
    # All okay:
    library(caracas)
    
    test_check("caracas")
  }
}
