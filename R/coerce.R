#' @importFrom methods setOldClass
setOldClass("caracas_symbol")

## ## ### From ->->-> To  ###

#' @importFrom methods setAs
setAs("caracas_symbol", "character",    function(from) as.character(from))
setAs("caracas_symbol", "matrix",       function(from) matrify(from))

## FIXME: vectorfy
