setOldClass("caracas_symbol")

## ## ### From ->->-> To  ###
setAs("caracas_symbol", "character",    function(from) as.character(from))
setAs("caracas_symbol", "matrix",       function(from) matrify(from))

## setAs("graphNEL", "matrix",    function(from) g_gn2dm_(from))
## setAs("graphNEL", "dgCMatrix", function(from) g_gn2sm_(from))
## setAs("graphNEL", "Matrix",    function(from) g_gn2sm_(from))

## setAs("igraph",   "graphNEL",  function(from) g_ig2gn_(from))
## setAs("igraph",   "matrix",    function(from) g_ig2dm_(from))
## setAs("igraph",   "dgCMatrix", function(from) g_ig2sm_(from))

## setAs("matrix",    "igraph",   function(from) g_dm2ig_(from))
## setAs("dgCMatrix", "igraph",   function(from) g_sm2ig_(from))
