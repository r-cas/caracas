##' @title Create a vector along
##' @param x a caracas object 
##' @param entry value 
##' @return a caracas vector
##' @author Søren Højsgaard
vec_along <- function(x, entry="v") { 
    if (length_(x) == 1){
        if (is_sym(x)){
            return(matrix_(entry))            
        } else {
            if (is.numeric(x) && (x > 0)){
                if (x == 1) {
                    return(matrix_(entry))
                } else {
                    return(vector_sym(x, entry))                    
                }
            }
            else {
                stop("can not create symbol\n")
            }
        }
    } else {
        vector_sym(prod(dim(x)), entry)
    }
}




##' @title Length of (caracas) object
##' @param x an object 
##' @return length
##' @author Søren Højsgaard
##' @export
length_ <- function(x) {
    if (is_sym(x)) {
        switch(sym_class(x),
               "atomic"={1},
               "matrix"={prod(dim(x))},
               "vector"={length(as_character_matrix(x))}
               )
    } else {
        switch(class(x),
               "numeric"=,"integer"=,"character"={length(x)},
               "list"  ={length(x)})        
    }
}

           



