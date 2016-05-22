##Below functions are realizing the idea of a Matrix with cached inversion. 
##Once calculated the inversion of our matrix, next time the inversion is needed - it will be taken from cache.



## This function creates a CacheMatrix based on a standard matrix given.
makeCacheMatrix <- function(x = matrix()) {
    #variable for cached inversion
    inv <- NULL
    
    set <- function(y){
        #clear cache if the matrix is changing
        if( !identical(x,y)) inv <<- NULL
        x <<- y
    }
    get <- function() x
    set_inv <- function(inversion) inv <<- inversion
    get_inv <- function() inv

    list(set = set, 
         get = get,
         set_inv = set_inv,
         get_inv = get_inv
         )
}


## Function for cached matrix inversion. 
#Returns cached value if previously calculated (and base matrix hasn't changed)
cacheSolve <- function(x, ...) {
    inv <- x$get_inv()
    if(!is.null(inv)) {
        return(inv)
    }
    inv <- solve(a=x$get())
    x$set_inv(inv)
    return(inv)
}




