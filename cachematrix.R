## Overall Description
## This code will write a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(mx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}


## This function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function listed above.  If the inverse has already
## been calculated (and the matrix has not changed), then this function should 
## retrieve the inversae from the cache.

cacheSolve <- function(mx, ...) {
    inverse <- mx$getinv()
    if(!is.null(inverse)) {
        message("Getting cached data...")
        return(inverse)
    }
    data <- mx$get()
    invserse <- solve(data, ...)
    mx$setinv(inverse)
    return(inverse)
}
