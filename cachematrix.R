## This pair of functions allows caching the inverse of a matrix,
## so one does not need to recalculate it every time it's needed.


## This function creates a list of functions, to get/set the matrix,
## and get/set the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(theInv) inv <<- theInv
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function returns the inverse of the supplied matrix x. 
## If possible, it uses the cached inverse, otherwise the inverse
## is calculated, stored for future use, and finally returned.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
