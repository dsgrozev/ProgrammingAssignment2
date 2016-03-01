## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    cached <- NULL
    set <- function (y)
    {
        x <<- y
        cached <<- NULL
    }
    get <- function() x
    setSolved <- function(solve) cached <<- solve
    getSolved <- function() cached
    list(set = set, get = get, setSolved = setSolved, 
         getSolved = getSolved)
    
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve retrieves the inverse from the 
## cache.

cacheSolve <- function(x, ...) {
    cached <- x$getSolved()
    if(!is.null(cached))
    {
        message("getting cached data")
        return (cached)
    }
    data <- x$get()
    cached <- solve(data, ...)
    x$setSolved(cached)
    cached
}
