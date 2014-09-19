## The two functions below return the inverse of a matrix
##
## Example function call:  cacheSolve(makeCacheMatrix(m)), where m is an input matrix
##
## The input matrix m is assumed to be square and invertible.
##
## The functions perform the matrix inversion calcuation only if the calculation has not been done previously.
## If the calculation has been done previously, then the cached result is retried and returned.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" object returned by makeCacheMatrix.
## If the inverse has already been calculated, then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
