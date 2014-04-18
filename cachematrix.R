## Put comments here that give an overall description of what your
## functions do

## The function takes a matrix x and returns a "special matrix" that
## contains a cache for the inverse of that matrix. Each time an new
## matrix is assigned the cache gets reset.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Computes the inverse of a matrix and caches the result. If the inverse
## has already been computed than the function returns the value of the cache
## and does not recompute the inverse.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
