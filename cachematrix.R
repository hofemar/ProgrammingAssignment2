## Put comments here that give an overall description of what your
## functions do

## The function takes a matrix x and returns a "special matrix" that
## contains a cache for the inverse of that matrix. Each time an new
## matrix is assigned the cache gets reset.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Set function for matrix (assigns new matrix)
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Get function for matrix (retrieves current matrix)
    get <- function() x
    
    ## Set and get functions for cache
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    ## return list of functions
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
    
    ## Check if cached results already exist
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    
    ## Compute inverse
    inv <- solve(data, ...)
    
    ## Write inverse to cache
    x$setinv(inv)
    inv
}
