## The makeCacheMatrix and cacheSolve functions allow you to 
## create a special matrix that can then be used to calculate and cache
## the inverse of that matrix. The benefit of this, is that you
## can then use that inverse from the cache without recalculating it
## each time you need it.

## makeCacheMatrix will :
##  - get and set the value of the matrix
##  - get and set the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve will first determine if the inverse is already cached
## if it is cached, it will return the cached value
## if it is not cached, it will calculate the inverse, cache it, and return it
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
