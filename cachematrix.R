# The first function makeCacheMatrix defines 
# a special matrix object which caches the inverse
# of the matrix passed, and defines four
# methods to manipulate it: set, get, setinverse 
# and getinverse

# Defines a list containing pointer to the four
# function methods
makeCacheMatrix <- function(x = matrix()) {
    invX <- NULL
    set <- function(y) {
        x <<- y
        invX <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) invX <<- inv
    getinverse <- function() invX
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}


# First checks to se if the inverse of the 
# matrix passes is cached, if so it gets it
# from the cache and returns it. If the inverse
# is not alr3eady stored it computes it using the
# solve function, stores it into the cache and
# returns the computed inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invX <- x$getinverse()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    mat <- x$get()
    invX <- solve(mat, ...)
    x$setinverse(invX)
    invX    
}
