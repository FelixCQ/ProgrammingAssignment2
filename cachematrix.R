## This file defines a cachematrix object, encapsulates a matrix and caches
## its inverse.

## This create a new cachematrix object.
## The object is implemented as a list of functions.

makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(newmat) {
                mat <<- newmat
                inv <<- NULL
        }
        get <- function() mat
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes a cachematrix as first argument, and acts like the
## usual solve() function, except the solve operation is only performed once.

cacheSolve <- function(cachemat, ...) {
        inv <- cachemat$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        actualmat <- cachemat$get()
        inv <- solve(actualmat, ...)
        cachemat$setinverse(inv)
        inv
}
