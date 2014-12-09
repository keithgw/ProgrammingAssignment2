## Computing the inverse of a matrix can be costly, so this pair of functions
## will cache the inverse of a matrix, so that it does not need to repeatedly
## calculate, if it remains unchanged.

## This f(x) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL                # set inverse to NULL when obj is created
    set <- function(y) {           # y is a new matrix to replace the original
        x <<- y                    # reassign the value of object
        inverse <<- NULL           # set inverse to NULL when obj is created
    }
    get <- function() x                         # return matrix
    setinverse <- function(inv) inverse <<- inv # cache inverse, once calculated
    getinverse <- function() inverse            # return inverse
    list(set = set, get = get,                  
         setinverse = setinverse,               # allows other objects to
         getinverse = getinverse)               # access object methods
}


## This f(x) computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been solved, and the matrix has not changed,
## then this function will retrieve the cached inverse.

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    mtrx <- x$get()
    inverse <- solve(mtrx)
    x$setinverse(inverse)
    inverse
}
